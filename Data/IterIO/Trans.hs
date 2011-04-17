{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

-- These extensions are only for MTL stuff where it is required
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE ScopedTypeVariables #-}

-- | Various helper functions and instances for using 'Iter's of
-- different Monads together in the same pipeline.
module Data.IterIO.Trans {-
                         (-- * Iter-specific state monad transformer
                          IterStateT(..), runIterStateT
                         , iget, igets, iput, imodify
                          -- * Functions for building Iter monad adapters
                         , adaptIter, adaptIterM
                         -- * Adapters for Iters of mtl transformers
                         , liftIterM, liftIterIO
                         , runContTI, runErrorTI, runListTI, runReaderTI
                         , runRWSI, runRWSLI, runStateTI, runStateTLI
                         , runWriterTI, runWriterTLI
                         )
-}
    where

import Control.Applicative ((<$>))
import Control.Monad.Cont
import Control.Monad.Error
import Control.Monad.List
import Control.Monad.Reader
import Control.Monad.RWS.Strict
import Control.Monad.State.Strict
import Control.Monad.Writer.Strict
import qualified Control.Monad.RWS.Lazy as Lazy
import qualified Control.Monad.State.Lazy as Lazy
import qualified Control.Monad.Writer.Lazy as Lazy
import Control.Monad
import Control.Monad.Trans
import Data.Monoid

import Data.IterIO.Iter

--
-- IterStateT monad
--

-- | @IterStateT@ is a variant of the 'StateT' monad transformer
-- specifically designed for use inside 'Iter's.  The difference
-- between 'IterStateT' and 'StateT' is that the 'runIterStateT'
-- function returns an 'Iter' instead of just the result, which is
-- somewhat analogous to the way 'finishI' returns an 'Iter' inside an
-- 'Iter' so you can inspect its state.  The advantage of this
-- approach is that you can recover the state even if an 'IterFail' or
-- 'InumFail' condition occurs.
newtype IterStateT s m a = IterStateT (s -> m (a, s))

instance (Monad m) => Monad (IterStateT s m) where
    return a = IterStateT $ \s -> return (a, s)
    (IterStateT mf) >>= k = IterStateT $ \s -> do (a, s') <- mf s
                                                  let (IterStateT kf) = k a
                                                  kf $! s'
    fail = IterStateT . const . fail

instance MonadTrans (IterStateT s) where
    lift m = IterStateT $ \s -> m >>= \a -> return (a, s)

instance (MonadIO m) => MonadIO (IterStateT s m) where
    liftIO = lift . liftIO

-- | Runs an @'IterStateT' s m@ computation on some state @s@.
-- Returns the result ('IterR') of the 'Iter' and the state of @s@ as
-- a pair.  Pulls residual input up to the enclosing 'Iter' monad as
-- with 'pullupResid'.
runIterStateT :: (ChunkData t, Monad m) => 
                 Iter t (IterStateT s m) a -> s -> Iter t m (IterR t m a, s)
runIterStateT i0 s0 = Iter $ adapt s0 . runIter i0
    where adapt s (IterM (IterStateT f)) =
              IterM $ liftM (uncurry $ flip adapt) (f s)
          adapt s r =
              stepR' r (adapt s) $ Done (setResid r mempty, s) (getResid r)
                
-- | Returns the state in an @'Iter' t ('IterStateT' s m)@ monad.
-- Analogous to @'get'@ for a @'StateT' s m@ monad.
iget :: (Monad m) => Iter t (IterStateT s m) s
iget = lift $ IterStateT $ \s -> return (s, s)

-- | Returns a particular field of the 'IterStateT' state, analogous
-- to @'gets'@ for @'StateT'@.
igets :: (Monad m) => (s -> a) -> Iter t (IterStateT s m) a
igets f = liftM f iget

-- | Sets the 'IterStateT' state.  Analogous to @'put'@ for
-- @'StateT'@.
iput :: (Monad m) => s -> Iter t (IterStateT s m) ()
iput s = lift $ IterStateT $ \_ -> return ((), s)

-- | Modifies the 'IterStateT' state.  Analogous to @'modify'@ for
-- @'StateT'@.
imodify :: (Monad m) => (s -> s) -> Iter t (IterStateT s m) ()
imodify f = lift $ IterStateT $ \s -> return ((), f s)

--
-- Adapter utility functions
--

-- | Adapt an 'Iter' from one monad to another.  Requires two
-- functions, one adapting the result to a new type (if required), and
-- a second adapting monadic computations from one monad to the other.
-- For example, 'liftIterM' could be implemented as:
--
-- > liftIterM :: (MonadTrans t, Monad m, Monad (t m), ChunkData s) =>
-- >              Iter s m a -> Iter s (t m) a
-- > liftIterM = adaptIter id $ lift . lift >=> liftIterM
--
-- Here @'lift' . 'lift'@ executes a computation of type
-- @m ('Iter' s m a)@ from within the @'Iter' s (t m)@ monad.
-- The result, of type @'Iter' s m a@, can then be fed back into
-- @liftIterM@ recursively.
--
-- Note that in general the computation adapters must invoke the outer
-- function recursively.  @adaptIter@ is designed this way because the
-- result adapter function may need to change.  An example is
-- 'runStateTI', which could be implemented as follows:
--
-- > runStateTI :: (ChunkData t, Monad m) =>
-- >               Iter t (StateT s m) a -> s -> Iter t m (a, s)
-- > runStateTI iter s = adaptIter adaptResult adaptComputation iter
-- >     where adaptResult a = (a, s)
-- >           adaptComputation m = do (r', s') <- lift (runStateT m s)
-- >                                   runStateTI r' s'
--
-- Here, after executing 'runStateT', the state may be modified.
-- Thus, @adaptComputation@ invokes @runStateTI@ recursively with the
-- modified state, @s'@, to ensure that subsequent 'IterM'
-- computations will be run on the latest state, and that eventually
-- @adaptResult@ will pair the result @a@ with the newest state.
adaptIter :: (ChunkData t, Monad m1) =>
             (a -> b)                          -- ^ How to adapt result values
          -> (m1 (Iter t m1 a) -> Iter t m2 b) -- ^ How to adapt computations
          -> Iter t m1 a                       -- ^ Input computation
          -> Iter t m2 b                       -- ^ Output computation
adaptIter f mf i = Iter $ check . runIter i
    where check (IterM m) = runIter (mf $ liftM (Iter . runIterR) m) mempty
          check r = stepR' r check $ fmapR f r

-- | Adapt monadic computations of an 'Iter' from one monad to
-- another.  This only works when the values are converted straight
-- through.  For more complex scenarios, you need 'adaptIter'.
--
-- As an example, the 'liftIterIO' function is implemented as follows:
--
-- > liftIterIO :: (ChunkData t, MonadIO m) => Iter t IO a -> Iter t m a
-- > liftIterIO = adaptIterM liftIO
adaptIterM :: (ChunkData t, Monad m1, Monad m2) =>
              (m1 (Iter t m1 a) -> m2 (Iter t m1 a)) -- ^ Conversion function
           -> Iter t m1 a       -- ^ 'Iter' of input monad
           -> Iter t m2 a       -- ^ Returns 'Iter' of output monad
adaptIterM f = adapt
    where adapt = adaptIter id $ lift . f >=> adapt

-- | Run an @'Iter' s m@ computation from witin the @'Iter' s (t m)@
-- monad, where @t@ is a 'MonadTrans'.
liftIterM :: (MonadTrans t, Monad m, Monad (t m), ChunkData s) =>
             Iter s m a -> Iter s (t m) a
liftIterM = adaptIterM lift

-- | Run an @'Iter' t IO@ computation from within an @'Iter' t m@
-- monad where @m@ is in class 'MonadIO'.
liftIterIO :: (ChunkData t, MonadIO m) =>
              Iter t IO a -> Iter t m a
liftIterIO = adaptIterM liftIO

--
-- mtl runner functions
--

-- | The type signature says it all.  Just a slightly optimized
-- version of @joinlift = join . lift@.
joinlift :: (Monad m) => m (Iter t m a) -> Iter t m a
joinlift m = Iter $ \c -> IterM $ m >>= \i -> return $ runIter i c

-- | Turn a computation of type @'Iter' t ('ContT' ('Iter' t m a) m)
-- a@ into one of type @'Iter' t m a@.  Note the return value of the
-- continuation is of type @'Iter' t m a@, not @a@, so that you can
-- return residual data.
runContTI :: (ChunkData t, Monad m) =>
             Iter t (ContT (Iter t m a) m) a -> Iter t m a
runContTI = adaptIter id adapt
    where adapt m = joinlift $ runContT m $ return . runContTI
--        adapt :: ContT (Iter t m a) m (Iter t (ContT (Iter t m a) m) a)
--              -> Iter t m a

-- | Run a computation of type @'Iter' t ('ErrorT' e m)@ from within
-- the @'Iter' t m@ monad.  This function is here for completeness,
-- but please consider using 'throwI' instead, since the 'Iter' monad
-- already has built-in exception handling and it's best to have a
-- single, uniform approach to error reporting.
runErrorTI :: (Monad m, ChunkData t, Error e) =>
              Iter t (ErrorT e m) a -> Iter t m (Either e a)
runErrorTI = adaptIter Right $ lift . runErrorT >=> next
    where next (Left e)     = return $ Left e
          next (Right iter) = runErrorTI iter

-- | Run an @'Iter' t ('ListT' m)@ computation from within the @'Iter'
-- t m@ monad.
runListTI :: (Monad m, ChunkData t) =>
             Iter t (ListT m) a -> Iter t m [a]
runListTI = adaptIter (: []) $
            lift . runListT >=> liftM concat . runListTI . sequence

-- | Run an @'Iter' t ('ReaderT' r m)@ computation from within the
-- @'Iter' t m@ monad.
runReaderTI :: (ChunkData t, Monad m) =>
               Iter t (ReaderT r m) a -> r -> Iter t m a
runReaderTI m r = adaptIterM (flip runReaderT r) m

-- | Run an @'Iter' t ('RWST' r w s m)@ computation from within the
-- @'Iter' t m@ monad.
runRWSI :: (ChunkData t, Monoid w, Monad m) =>
           Iter t (RWST r w s m) a -- ^ Computation to transform
        -> r                       -- ^ Reader State
        -> s                       -- ^ Mutable State
        -> Iter t m (a, s, w)      -- ^ Returns result, mutable state, writer
runRWSI iter0 r s0 = doRWS mempty s0 iter0
    where doRWS w s = adaptIter (\a -> (a, s, w)) $ \m -> do
                        (iter, s', w') <- lift $ runRWST m r s
                        doRWS (mappend w w') s' iter
                                                  
-- | Run an @'Iter' t ('Lazy.RWST' r w s m)@ computation from within
-- the @'Iter' t m@ monad.  Just like 'runRWSI', execpt this function
-- is for /Lazy/ 'Lazy.RWST' rather than strict 'RWST'.
runRWSLI :: (ChunkData t, Monoid w, Monad m) =>
           Iter t (Lazy.RWST r w s m) a
         -- ^ Computation to transform
        -> r                       -- ^ Reader State
        -> s                       -- ^ Mutable State
        -> Iter t m (a, s, w)      -- ^ Returns result, mutable state, writer
runRWSLI iter0 r s0 = doRWS mempty s0 iter0
    where doRWS w s = adaptIter (\a -> (a, s, w)) $ \m -> do
                        (iter, s', w') <- lift $ Lazy.runRWST m r s
                        doRWS (mappend w w') s' iter

-- | Run an @'Iter' t ('StateT' m)@ computation from within the
-- @'Iter' t m@ monad.
runStateTI :: (ChunkData t, Monad m) =>
              Iter t (StateT s m) a -> s -> Iter t m (a, s)
runStateTI iter0 s0 = adaptIter (\a -> (a, s0)) adapt iter0
    where adapt m = lift (runStateT m s0) >>= uncurry runStateTI

-- | Run an @'Iter' t ('Lazy.StateT' m)@ computation from within the
-- @'Iter' t m@ monad.  Just like 'runStateTI', except this function
-- works on /Lazy/ 'Lazy.StateT' rather than strict 'StateT'.
runStateTLI :: (ChunkData t, Monad m) =>
              Iter t (Lazy.StateT s m) a -> s -> Iter t m (a, s)
runStateTLI iter0 s0 = adaptIter (\a -> (a, s0)) adapt iter0
    where adapt m = lift (Lazy.runStateT m s0) >>= uncurry runStateTLI

-- | Run an @'Iter' t ('WriterT' w m)@ computation from within the
-- @'Iter' t m@ monad.
runWriterTI :: (ChunkData t, Monoid w, Monad m) =>
               Iter t (WriterT w m) a -> Iter t m (a, w)
runWriterTI = doW mempty
    where doW w = adaptIter (\a -> (a, w)) $
                  lift . runWriterT >=> \(iter, w') -> doW (mappend w w') iter

-- | Run an @'Iter' t ('Lazy.WriterT' w m)@ computation from within
-- the @'Iter' t m@ monad.  This is the same as 'runWriterT' but for
-- the /Lazy/ 'Lazy.WriterT', rather than the strict one.
runWriterTLI :: (ChunkData t, Monoid w, Monad m) =>
                Iter t (Lazy.WriterT w m) a -> Iter t m (a, w)
runWriterTLI = doW mempty
    where doW w = adaptIter (\a -> (a, w)) $
                  lift . Lazy.runWriterT >=> \(iter, w') ->
                  doW (mappend w w') iter

--
-- Below this line, we use FlexibleInstances and UndecidableInstances,
-- but only because this is required by mtl.
--

instance (ChunkData t, MonadCont m) => MonadCont (Iter t m) where
    callCC f = joinlift $ (callCC $ \cc -> return $ f (cont cc))
        where cont cc a = Iter $ \c -> IterM $ cc (reRunIter $ Done a c)

instance (Error e, MonadError e m, ChunkData t) =>
    MonadError e (Iter t m) where
        throwError = lift . throwError
        catchError m0 h = adaptIter id (joinlift . runm) m0
            where runm m = do
                    r <- catchError (liftM Right m) (return . Left . h)
                    case r of
                      Right iter -> return $ catchError iter h
                      Left iter  -> return iter

instance (MonadReader r m, ChunkData t) => MonadReader r (Iter t m) where
    ask = lift ask
    local f = adaptIterM $ local f

instance (MonadState s m, ChunkData t) => MonadState s (Iter t m) where
    get = lift get
    put = lift . put

instance (Monoid w, MonadWriter w m, ChunkData t) =>
    MonadWriter w (Iter t m) where
        tell = lift . tell
        listen = adapt mempty
            where adapt w = adaptIter (\a -> (a, w)) $
                            lift . listen >=> \(iter, w') ->
                            adapt (mappend w w') iter
        pass m = do
          ((a, f), w) <- adapt mempty m
          tell (f w)
          return a
            where
              adapt w = adaptIter (\af -> (af, w)) $
                        lift . censor (const mempty) . listen >=> \(i, w') ->
                        adapt (mappend w w') i

--
-- and instances for IterStateT (which are identical to StateT)
--

unIterStateT :: IterStateT s m a -> (s -> m (a, s))
unIterStateT (IterStateT f) = f

instance (MonadCont m) => MonadCont (IterStateT s m) where
    callCC f = IterStateT $ \s -> callCC $ \c ->
               unIterStateT (f (\a -> IterStateT $ \s' -> c (a, s'))) s

instance (MonadError e m) => MonadError e (IterStateT s m) where
    throwError = lift . throwError
    catchError m h = IterStateT $ \s ->
                     unIterStateT m s `catchError` \e ->
                     unIterStateT (h e) s

instance (MonadReader r m) => MonadReader r (IterStateT s m) where
    ask = lift ask
    local f m = IterStateT $ \s -> local f (unIterStateT m s)

instance (MonadWriter w m) => MonadWriter w (IterStateT s m) where
    tell = lift . tell
    listen m = IterStateT $ \s -> do
                 ((a, s'), w) <- listen (unIterStateT m s)
                 return ((a, w), s')
    pass   m = IterStateT $ \s -> pass $ do
                 ((a, f), s') <- unIterStateT m s
                 return ((a, s'), f)
