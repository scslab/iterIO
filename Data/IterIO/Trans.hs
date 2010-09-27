{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- These extensions are only for MTL stuff where it is required
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}


-- | Various helper functions and instances to make 'Iter's work with
-- 'MonadTrans' instances.
module Data.IterIO.Trans where

import Control.Monad.Cont
import Control.Monad.Error
import Control.Monad.List
import Control.Monad.Reader
import Control.Monad.RWS.Strict
import Control.Monad.State.Strict
import Control.Monad.Writer.Strict

import Data.IterIO.Base

-- | Adapt an 'Iter' from one monad to another.  Requires two
-- functions, one adapting the result to a new type (if required), and
-- a second adapting monadic computations from one monad to the other.
-- For example, 'liftI' is implemented as:
--
-- > liftI = adaptIter id $ lift . lift >=> liftI
--
-- Note that in general the coputation adapter must invoke itself
-- recursively.  @adaptIter@ is designed this way because the result
-- adapter function may need to change.  An example is 'runListTI',
-- implemented as follows:
--
-- > runStateTI s = adaptIter (flip (,) s) $ \m ->
-- >                IterM $ runStateT m s >>= \(i, s') ->
-- >                return $ runStateTI s' i
--
-- Here, after executing 'runStateT', the state may be modified.
-- Thus, 'runStateTI' invokes itself recursively with the modified
-- state, @s'@, to ensure that the result will be paired with the
-- appropriate state.
adaptIter :: (ChunkData t, Monad m1, Monad m2) =>
             (a -> b)           -- ^ How to adapt return value
          -> (m1 (Iter t m1 a) -> Iter t m2 b) -- ^ How to adapt computations
          -> Iter t m1 a                       -- ^ Input computation
          -> Iter t m2 b                       -- ^ Output computation
adaptIter f mf = adapt
    where
      adapt iter@(IterF _) = IterF $ adapt . feedI iter
      adapt (IterM m)      = mf m
      adapt (Done a c)     = Done (f a) c
      adapt (IterC a fr)   = IterC a $ adapt . fr
      adapt (IterFail e)   = IterFail e
      adapt (InumFail e a) = InumFail e (f a)


-- | Run an @'Iter' s m@ computation from witin the @'Iter' s (t m)@
-- monad, where @t@ is a 'MonadTrans'.
liftI :: (MonadTrans t, Monad m, Monad (t m), ChunkData s) =>
         Iter s m a -> Iter s (t m) a
liftI = adaptIter id $ lift . lift >=> liftI

-- | Turn a computation of type @'Iter' t ('ContT' ('Iter' t m a) m)
-- a@ into one of type @'Iter' t m a@.  Note the return value of the
-- continuation is of type @'Iter' t m a@, not @a@, so that you can
-- return residual data.
runContTI :: (Monad m, ChunkData t) =>
             Iter t (ContT (Iter t m a) m) a -> Iter t m a
runContTI = adaptIter id $ \m -> IterM $ runContT m (return . runContTI)

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
runReaderTI m0 r = adaptIter id adapt m0
    where adapt m = lift (runReaderT m r) >>= flip runReaderTI r

-- | Run an @'Iter' t ('RWST' r w s m)@ computation from within the
-- @'Iter' t m@ monad.
runRWSI :: (ChunkData t, Monoid w, Monad m) =>
           Iter t (RWST r w s m) a -- ^ Computation to transform
        -> r                       -- ^ Reader State
        -> s                       -- ^ Mutable State
        -> Iter t m (a, s, w)      -- ^ Returns result, mutable state, writer
runRWSI iter0 r s0 = doRWS mempty s0 iter0
    where doRWS w s = adaptIter (\a -> (a, s, w)) $ \m ->
                      IterM $ runRWST m r s >>= \(iter, s', w') ->
                      return $ doRWS (mappend w w') s' iter

-- | Run an @'Iter' t ('StateT' m)@ computation from within the
-- @'Iter' t m@ monad.
runStateTI :: (ChunkData t, Monad m) =>
              Iter t (StateT s m) a -> s -> Iter t m (a, s)
runStateTI iter0 s0 = adaptIter (flip (,) s0) adapt iter0
    where adapt m = IterM $ runStateT m s0 >>= \(iter, s) ->
                    return $ runStateTI iter s

-- | Run an @'Iter' t ('WriterT' w m)@ computation from within the
-- @'Iter' t m@ monad.
runWriterTI :: (ChunkData t, Monoid w, Monad m) =>
               Iter t (WriterT w m) a -> Iter t m (a, w)
runWriterTI = doW mempty
    where doW w = adaptIter (\a -> (a, w)) $
                  lift . runWriterT >=> \(iter, w') -> doW (mappend w w') iter

--
-- Below this line, we use FlexibleInstances and UndecidableInstances
--


instance (Error e, MonadError e m, ChunkData t) => MonadError e (Iter t m) where
    throwError = IterM . throwError

    catchError (IterM m) h = IterM $ do
        r <- (liftM Right m) `catchError` (return . Left . h)
        case r of
          Right iter -> return $ catchError iter h
          Left iter  -> return iter
    catchError iter h
        | isIterActive iter = inumFC passCtl iter >>= flip catchError h
        | otherwise         = iter
