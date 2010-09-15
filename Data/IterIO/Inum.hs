{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

-- | This module contains two different functions for constructiing
-- 'Inum's.  The first, 'mkInum', creates simple, stateless 'Inum's
-- out of 'Iter's that translate from an input type to an output type.
-- As an example, here is a simple HTTP chunk encoder.  It translates
-- input into a series of HTTP chunks preceded by their length in hex,
-- with a 0-length chunk signifiying an end-of-file:
--
-- > inumToChunks :: (Monad m) => Inum L.ByteString L.ByteString m a
-- > inumToChunks = mkInum $ do
-- >         Chunk s eof <- chunkI
-- >         let len       = L8.length s
-- >             chunksize = L8.pack $ printf "%x\r\n" len
-- >             trailer   = if eof && len > 0
-- >                         then L8.pack "\r\n0\r\n\r\n"
-- >                         else L8.pack "\r\n"
-- >         return $ L8.concat [chunksize, s, trailer]
--
-- For more complex 'Inum's that need state and different control
-- flow, there is the 'mkInumM' function.  'mkInumM' creates an 'Inum'
-- out of a computation in the 'InumM' monad.  'InumM' is a
-- 'MonadTrans' around 'Iter'.  Thus you can consume input by applying
-- 'lift' to any 'Iter' of the input type and monad.

module Data.IterIO.Inum
    (-- * Simple Enumerator construction function
     mkInum
    -- * IterStateT monad
    , IterStateT(..), runIterStateT, catchIterState, tryIterState
    -- * Enumerator construction monad
    , InumM, mkInumM, mkInumAutoM
    , setCtlHandler, setAutoEOF, setAutoDone, addCleanup, withCleanup
    , ifeed, ifeed1, ipipe, irun, irepeat
    ) where

import Prelude hiding (null)
import Control.Exception (ErrorCall(..), Exception(..))
import Control.Monad
import Control.Monad.State.Class
import Control.Monad.Trans
import Data.Monoid
import Data.Typeable

import Data.IterIO.Base

--
-- Simple enumerator construction function
--

-- | Construct a simple @'Inum' tIn tOut m a@ from a transcoder @'Iter
-- tIn m tOut@.  The returned 'Inum' simply invokes the transcoder
-- over and over again to get more data.  It finishes when the @'Iter'
-- tOut m a@ being feed by the 'Inum' is no longer active or the
-- trancoder @'Iter' tIn m tOut@ throws an exception of class
-- 'IterEOF'.  If the transcoder throws any other kind of exception,
-- the whole 'Inum' fails with an 'InumFail' error.
--
-- The 'Inum's created by this function are simple and stateless.  For
-- a more powerful interface, see 'mkInumM' and the 'InumM' monad.
mkInum :: (Monad m, ChunkData tIn, ChunkData tOut) =>
           Iter tIn m tOut
        -- ^ This Iteratee will be executed repeatedly to produce
        -- transcoded chunks.
        -> Inum tIn tOut m a
mkInum fn = loop
    where
      loop = inumMC passCtl `cat` process
      process iter@(IterF _) =
          catchOrI fn (\(IterEOF _) -> return iter) $ loop . feedI iter . chunk
      process iter = return iter


--
-- IterStateT monad
--

-- | @IterStateT@ is a lot like the @'StateT'@ monad transformer, but
-- specific to transforming the 'Iter' monad (i.e., type argument @m@
-- should be @'Iter' t m@ for some 'ChunkData' t and 'Monad' m).  What
-- @IterStateT@ does unlike @'StateT'@ is to convert all 'IterFail'
-- failures into 'InumFail' failures, and use the 'InumFail' to
-- propagate the state.  Thus, it is possible to extract the state
-- from an 'IterStateT' even after a failure of the underlying 'Iter'
-- monad.
newtype IterStateT s m a = IterStateT (s -> m (a, s))

-- | Runs an 'IterStateT' monad on some input state.  Because of its
-- special error handling feature, the 'IterStateT' 'Monad' can only be
-- run when the transformed monad is an @'Iter' t m@.
--
-- This function could equally well have returned an @'Iter' t m (s,
-- a)@.  However, it returns @'InumR' t t m a@ (equivalent to @'Iter'
-- t m ('Iter' t m a)@) to make it more convenient to inspect the
-- result and check for errors.  This is the same trick used by
-- 'finishI', which @runIterStateT@ uses internally; returning 'InumR'
-- saves other code from needing to invoke 'finishI' a second time.
runIterStateT :: (Monad m, ChunkData t) =>
                 IterStateT s (Iter t m) a -> s -> InumR t t m (a, s)
runIterStateT (IterStateT isf) s = finishI (isf s) >>= return . check
    where check (IterFail e) = InumFail e (error "runIterStateT", s)
          check iter         = iter

-- | This class is kind of silly and wouldn't be required with
-- -XFlexibleInstances, but is used so that the order and nature of
-- the type arguments to IterStateT is appropriate for a 'MonadTrans'.
class (Monad m) => IterStateTClass m where
    isc_return :: a -> IterStateT s m a
    isc_bind   :: IterStateT s m a -> (a -> IterStateT s m b)
               -> IterStateT s m b
    isc_fail   :: String -> IterStateT s m a

-- | Ignore the @IterStateTClass@ class, which has only one instance
-- and is just used for a technicality to get the type parameter order
-- to work out.
instance (Monad m, ChunkData t) => IterStateTClass (Iter t m) where
    isc_return a = IterStateT $ \s -> return (a, s)

    isc_bind m k = IterStateT $ \s -> (runIterStateT m s) >>= check
        where check (InumFail e (_, s')) = InumFail e (error "isc_bind", s')
              check msa = msa >>= \(a, s') -> join $ runIterStateT (k a) s'

    isc_fail msg = IterStateT $ \s -> InumFail (toException $ ErrorCall msg)
                                               (error "isc_fail", s)

instance (IterStateTClass m) => Monad (IterStateT s m) where
    return = isc_return
    (>>=)  = isc_bind
    fail   = isc_fail

instance MonadTrans (IterStateT s) where
    lift m = IterStateT $ \s -> m >>= \a -> return (a, s)

instance (MonadIO m, IterStateTClass m) => MonadIO (IterStateT s m) where
    liftIO = lift . liftIO

-- | Catch an exception thrown in an @'IterStateT' s (Iter t m)@
-- monad.
catchIterState :: (ChunkData t, Monad m, Exception e) => 
                  IterStateT s (Iter t m) a
               -> (e -> IterStateT s (Iter t m) a)
               -> IterStateT s (Iter t m) a
catchIterState m handler = tryIterState m >>= either handler return

-- | Try an 'IterStateT' action, returning @'Left' e@ if it throws an
-- exception @e@.
tryIterState :: (ChunkData t, Monad m, Exception e) => 
                IterStateT s (Iter t m) a
             -> IterStateT s (Iter t m) (Either e a)
tryIterState m = IterStateT $ \s -> runIterStateT m s >>= check
    where check (InumFail e (a, s)) =
              case fromException e of
                Just e' -> return (Left e', s)
                Nothing -> InumFail e (Right a, s)
          check iter = iter >>= \(a, s) -> return (Right a, s)

-- | This should be the only declaration that requires
-- FlexibleInstances, and only because mtl makes that necessary.
instance (IterStateTClass m) => MonadState s (IterStateT s m) where
    get   = IterStateT $ \s -> return (s, s)
    put s = IterStateT $ \_ -> return ((), s)


--
-- InumM monad
--

data InumState tIn tOut m a = InumState {
      insAutoEOF :: !Bool
    , insAutoDone :: !Bool
    , insCtl :: !(CtlHandler (Iter tIn m))
    , insIter :: !(Iter tOut m a)
    , insRemain :: !(Chunk tIn)
    , insCleanup :: !(InumM tIn tOut m a ())
    , insCleaning :: !Bool
    }

defaultInumState :: (ChunkData tIn, Monad m) => InumState tIn tOut m a
defaultInumState = InumState {
                     insAutoEOF = False
                   , insAutoDone = False
                   , insCtl = passCtl
                   , insIter = IterF $ const $ error "insIter"
                   , insRemain = mempty
                   , insCleanup = return ()
                   , insCleaning = False
                   }

-- | A monad in which to define the actions of an @'Inum' tIn tOut m a@.
type InumM tIn tOut m a = IterStateT (InumState tIn tOut m a) (Iter tIn m)

data InumDone = InumDone deriving (Show, Typeable)
instance Exception InumDone

-- | Set the control handler an 'Inum' should use from within an
-- 'InumM' computation.  (The default is 'passCtl'.)
setCtlHandler :: (ChunkData tIn, Monad m) =>
                  CtlHandler (Iter tIn m) -> InumM tIn tOut m a ()
setCtlHandler ch = modify $ \s -> s { insCtl = ch }

-- | Set the \"Auto EOF\" flag within an 'InumM' computation.  If this
-- flag is 'True', handle 'IterEOF' exceptions like a normal but
-- immediate termination of the 'Inum'.  If this flag is @'False'@
-- (the default), then 'IterEOF' exceptions must be manually caught or
-- they will terminate the thread.
setAutoEOF :: (ChunkData tIn, Monad m) => Bool -> InumM tIn tOut m a ()
setAutoEOF val = modify $ \s -> s { insAutoEOF = val }

-- | Set the \"Auto Done\" flag within an 'InumM' computation.  When
-- @'True'@, the 'Inum' will immediately terminate as soon as the
-- 'Iter' it is feeding enters a non-active state (i.e., 'Done' or a
-- failure state).  If this flag is @'False'@ (the default), the
-- 'InumM' computation will need to monitor the results of the
-- 'ifeed', 'ipipe', and 'irun' functions to ensure the 'Inum'
-- terminates when one of these functions returns @'False'@.
setAutoDone :: (ChunkData tIn, Monad m) => Bool -> InumM tIn tOut m a ()
setAutoDone val = modify $ \s -> s { insAutoDone = val }

-- | Like modify, but throws an error if the insCleaning flag is set.
ncmodify :: (ChunkData tIn, Monad m) =>
            (InumState tIn tOut m a -> InumState tIn tOut m a)
            -> InumM tIn tOut m a ()
ncmodify fn = IterStateT $ \s ->
              if insCleaning s
              then error "illegal call within Cleaning function"
              else return ((), fn s)

-- | Add a cleanup action to be executed when the 'Inum' finishes.
addCleanup :: (ChunkData tIn, Monad m) =>
              InumM tIn tOut m a () -> InumM tIn tOut m a ()
addCleanup clean = ncmodify $ \s -> s { insCleanup = clean >> insCleanup s }

-- | Run an 'InumM' action with some cleanup function in effect.
-- Resets the cleanup function after executing the action (which also
-- undoes the effects of any 'addCleanup' calls within the action).
withCleanup :: (ChunkData tIn, Monad m) =>
              InumM tIn tOut m a ()
           -> InumM tIn tOut m a b
           -> InumM tIn tOut m a b
withCleanup clean action = do
  old <- gets insCleanup
  ncmodify $ \s -> s { insCleanup = clean >> old }
  b <- action
  modify $ \s -> s { insCleanup = old }
  return b

-- | A variant of 'mkInumM' that sets the flags controlled by
-- 'setAutoEOF' and 'setAutoDone' to @'True'@.
mkInumAutoM :: (ChunkData tIn, ChunkData tOut, Monad m) =>
               InumM tIn tOut m a b -> Inum tIn tOut m a
mkInumAutoM inumm iter0 =
    runInumM inumm defaultInumState { insIter = iter0
                                    , insAutoEOF = True
                                    , insAutoDone = True
                                    }

-- | Build an 'Inum' out of an 'InumM' computation.  The 'InumM'
-- computation can use 'lift' to execute 'Iter' monads and process
-- input of type 'tIn'.  It must then feed output of type 'tOut' to an
-- output 'Iter' (which is implicitly contained in the monad state),
-- using the 'ifeed', 'ipipe', and 'irun' functions.
mkInumM :: (ChunkData tIn, ChunkData tOut, Monad m) =>
           InumM tIn tOut m a b -> Inum tIn tOut m a
mkInumM inumm iter0 =
    runInumM inumm defaultInumState { insIter = iter0 }

-- | Convert an 'InumM' computation into an 'Inum', given some
-- 'InumState' to run on.
runInumM :: (ChunkData tIn, ChunkData tOut, Monad m) =>
            InumM tIn tOut m a b
         -- ^ Monadic computation defining the 'Inum'.
         -> InumState tIn tOut m a
         -- ^ State to run on
         -> InumR tIn tOut m a
runInumM inumm state0 = do
  result1 <- runIterStateT inumm state0 >>= convertFail
  let state1 = (getState result1) { insAutoDone = False, insCleaning = True }
  result2 <- runIterStateT (insCleanup state1) state1 >>= convertFail
  let iter = insIter $ getState result2
  case (result1, result2) of
    (InumFail e _, _) -> InumFail e iter
    (_, InumFail e _) -> InumFail e iter
    _                 -> return iter
    where
      getState (Done (_, s) _)     = s
      getState (InumFail _ (_, s)) = s
      getState _                   = error "runInumM getState"

      tryErr e fnjust nothing = maybe nothing fnjust $ fromException e
      convertDone s InumDone = inumF $
          Done (error "convertDone", s {insRemain = mempty}) (insRemain s)
      convertEOF s (IterEOF _) = return $ return (error "convertEOF", s)
      convertFail iter@(InumFail e (_, s)) =
          tryErr e (convertDone s) $
          (if insAutoEOF s then tryErr e (convertEOF s) else id) $
          return iter
      convertFail iter = inumF iter

{-
ithrow :: (Exception e) => e -> InumM tIn tOut m a b
ithrow e = IterStateT $ \s -> IterF $ \c ->
           InumFail (toException e) (s { insRemain = c }, error "ithrow")
-}

-- | Used from within the 'InumM' monad to feed data to the 'Iter'
-- being processed by that 'Inum'.  Returns @'False'@ if the 'Iter' is
-- still active and @'True'@ if the iter has finished and the 'Inum'
-- should also return.  (If the @autoDone@ flag is @'True'@, then
-- @ifeed@, @ipipe@, and @irun@ will never actually return @'True'@,
-- but instead just exit the 'Inum' immediately.)
ifeed :: (ChunkData tIn, ChunkData tOut, Monad m) =>
         tOut -> InumM tIn tOut m a Bool
ifeed = ipipe . inumPure

-- | A variant of 'ifeed' that throws an exception of type 'IterEOF'
-- if the data being fed is 'null'.
ifeed1 :: (ChunkData tIn, ChunkData tOut, Monad m) =>
          tOut -> InumM tIn tOut m a Bool
ifeed1 dat = if null dat then lift $ throwEOFI "ifeed: null" else ifeed dat

-- | Apply another 'Inum' to the target 'Iter' from within the 'InumM'
-- monad.  As with 'ifeed', returns @'True'@ when the 'Iter' is
-- finished.
ipipe :: (ChunkData tIn, ChunkData tOut, Monad m) =>
         Inum tIn tOut m a -> InumM tIn tOut m a Bool
ipipe inum = IterStateT $ \s -> do
  iter <- inumRepeat (inumMC (insCtl s) `cat` inumF) |. inum $ insIter s
  let done = not $ isIterActive iter
  if done && insAutoDone s
    then do c <- IterF $ \c -> return c
            InumFail (toException InumDone)
                     (True, s {insIter = iter, insRemain = c })
    else return (done, s {insIter = iter })

-- | Apply an 'Onum' (or 'Inum' of an arbitrary, unused input type) to
-- the 'Iter' from within the 'InumM' monad.  As with 'ifeed', returns
-- @'True'@ when the 'Iter' is finished.
irun :: (ChunkData tAny, ChunkData tIn, ChunkData tOut, Monad m) =>
        Inum tAny tOut m a -> InumM tIn tOut m a Bool
irun onum = ipipe $ runI . onum

-- | Repeats an action until the 'Iter' is done or an EOF error is
-- thrown.  (Also stops if a different kind of exception is thrown, in
-- which case the exception is further propagates.)  @irepeat@ sets
-- 'setAutoEOF' and 'setAutoDone' to @'True'@.
irepeat :: (ChunkData tIn, Monad m) =>
           InumM tIn tOut m a b -> InumM tIn tOut m a ()
irepeat action = do
  modify $ \s -> s { insAutoEOF = True, insAutoDone = True }
  let loop = action >> loop in loop
