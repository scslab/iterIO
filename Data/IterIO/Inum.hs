{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

-- | This module contains various functions used to construct 'Inum's.

module Data.IterIO.Inum
    (-- * Enumerator construction functions
     Codec, CodecR(..)
    , iterToCodec
    , mkInumC, mkInum, mkInum'
    , inumCBracket, inumBracket
    -- * Enumerator construction monad
    , IterStateT(..), runIterStateT, catchIterState, tryIterState
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
-- Enumerator construction functions
--

-- | A @Codec@ is an 'Iter' that tranlates data from some input type
-- @tIn@ to an output type @tOut@ and returns the result in a
-- 'CodecR'.  If the @Codec@ is capable of repeatedly being invoked to
-- translate more input, it returns a 'CodecR' in the 'CodecF' state.
-- This convention allows @Codec@s to maintain state from one
-- invocation to the next by currying the state into the codec
-- function for the next time it is invoked.  A @Codec@ that cannot
-- process more input returns a 'CodecR' in the 'CodecE' state,
-- possibly including some final output.
type Codec tIn m tOut = Iter tIn m (CodecR tIn m tOut)

-- | The result type of a 'Codec' that translates from type @tIn@ to
-- @tOut@ by executing in monad @'Iter' tIn m@.  The result
-- potentially includes a new 'Codec' for translating subsequent
-- input.
data CodecR tIn m tOut = CodecF { unCodecF :: !(Codec tIn m tOut)
                                , unCodecR :: !tOut }
                          -- ^ This is the normal 'Codec' result,
                          -- which includes another 'Codec' (often the
                          -- same as the one that was just called) for
                          -- processing further input.
                        | CodecE { unCodecR :: !tOut }
                          -- ^ This constructor is used if the 'Codec'
                          -- is ending--i.e., returning for the last
                          -- time--and thus cannot provide another
                          -- 'Codec' to process further input.

-- | Transform an ordinary 'Iter' into a stateless 'Codec'.
iterToCodec :: (ChunkData t, Monad m) => Iter t m a -> Codec t m a
iterToCodec iter = let codec = CodecF codec `liftM` iter in codec

-- | Build an 'Inum' given a 'Codec' that returns chunks of the
-- appropriate type and a 'CtlHandler' to handle control requests.
-- Makes an effort to send an EOF to the codec if the inner 'Iter'
-- fails, so as to facilitate cleanup.  However, if a containing
-- 'Inum' fails, code handling that failure will have to send an EOF
-- or the codec will not be able to clean up.
mkInumC :: (Monad m, ChunkData tIn, ChunkData tOut) =>
           CtlHandler (Iter tIn m)
        -- ^ Control request handler
        -> Codec tIn m tOut
        -- ^ Codec to be invoked to produce transcoded chunks.
        -> Inum tIn tOut m a
mkInumC cf codec = inumMC cf `cat` process
    where
      process iter@(IterF _) =
          catchOrI codec (chkeof iter) $ \codecr ->
              case codecr of
                CodecF c d -> mkInumC cf c (feedI iter $ chunk d)
                CodecE d   -> inumMC cf (feedI iter $ chunk d)
      process iter =
          case codec of
            IterF _ -> catchOrI (runI codec) (chkeof iter) (\_ -> return iter)
            _       -> return iter
      chkeof iter e = if isIterEOF e then return iter else InumFail e iter
                
-- | A variant of 'mkInumC' that passes all control requests from the
-- innner 'Iter' through to enclosing enumerators.  (If you want to
-- reject all control requests, use @'mkInumC' 'noCtl'@ instead of
-- @mkInum@.)
mkInum :: (Monad m, ChunkData tIn, ChunkData tOut) =>
          Codec tIn m tOut
       -- ^ Codec to be invoked to produce transcoded chunks.
       -> Inum tIn tOut m a
mkInum = mkInumC passCtl

-- | A variant of 'mkInum' that transcodes data using a stateless
-- translation 'Iter' instead of a 'Codec'
mkInum' :: (Monad m, ChunkData tIn, ChunkData tOut) =>
           Iter tIn m tOut
        -- ^ This Iteratee will be executed repeatedly to produce
        -- transcoded chunks.
        -> Inum tIn tOut m a
mkInum' fn iter = mkInum (iterToCodec fn) iter

-- | A variant of 'inumBracket' that also takes a 'CtlHandler' (as a
-- function of the input).
inumCBracket :: (Monad m, ChunkData tIn, ChunkData tOut) =>
                (Iter tIn m b)
             -- ^ Before action
             -> (b -> Iter tIn m c)
             -- ^ After action, as a function of before action result
             -> (b -> CtlHandler (Iter tIn m))
             -- ^ Control request handler, as funciton of before action result
             -> (b -> (Codec tIn m tOut))
             -- ^ Input 'Codec', as a funciton of before aciton result
             -> Inum tIn tOut m a
inumCBracket before after cf codec iter0 = tryI before >>= checkBefore
    where
      checkBefore (Left (e, _)) = InumFail e iter0
      checkBefore (Right b)     = finishI (mkInumC (cf b) (codec b) iter0)
                                  >>= checkMain b
      checkMain b iter = tryI (after b) >>= checkAfter iter
      checkAfter iter (Left (e,_)) = iter `inumBind` InumFail e
      checkAfter iter _            = iter

-- | Build an 'Inum' from a @before@ action, an @after@ function, and
-- an @input@ 'Codec' in a manner analogous to the IO @'bracket'@
-- function.  For instance, you could implement @`enumFile'`@ as
-- follows:
--
-- >   enumFile' :: (MonadIO m) => FilePath -> Onum L.ByteString m a
-- >   enumFile' path = inumBracket (liftIO $ openBinaryFile path ReadMode)
-- >                                (liftIO . hClose) doGet
-- >       where
-- >         doGet h = do
-- >           buf <- liftIO $ hWaitForInput h (-1) >> L.hGetNonBlocking h 8192
-- >           return $ if null buf then CodecE L.empty
-- >                                else CodecF (doGet h) buf
--
-- (As a side note, the simple 'L.hGet' function can block when there
-- is some input data but not as many bytes as requested.  Thus, in
-- order to work with named pipes and process data as it arrives, it
-- is best to call 'hWaitForInput' followed by 'L.hGetNonBlocking'
-- rather than simply 'L.hGet'.  This is a common idiom in enumerators
-- that use 'Handle's.)
inumBracket :: (Monad m, ChunkData tIn, ChunkData tOut) =>
               (Iter tIn m b)
            -- ^ Before action
            -> (b -> Iter tIn m c)
            -- ^ After action, as a function of before action result
            -> (b -> (Codec tIn m tOut))
            -- ^ Input 'Codec', as a funciton of before aciton result
            -> Inum tIn tOut m a
inumBracket before after codec iter0 =
    inumCBracket before after (const noCtl) codec iter0

--
-- Monad
--

-- | @IterStateT@ is a lot like the @'StateT'@ monad transformer, but
-- specific to transforming the 'Iter' monad (i.e., type argument @m@
-- should be @'Iter' t m@ for some 'ChunkData' t and 'Monad' m).  What
-- @IterStateT@ does unlike @'StateT'@ is to convert all 'IterFail'
-- failures into 'InumFail' failures, and use the 'InumFail' to
-- propagate the state.  Thus, it is possible to extract the state
-- from an 'IterStateT' even after a failure of the underlying 'Iter'
-- monad.
newtype IterStateT s m a = IterStateT (s -> m (s, a))

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
                 IterStateT s (Iter t m) a -> s -> InumR t t m (s, a)
runIterStateT (IterStateT isf) s = finishI (isf s) >>= return . check
    where check (IterFail e) = InumFail e (s, error "runIterStateT")
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
    isc_return a = IterStateT $ \s -> return (s, a)

    isc_bind m k = IterStateT $ \s -> (runIterStateT m s) >>= check
        where check (InumFail e (s', _)) = InumFail e (s', error "isc_bind")
              check msa = msa >>= \(s', a) -> join $ runIterStateT (k a) s'

    isc_fail msg = IterStateT $ \s -> InumFail (toException $ ErrorCall msg)
                                               (s, error "isc_fail")

instance (IterStateTClass m) => Monad (IterStateT s m) where
    return = isc_return
    (>>=)  = isc_bind
    fail   = isc_fail

instance MonadTrans (IterStateT s) where
    lift m = IterStateT $ \s -> m >>= return . (,) s

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
    where check (InumFail e (s, a)) =
              case fromException e of
                Just e' -> return (s, Left e')
                Nothing -> InumFail e (s, Right a)
          check iter = iter >>= \(s, a) -> return (s, Right a)

-- | This should be the only declaration that requires
-- FlexibleInstances, and only because mtl makes that necessary.
instance (IterStateTClass m) => MonadState s (IterStateT s m) where
    get   = IterStateT $ \s -> return (s, s)
    put s = IterStateT $ \_ -> return (s, ())


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
              else return (fn s, ())

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
      getState (Done (s, _) _)     = s
      getState (InumFail _ (s, _)) = s
      getState _                   = error "runInumM getState"

      tryErr e fnjust nothing = maybe nothing fnjust $ fromException e
      convertDone s InumDone = inumF $
          Done (s {insRemain = mempty}, error "convertDone") (insRemain s)
      convertEOF s (IterEOF _) = return $ return (s, error "convertEOF")
      convertFail iter@(InumFail e (s, _)) =
          tryErr e (convertDone s) $
          (if insAutoEOF s then tryErr e (convertEOF s) else id) $
          return iter
      convertFail iter = return iter

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
               iter <- inumMC (insCtl s) |. inum $ insIter s
               let done = not $ isIterActive iter
               if done && insAutoDone s
                 then do c <- IterF $ \c -> return c
                         InumFail (toException InumDone)
                                  (s {insIter = iter, insRemain = c }, True)
                 else return (s {insIter = iter }, done)

-- | Apply an 'Onum' to the 'Iter' from within the 'InumM' monad.  As
-- with 'ifeed', returns @'True'@ when the 'Iter' is finished.
irun :: (ChunkData tIn, ChunkData tOut, Monad m) =>
        Onum tOut m a -> InumM tIn tOut m a Bool
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
