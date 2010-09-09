
-- | This module contains various functions used to construct 'Inum's.

module Data.IterIO.Inum
    (-- * Enumerator construction functions
     Codec, CodecR(..)
    , iterToCodec
    , mkInumC, mkInum, mkInum'
    , inumCBracket, inumBracket
    -- * Control handler construction functions
    , ctl, ctl', ctlHandler
    -- * Enumerator construction monad
    , IterStateT(..), runIterStateT
    , InumM, mkInumM, feed, pipe
    ) where

import Control.Exception (ErrorCall(..), Exception(..), SomeException(..))
import Control.Monad
import Control.Monad.Trans
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
           CtlHandler tIn tOut m a
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
             -> (b -> CtlHandler tIn tOut m a)
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
      checkAfter iter (Left (e,_)) = iter `failBind` InumFail e
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
-- Control handler functions
--

-- | Wrap a control command for requests of type @carg@ into a
-- function of type @'CtlReq' t m a -> Maybe 'Iter' t m a@, which is
-- not parameterized by @carg@ and therefore can be grouped in a list
-- with control functions for other types.  The intent is then to
-- combine a list of such functions into a 'CtlHandler' with
-- 'tryCtls'.
--
-- As an example, the following funciton produces a 'CtlHandler'
-- (suitable to be passed to 'mkInum' or 'inumCBracket') that
-- implements control operations for three types:
--
-- @
--  fileCtl :: (ChunkData tIn, ChunkData tOut, MonadIO m) =>
--             Handle -> CtlHandler tIn tOut m a
--  fileCtl h = 'ctlHandler' 'passCtl'
--              [ `ctl'` $ \\('SeekC' mode pos) -> 'liftIO' ('hSeek' h mode pos)
--              , `ctl'` $ \\'TellC' -> 'liftIO' ('hTell' h)
--              , `ctl'` $ \\'SizeC' -> 'liftIO' ('hFileSize' h)
--              ]
-- @
ctl :: (CtlCmd carg cres, ChunkData tIn, ChunkData tOut, Monad m) =>
       (carg -> Iter tIn m cres)
    -> CtlReq tOut m a
    -> Maybe (InumR tIn tOut m a)
ctl f (CtlReq carg fr) = case cast carg of
                           Nothing    -> Nothing
                           Just carg' -> Just $ f carg' >>= return . fr . cast

-- | A variant of 'ctl' that, makes the control operation fail if it
-- throws any kind of exception (as opposed to re-propagating the
-- exception as an 'InumFail', which is what would end up happening
-- with 'ctl').
ctl' :: (CtlCmd carg cres, ChunkData tIn, ChunkData tOut, Monad m) =>
        (carg -> Iter tIn m cres)
     -> CtlReq tOut m a -> Maybe (InumR tIn tOut m a)
ctl' f (CtlReq carg fr) = case cast carg of
                            Nothing    -> Nothing
                            Just carg' -> Just $ tryf carg'
    where tryf carg' = tryI (f carg') >>=
                       return . either (\(SomeException _, _) -> fr Nothing)
                                       (fr . cast)

-- | Create a 'CtlHandler' from a list of functions created with 'ctl'
-- or `ctl'`.  Tries each argument type in turn until one succeeds.
-- If none succeeds, runs a fallback handler, which can be 'noCtl',
-- 'passCtl', or another custom 'CtlHandler'.  See the use example
-- given for 'ctl'.
ctlHandler :: (ChunkData tIn, ChunkData tOut, Monad m) =>
              CtlHandler tIn tOut m a
           -- ^ Fallback 'CtlHandler'
           -> [CtlReq tOut m a -> Maybe (InumR tIn tOut m a)]
           -- ^ List of individual request handlers to try (each
           -- created with 'ctl' or `ctl'`).
           -> CtlHandler tIn tOut m a
ctlHandler fallback ctls req = case res of
                                 Nothing   -> fallback req
                                 Just iter -> iter
    where
      res = foldr (\a b -> maybe b Just $ a req) Nothing ctls

--
-- Monad
--

-- | @IterStateT@ is a lot like the @'StateT'@ monad transformer, but
-- specific to transforming the 'Iter' monad (i.e., type argument @m@
-- should be @'Iter' t m@ for some 'ChunkData' t and 'Monad' m).  What
-- @IterStateT@ does unlike @'StateT'@ is to convert all 'IterFail'
-- failures into 'InumFail' failures, and use the 'InumFail' to
-- propagate the state.  Thus, it is possible to extrace the state
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
-- result and in check for errors.  This is the same trick used by
-- 'finishI', which @runIterStateT@ uses internally, so returning the
-- 'InumR' saves other code from needing to invoke 'finishI' a second
-- time.
runIterStateT :: (Monad m, ChunkData t) =>
                IterStateT s (Iter t m) a -> s -> InumR t t m (s, a)
runIterStateT (IterStateT isf) s = finishI (isf s) >>= return . check
    where check (IterFail e)         = InumFail e (s, error "runIterStateT")
          check iter                 = iter

-- | This class is kind of silly and wouldn't be required with
-- -XFlexibleInstances, but is used so that the order and nature of
-- the type arguments to IterStateT is appropriate for a 'MonadTrans'.
class IterStateTClass m where
    isc_return :: a -> IterStateT s m a
    isc_bind   :: IterStateT s m a -> (a -> IterStateT s m b)
               -> IterStateT s m b
    isc_fail   :: String -> IterStateT s m a

-- | Ignore the @IterStateTClass@ class, which has only one instance
-- and is just used for a technicality to get the type parameter order
-- to work out.
instance (Monad m, ChunkData t) => IterStateTClass (Iter t m) where
    isc_return a = IterStateT $ \s -> return (s, a)

    isc_bind m k = IterStateT $ \s -> (runIterStateT m s) >>= \r ->
                   case r of
                     InumFail e (s', _)  -> InumFail e (s', error "isc_bind")
                     _ -> r >>= \(s', a) -> runIterStateT (k a) s' >>= id

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

data InumState tIn tOut m a = InumState {
      insCtl  :: !(CtlHandler tIn tIn m (Iter tOut m a))
    , insIter :: !(Iter tOut m a)
    }

-- | A monad in which to define the actions of an @'Inum' tIn tOut m a@.
type InumM tIn tOut m a = IterStateT (InumState tIn tOut m a) (Iter tIn m)

{-
inumM :: (ChunkData tIn, ChunkData tOut, Monad m) =>
         (CtlHandler tIn tIn m (Iter tOut m a) -> Iter tOut m a
          -> Iter tIn m (Iter tOut m a, r))
      -> InumM tIn tOut m a r
inumM f = IterStateT $ \s@InumState{ insCtl = ch, insIter = iter0 } -> do
            (iter, r) <- f ch iter0
            return (s { insIter = iter }, r)
-}

-- | Feed data the 'Iter' from within the 'InumM' monad.
feed :: (ChunkData tIn, ChunkData tOut, Monad m) =>
        tOut -> InumM tIn tOut m a Bool
feed dat = IterStateT $ \s@InumState{ insCtl = ch, insIter = iter0 } -> do
             iter <- inumMC ch |. inumMC passCtl $ feedI iter0 (chunk dat)
             return (s{ insIter = iter }, isIterActive iter)

-- | Apply another 'Inum' to the 'Iter' from within the 'InumM' monad.
pipe :: (ChunkData tIn, ChunkData tOut, Monad m) =>
        Inum tIn tOut m a -> InumM tIn tOut m a Bool
pipe inum = IterStateT $ \s@InumState{ insCtl = ch, insIter = iter0 } -> do
              iter <- inumMC ch |. inum $ iter0
              return (s{ insIter = iter }, isIterActive iter)

-- | Build an 'Inum' in the 'InumM' monad, using 'lift' to consume
-- input with various 'Iter's, and using 'feed' and 'pipe' to send
-- output.
mkInumM :: (ChunkData tIn, ChunkData tOut, Monad m) =>
           CtlHandler tIn tIn m (Iter tOut m a)
        -> InumM tIn tOut m a b -> Inum tIn tOut m a
mkInumM ch inumm iter0 = do
   result <- runIterStateT inumm InumState { insCtl = ch, insIter = iter0 }
   case result of
     Done (s, _) _     -> return $ insIter s
     InumFail e (s, _) -> InumFail e $ insIter s
     _                 -> error "mkInumM" -- Shouldn't happen
