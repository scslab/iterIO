{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | This module contains two different functions for constructiing
-- 'Inum's.  The first, 'mkInum', creates simple, stateless 'Inum's
-- out of 'Iter's that translate from an input type to an output type.
-- The second, 'mkInumM', creates an 'Inum' out of a computation in
-- the 'InumM' monad.  'InumM' is a 'MonadTrans' around 'Iter'.  Thus
-- 'InumM' computations can consume input by applying 'lift' to any
-- 'Iter' of the appropriate input type and monad, and can produce
-- output by calling any of the 'ifeed', 'ipipe', and 'irun'
-- functions.

module Data.IterIO.Inum
    (-- * Simple Enumerator construction function
     -- $mkInumIntro
     mkInum
    -- * IterStateT monad
    , IterStateT(..), runIterStateT
    , iget, igets, iput, imodify
    -- * Enumerator construction monad
    -- $mkInumMIntro
    , InumM, mkInumM, mkInumAutoM
    , setCtlHandler, setAutoEOF, setAutoDone, addCleanup, withCleanup
    , ifeed, ifeed1, ipipe, irun, irepeat, ipopresid, iunget, idone
    ) where

import Prelude hiding (null)
import Control.Exception (ErrorCall(..), Exception(..))
import Control.Monad
import Control.Monad.Trans
import Data.Monoid
import Data.Typeable

import Data.IterIO.Base

--
-- Simple enumerator construction function
--

-- $mkInumIntro
--
-- The 'mkInum' function allows you to create stateless 'Inum's out of
-- simple transcoding 'Iter's.  As an example, suppose you are
-- processing a list of @L.ByteString@s representing packets, and want
-- to concatenate them all into one continuous stream of bytes.  You
-- could implement an 'Inum' called @inumConcat@ to do this as
-- follows:
--
-- @
--iterConcat :: (Monad m) => 'Iter' [L.ByteString] m L.ByteString
--iterConcat = L.concat ``liftM`` 'dataI'
--
--inumConcat :: (Monad m) => 'Inum' [L.ByteString] L.ByteString m a
--inumConcat = 'mkInum' iterConcat
-- @
--

-- | Construct a simple @'Inum' tIn tOut m a@ from a transcoder of
-- type @'Iter' tIn m tOut@.  The created 'Inum' simply invokes the
-- transcoder over and over again to get more data.  It finishes when
-- the @'Iter' tOut m a@ being fed by the 'Inum' is no longer active
-- or the trancoder @'Iter' tIn m tOut@ throws an exception of class
-- 'IterEOF'.  If the transcoder throws any other kind of exception,
-- the whole 'Inum' fails with an 'InumFail' error.
mkInum :: (Monad m, ChunkData tIn, ChunkData tOut) =>
           Iter tIn m tOut
        -- ^ This Iteratee will be executed repeatedly to produce
        -- transcoded chunks.
        -> Inum tIn tOut m a
mkInum codec = loop
    where
      loop = inumMC passCtl `cat` process
      process iter@(IterF _) = catchOrI codec (\(IterEOF _) -> return iter) $
                               loop . feedI iter . chunk
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
-- This function could equally well have returned @'Iter' t m (a, s)@.
-- However, it returns @'InumR' t t m (a, s)@, equivalent to @'Iter' t
-- m ('Iter' t m (a, s))@, to make it more convenient to inspect the
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

-- | Returns the state in the 'IterStateT' monad.  Analogous to
-- @'get'@ for @'StateT'@.
iget :: (Monad m) => IterStateT s m s
iget = IterStateT $ \s -> return (s, s)

-- | Returns a particular field of the state in the 'IterStateT'
-- monad.  Analogous to @'gets'@ for @'StateT'@.
igets :: (Monad m) => (s -> a) -> IterStateT s m a
igets f = IterStateT $ \s -> return (f s, s)

-- | Sets the state in the 'IterStateT' monad.  Analogous to @'put'@
-- for @'StateT'@.
iput :: (Monad m) => s -> IterStateT s m ()
iput s = IterStateT $ \_ -> return ((), s)

-- | Modifies the state in the 'IterStateT' monad.  Analogous to
-- @'modify'@ for @'StateT'@.
imodify :: (Monad m) => (s -> s) -> IterStateT s m ()
imodify f = IterStateT $ \s -> return ((), f s)


{-
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
-}


--
-- InumM monad
--

{- $mkInumMIntro

Complex 'Inum's that need state and non-trivial control flow can be
constructed using the 'mkInumM' function.  'mkInumM' creates an 'Inum'
out of a computation in the 'InumM' monad.  'InumM' is a 'MonadTrans'
around 'Iter'.  Thus, you can consume input by applying 'lift' to any
'Iter' of the same input type and monad.

The 'InumM' monad implicitly keeps track of the state of the 'Iter'
being fed by the enumerator, which we call the \"target 'Iter'\".
Output can be fed to the target 'Iter' by means of the 'ifeed'
function.  As an example, here is another version of the @inumConcat@
function given previously for 'mkInum' at <#1>:

@
inumConcat :: (Monad m) => 'Inum' [L.ByteString] L.ByteString m a
inumConcat = 'mkInumM' loop
    where loop = do
            'Chunk' t eof <- 'lift' 'chunkI'
            done <- 'ifeed' $ L.concat t
            if not (eof || done)
              then loop
              else do resid <- 'ipopresid'
                      'iunget' [resid]
@

There are several points to note about this function.  It reads data
in 'Chunk's using 'chunkI', rather than just inputting data with
'dataI'.  Because we are in the @'InumM' tIn tOut m a@ monad and not
the @'Iter' t m@ monad, we must use 'lift' to invoke an 'Iter'
computation such as 'chunkI'.  The choice of 'chunkI' rather than
'dataI' allows @inumConcat@ to see the @eof@ flag and know when there
is no more input.  It also avoids throwing an 'IterEOF' exception on
end of file, as 'dataI' would; such an exception would cause the
'Inum' to fail.

As previously mentioned, data is fed to the target 'Iter', which here
is of type @'Iter' L.ByteString m a@, using 'ifeed'.  'ifeed' returns
a 'Bool' that is @'True'@ when the 'Iter' is no longer active.  This
brings us to another point--there is no implicit looping or
repetition.  We explicitly loop via a tail-recursive call to @loop@ so
long as the @eof@ flag is clear and 'ifeed' returned @'False'@ because
the target 'Iter' has not finished.

What happens when @eof@ or @done@ is set?  One possibility is to do
nothing.  This is often correct.  Falling off the end of the 'InumM'
do-block causes the 'Inum' to return the current state of the 'Iter'.
However, it may be that the 'Inum' has been fused to the target
'Iter', in which case any left-over residual data fed to but not
consumed by the target 'Iter' will be discarded.  We may instead want
to put the data back onto the input stream.  The 'ipopresid' function
extracts any left-over data from the target 'Iter', while 'iunget'
places data back in the input stream.  Since here the input stream is
a list of @L.ByteString@s, we have to place @resid@ in a list.  (After
doing this, the list element boundaries may be different, but all the
input bytes will be there.)  Note that the version of @inumConcat@
implemented with 'mkInum' at <#1> does not have this input-restoring
feature.

The code above looks much clumsier than the version based on 'mkInum',
but several of these steps can be made implicit.  There is an
/AutoEOF/ flag, controlable with the 'setAutoEOF' function, that
causes 'IterEOF' exceptions to produce normal termination of the
'Inum', rather than failure.  Another flag, /AutoDone/, is controlable
with the 'setAutoDone' function and causes the 'Inum' to exit
immediately when the underlying 'Iter' is no longer active (i.e., the
'ifeed' function returns @'True'@).  Both of these flags are set at
once by the 'mkInumAutoM' function, which yields the following simpler
implementation of @inumConcat@:

@
inumConcat = 'mkInumAutoM' $ do
  'addCleanup' $ 'ipopresid' >>= 'iunget' . (: [])
  loop
    where loop = do
            t <- 'lift' 'dataI'    -- AutoEOF flag will handle IterEOF err
            'ifeed' $ L.concat t -- AutoDone flag will catch True result
            loop
@

The 'addCleanup' function registers actions that should always be
executed when the 'Inum' finishes.  Here we use it to place residual
data from the target 'Iter' back into the `Inum`'s input stream.

Finally, there is a function 'irepeat' that automatically sets the
/AutoEOF/ and /AutoDone/ flags and then loops forever on an 'InumM'
computation.  Using 'irepeat' to simplify further, we have:

@
'inumConcat' = 'mkInumM' $ 'withCleanup' ('ipopresid' >>= 'iunget' . (: [])) $
             'irepeat' $ 'lift' 'dataI' >>= 'ifeed' . L.concat
@

'withCleanup', demonstrated here, is a variant of 'addCleanup' that is
in effect only for the duration of a single action, and only if that
action executes a non-local exit (e.g., by calling 'idone', throwing
an exception, or triggering temrination via the /AutoEOF/ or
/AutoDone/ flags).

In addition to 'ifeed', the 'ipipe' function invokes a different
'Inum' from within the 'InumM' monad, piping its output directly to
the target 'Iter'.  As an example, consider an 'Inum' that processes a
mail message and appends a signature line, implemented as follows:

@
inumAddSig :: (Monad m) => 'Inum' L.ByteString L.ByteString m a
inumAddSig = 'mkInumM' $ do
  'ipipe' 'inumNop'
  'ifeed' $ L8.pack \"\\n--\\nSent from my Haskell interpreter.\\n\"
@

Here we start by using 'inumNop' to \"pipe\" all input to the target
'Iter' unmodified.  On reading an end of file, 'inumNop' returns, at
which point we use 'ifeed' to append our signature.

A similar function 'irun' runs an 'Onum' (or 'Inum' of a different
type) on the target 'Iter'.  For instance, to read the signature from
a file called @\".signature\"@, one could use:

@
inumAddSig :: ('MonadIO' m) => 'Inum' L.ByteString L.ByteString m a
inumAddSig = 'mkInumM' $ do
  'ipipe' 'inumNop'
  'irun' $ 'enumFile' \".signature\"
@

Of course, these examples are a bit contrived.  An even simpler
implementation is:

@
inumAddSig = 'inumNop' ``cat`` 'runI' . 'enumFile' \".signature\"
@

The @.@ between 'runI' and @'enumFile'@ is because 'Inum's are
functions from 'Iter's to 'Iter's; we want to apply 'runI' to the
result applying @'enumFile' \".signature\"@ to an 'Iter'.  Spelled
out, the type of @'enumFile'@ is:

@
enumFile :: (MonadIO m, ChunkData t, ListLikeIO t e) =>
            FilePath
         -> 'Iter' t m a
         -> 'Iter' () m a ('Iter' t m a)
@

-}

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

-- | A monad in which to define the actions of an @'Inum' tIn tOut m
-- a@.  Note @InumM tIn tOut m a@ is a 'Monad' of kind @* -> *@.  @a@
-- is the (almost always parametric) return type of the 'Inum'.  A
-- fifth type argument is required for monadic computations of kind
-- @*@, e.g.:
--
-- > seven :: InumM tIn tOut m a Int
-- > seven = return 7
type InumM tIn tOut m a = IterStateT (InumState tIn tOut m a) (Iter tIn m)

data InumDone = InumDone deriving (Show, Typeable)
instance Exception InumDone

-- | Set the control handler an 'Inum' should use from within an
-- 'InumM' computation.  (The default is 'passCtl'.)
setCtlHandler :: (ChunkData tIn, Monad m) =>
                  CtlHandler (Iter tIn m) -> InumM tIn tOut m a ()
setCtlHandler ch = imodify $ \s -> s { insCtl = ch }

-- | Set the /AutoEOF/ flag within an 'InumM' computation.  If this
-- flag is 'True', handle 'IterEOF' exceptions like a normal but
-- immediate termination of the 'Inum'.  If this flag is @'False'@
-- (the default), then 'IterEOF' exceptions must be manually caught or
-- they will terminate the thread.
setAutoEOF :: (ChunkData tIn, Monad m) => Bool -> InumM tIn tOut m a ()
setAutoEOF val = imodify $ \s -> s { insAutoEOF = val }

-- | Set the /AutoDone/ flag within an 'InumM' computation.  When
-- @'True'@, the 'Inum' will immediately terminate as soon as the
-- 'Iter' it is feeding enters a non-active state (i.e., 'Done' or a
-- failure state).  If this flag is @'False'@ (the default), the
-- 'InumM' computation will need to monitor the results of the
-- 'ifeed', 'ipipe', and 'irun' functions to ensure the 'Inum'
-- terminates when one of these functions returns @'False'@.
setAutoDone :: (ChunkData tIn, Monad m) => Bool -> InumM tIn tOut m a ()
setAutoDone val = imodify $ \s -> s { insAutoDone = val }

-- | Like imodify, but throws an error if the insCleaning flag is set.
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

-- | Run an 'InumM' action with some cleanup action in effect for
-- non-local termination (which means an invocation of 'idone', an
-- exception, or termination of the 'Inum' because of /AutoDone/ or
-- /AutoEOF/).  If the action returns without causing non-local
-- termination of the 'Inum', then the cleanup action is never
-- executed and the effects of any calls to 'addCleanup' within the
-- 'InumM' are reset.
withCleanup :: (ChunkData tIn, Monad m) =>
              InumM tIn tOut m a () -- ^ Cleanup action
           -> InumM tIn tOut m a b  -- ^ Action to execute with Cleanup
           -> InumM tIn tOut m a b
withCleanup clean action = do
  old <- igets insCleanup
  ncmodify $ \s -> s { insCleanup = clean >> old }
  b <- action
  imodify $ \s -> s { insCleanup = old }
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
-- input of type @tIn@.  It must then feed output of type @tOut@ to an
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
-- but instead just run cleanup functions and exit the 'Inum'
-- immediately.)
ifeed :: (ChunkData tIn, ChunkData tOut, Monad m) =>
         tOut -> InumM tIn tOut m a Bool
ifeed = ipipe . inumPure

-- | A variant of 'ifeed' that throws an exception of type 'IterEOF'
-- if the data being fed is 'null'.  Convenient when reading input
-- with a function (such as "Data.ListLike"'s @hget@) that returns 0
-- bytes instead of throwing an EOF exception to indicate end of file.
-- For instance, the main loop of @'enumFile'@ looks like:
--
-- @
--  'irepeat' $ 'liftIO' ('LL.hGet' h 'defaultChunkSize') >>= 'ifeed1'
-- @
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
    then do c <- IterF return
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
-- which case the exception propagates further and may cause the
-- 'Inum' to fail.)  @irepeat@ sets both the /AutoEOF/ and
-- /AutoDone/ flags to @'True'@.
irepeat :: (ChunkData tIn, Monad m) =>
           InumM tIn tOut m a b -> InumM tIn tOut m a ()
irepeat action = do
  imodify $ \s -> s { insAutoEOF = True, insAutoDone = True }
  let loop = action >> loop in loop

-- | If the target 'Iter' being fed by the 'Inum' is in the 'Done'
-- state, this funciton pops the residual data out of the 'Iter' and
-- returns it.  If the target is in any other state, returns 'mempty'.
ipopresid :: (ChunkData tIn, ChunkData tOut, Monad m) => InumM tIn tOut m a tOut
ipopresid = IterStateT $ \s ->
    case insIter s of
      Done a (Chunk t eof) ->
          return (t, s { insIter = Done a $ Chunk mempty eof })
      _ -> return (mempty, s)

-- | @iunget = 'lift' . 'ungetI'@.
iunget :: (MonadTrans mt, ChunkData t, Monad m) => t -> mt (Iter t m) ()
iunget = lift . ungetI

-- | Immediately perform a successful exit from an 'InumM' monad,
-- terminating the 'Inum' and returning the current state of the
-- 'Iter'.  Can be used to end an 'irepeat' loop.  (Use @'lift' $
-- 'throwI' ...@ for an unsuccessful exit.)
idone :: (ChunkData tIn, Monad m) => InumM tIn tOut m a b
idone = IterStateT $ \s -> do
          c <- IterF return
          InumFail (toException InumDone) (error "idone", s { insRemain = c })
