{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | This module contains two different functions for constructiing
-- 'Inum's.  The first, 'mkInum', creates simple, stateless 'Inum's
-- out of 'Iter's that translate from an input type to an output type.
-- The second, 'mkInumM', creates an 'Inum' out of a computation in
-- the 'InumM' monad.  'InumM' is an 'Iter' that wraps its inner
-- monadic type with an 'IterStateT' to keep track of the state of the
-- 'Iter' being feed data.  'InumM' computations can consume input by
-- invoking 'Iter' computations (though 'Iter's that are not
-- polymorphic in their monad may need to be transformed with the
-- 'liftIterM' function).  The 'InumM' computation can produce output
-- by calling any of the 'ifeed', 'ipipe', and 'irun' functions.

module Data.IterIO.Inum
    (-- * Simple Enumerator construction function
     -- $mkInumIntro
     mkInum
    -- * Enumerator construction monad
    -- $mkInumMIntro
    , InumM, mkInumM, mkInumAutoM
    , setCtlHandler, setAutoEOF, setAutoDone, addCleanup, withCleanup
    , ifeed, ifeed1, ipipe, irun, irepeat, ipopresid, idone
    ) where

import Prelude hiding (null)
import Control.Exception (Exception(..))
import Data.Monoid
import Data.Typeable

import Data.IterIO.Base
import Data.IterIO.Trans

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
-- #mkInumExample#
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
-- type @'Iter' tIn m tOut@.  The 'Inum' created simply invokes the
-- transcoder over and over again to get more data.  It finishes when
-- the \"target\" 'Iter' (of type @'Iter' tOut m a@) being fed by the
-- 'Inum' is no longer active, or when the trancoder 'Iter' throws an
-- exception of class 'IterEOF'.  If the transcoder throws any other
-- kind of exception, the whole 'Inum' fails with an 'InumFail' error.
mkInum :: (Monad m, ChunkData tIn, ChunkData tOut) =>
           Iter tIn m tOut
        -- ^ This \"transcoder\" 'Iter' is executed repeatedly to
        -- produce transcoded chunks of type @tOut@.
        -> Inum tIn tOut m a
mkInum codec = loop
    where
      loop = inumMC passCtl `cat` process
      process iter@(IterF _) = catchOrI codec (\(IterEOF _) -> return iter) $
                               loop . feedI iter . chunk
      process iter = return iter


--
-- InumM monad
--

{- $mkInumMIntro

Complex 'Inum's that need state and non-trivial control flow can be
constructed using the 'mkInumM' function to produce an 'Inum' out of a
computation in the 'InumM' monad.  The 'InumM' monad implicitly keeps
track of the state of the 'Iter' to which the 'Inum' is feeding data,
which we call the \"target\" 'Iter'.

'InumM' is an 'Iter' monad, and so can consume input by invoking
ordinary 'Iter' actions.  However, to keep track of the state of the
target 'Iter', 'InumM' wraps its inner monadic type with an
'IterStateT' transformer.  Specifically, when creating an enumerator
of type @'Inum' tIn tOut m a@, the 'InumM' action is of a type like
@'Iter' tIn ('IterStateT' (InumStateT ...) m) ()@.  That means that to
execute actions of type @'Iter' tIn m a@ that are not polymorphic in
@m@, you have to transform them with the 'liftIterM' function.

Output can be fed to the target 'Iter' by means of the 'ifeed'
function.  As an example, here is another version of the @inumConcat@
function given previously for 'mkInum' at <#mkInumExample>:

@
inumConcat :: (Monad m) => 'Inum' [L.ByteString] L.ByteString m a
inumConcat = 'mkInumM' loop
    where loop = do
            'Chunk' t eof <- 'chunkI'
            done <- 'ifeed' $ L.concat t
            if not (eof || done)
              then loop
              else do resid <- 'ipopresid'
                      'ungetI' [resid]
@

There are several points to note about this function.  It reads data
in 'Chunk's using 'chunkI', rather than just inputting data with
'dataI'.  The choice of 'chunkI' rather than 'dataI' allows
@inumConcat@ to see the @eof@ flag and know when there is no more
input.  'chunkI' also avoids throwing an 'IterEOF' exception on end of
file, as 'dataI' would.  In contrast to 'mkInum', which gracefully
interprets 'IterEOF' exceptions as an exit request, 'mkInumM' by
default treats such exceptions as an 'Inum' failure.

As previously mentioned, data is fed to the target 'Iter', which here
is of type @'Iter' L.ByteString m a@, using 'ifeed'.  'ifeed' returns
a 'Bool' that is @'True'@ when the 'Iter' is no longer active.  This
brings us to another point--there is no implicit looping or
repetition.  We explicitly loop via a tail-recursive call to @loop@ so
long as the @eof@ flag is clear and 'ifeed' returned @'False'@
indicating the target 'Iter' has not finished.

What happens when @eof@ or @done@ is set?  One possibility is to do
nothing.  This is often correct.  Falling off the end of the 'InumM'
do-block causes the 'Inum' to return the current state of the 'Iter'.
However, it may be that the 'Inum' has been fused to the target
'Iter', in which case any left-over residual data fed to, but not
consumed by, the target 'Iter' will be discarded.  We may instead want
to put the data back onto the input stream.  The 'ipopresid' function
extracts any left-over data from the target 'Iter', while 'ungetI'
places data back in the input stream.  Since here the input stream is
a list of @L.ByteString@s, we have to place @resid@ in a list.  (After
doing this, the list element boundaries may be different, but all the
input bytes will be there.)  Note that the version of @inumConcat@
implemented with 'mkInum' at <#mkInumExample> does not have this
input-restoring feature.

The code above looks much clumsier than the version based on 'mkInum',
but several of these steps can be made implicit.  There is an
/AutoEOF/ flag, controlable with the 'setAutoEOF' function, that
causes 'IterEOF' exceptions to produce normal termination of the
'Inum', rather than failure (just as 'mkInum' handles such
exceptions).  Another flag, /AutoDone/, is controlable with the
'setAutoDone' function and causes the 'Inum' to exit immediately when
the underlying 'Iter' is no longer active (i.e., the 'ifeed' function
returns @'True'@).  Both of these flags are set at once by the
'mkInumAutoM' function, which yields the following simpler
implementation of @inumConcat@:

@
inumConcat = 'mkInumAutoM' $ do 'addCleanup' $ 'ipopresid' >>= 'ungetI' . (: [])
                              loop
    where loop = do
            t <- 'dataI'         -- AutoEOF flag will handle IterEOF err
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
'inumConcat' = 'mkInumM' $ 'withCleanup' ('ipopresid' >>= 'ungetI' . (: [])) $
             'irepeat' $ 'dataI' >>= 'ifeed' . L.concat
@

'withCleanup', demonstrated here, is a variant of 'addCleanup' that
cleans up after a particular action, rather than at the end of the
`Inum`'s whole execution.  (At the outermost level, as used here,
`withCleanup`'s effects are identical to `addCleanup`'s.)

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
result of applying @'enumFile' \".signature\"@ to an 'Iter'.  Spelled
out, the type of @'enumFile'@ is:

@
enumFile :: (MonadIO m, ChunkData t, ListLikeIO t e) =>
            FilePath
         -> 'Iter' t m a
         -> 'Iter' () m a ('Iter' t m a)
@

-}

-- | Internal data structure for the 'InumM' monad's state.
data InumState tIn tOut m a = InumState {
      insAutoEOF :: !Bool
    , insAutoDone :: !Bool
    , insCtl :: !(CtlHandler (Iter tIn m))
    , insIter :: !(Iter tOut m a)
    , insCleanup :: !(InumM tIn tOut m a ())
    , insCleaning :: !Bool
    }

defaultInumState :: (ChunkData tIn, Monad m) => InumState tIn tOut m a
defaultInumState = InumState {
                     insAutoEOF = False
                   , insAutoDone = False
                   , insCtl = passCtl
                   , insIter = IterF $ const $ error "insIter"
                   , insCleanup = return ()
                   , insCleaning = False
                   }

-- | A monad in which to define the actions of an @'Inum' tIn tOut m
-- a@.  Note @InumM tIn tOut m a@ is a 'Monad' of kind @* -> *@, where
-- @a@ is the (almost always parametric) return type of the 'Inum'.  A
-- fifth type argument is required for monadic computations of kind
-- @*@, e.g.:
--
-- > seven :: InumM tIn tOut m a Int
-- > seven = return 7
--
-- Another important thing to note about the 'InumM' monad, as
-- described in the documentation for 'mkInumM', is that you must call
-- @'lift'@ twice execute actions in monad @m@, and you must use the
-- 'liftIterM' function to execute actions in monad @'Iter' t m a@.
type InumM tIn tOut m a = Iter tIn (IterStateT (InumState tIn tOut m a) m)

data InumDone = InumDone deriving (Show, Typeable)
instance Exception InumDone

-- | Set the control handler an 'Inum' should use from within an
-- 'InumM' computation.  (The default is 'passCtl'.)
setCtlHandler :: (ChunkData tIn, Monad m) =>
                 CtlHandler (Iter tIn m)
              -> InumM tIn tOut m a ()
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
ncmodify fn = imodify $ \s -> if insCleaning s
                              then error "illegal call within Cleanup function"
                              else fn s

-- | Add a cleanup action to be executed when the 'Inum' finishes, or,
-- if used in conjunction with the 'withCleanup' function, when the
-- innermost enclosing 'withCleanup' action finishes.
addCleanup :: (ChunkData tIn, Monad m) =>
              InumM tIn tOut m a () -> InumM tIn tOut m a ()
addCleanup clean = ncmodify $ \s -> s { insCleanup = clean >> insCleanup s }

-- | Run an 'InumM' with some cleanup action in effect.  The cleanup
-- action specified will be executed when the main action returns,
-- whether normally, through an exception, because of the /AutoDone/
-- or /AutoEOF/ flags, or because 'idone' is invoked.
--
-- Note @withCleanup@ also defines the scope of actions added by the
-- 'addCleanup' function.  In other words, given a call such as
-- @withCleanup cleaner1 main@, if @main@ invokes @'addCleanup'
-- cleaner2@, then both @cleaner1@ and @cleaner2@ will be executed
-- upon @main@'s return, even if the overall 'Inum' has not finished
-- yet.
withCleanup :: (ChunkData tIn, Monad m) =>
              InumM tIn tOut m a () -- ^ Cleanup action
           -> InumM tIn tOut m a b  -- ^ Main action to execute
           -> InumM tIn tOut m a b
withCleanup clean action = do
  old <- igets insCleanup
  ncmodify $ \s -> s { insCleanup = clean }
  action `finallyI` do
    newclean <- igets insCleanup
    imodify $ \s -> s { insCleanup = old }
    newclean

-- | Convert an 'InumM' computation into an 'Inum', given some
-- @'InumState'@ to run on.
runInumM :: (ChunkData tIn, ChunkData tOut, Monad m) =>
            InumM tIn tOut m a b
         -- ^ Monadic computation defining the 'Inum'.
         -> InumState tIn tOut m a
         -- ^ State to run on
         -> Iter tIn m (Iter tOut m a)
runInumM inumm s0 = do
  (result1, s1) <- runIterStateT inumm s0 >>= convertFail
  (result2, s2) <- runIterStateT (insCleanup s1) s1 { insAutoDone = False
                                                    , insCleaning = True }
                   >>= convertFail
  let iter = insIter s2
  case (result1, result2) of
    (IterFail e _, _) -> InumFail e iter mempty
    (_, IterFail e _) -> InumFail e iter mempty
    _                 -> return iter
    where
      convertFail (InumFail e _ c, s) = convertFail (IterFail e c, s)
      convertFail (iter@(IterFail e _), s) =
          if isInumDone e || (insAutoEOF s && isIterEOF e)
          then return (return $ error "runInumM", s)
          else return (iter, s)
      convertFail is = return is
      isInumDone e = maybe False (\InumDone -> True) $ fromException e

-- | A variant of 'mkInumM' that sets /AutoEOF/ and /AutoDone/ to
-- 'True' by default.  (Equivalent to calling @'setAutoEOF' 'True' >>
-- 'setAutoDone' 'True'@ as the first thing inside 'mkInumM'.)
mkInumAutoM :: (ChunkData tIn, ChunkData tOut, Monad m) =>
               InumM tIn tOut m a b -> Inum tIn tOut m a
mkInumAutoM inumm iter0 =
    runInumM inumm defaultInumState { insIter = iter0
                                    , insAutoEOF = True
                                    , insAutoDone = True
                                    }


-- | Build an 'Inum' out of an 'InumM' computation.  If you run
-- 'mkInumM' inside the @'Iter' tIn m@ monad (i.e., to create an
-- enumerator of type @'Inum' tIn tOut m a@), then the 'InumM'
-- computation will be in a Monad of type @'Iter' t tm@ where @tm@ is
-- a transformed version of @m@.  This has the following two
-- consequences:
--
--  - If you wish to execute actions in monad @m@ from within your
--    'InumM' computation, you will have to apply @'lift'@ twice (as
--    in @'lift' $ 'lift' action_in_m@) rather than just once.
--
--  - If you need to execute actions in the @'Iter' t m@ monad, you
--    will have to lift them with the 'liftIterM' function.
--
-- The 'InumM' computation you construct can feed output of type
-- @tOut@ to the target 'Iter' (which is implicitly contained in the
-- monad state), using the 'ifeed', 'ipipe', and 'irun' functions.
mkInumM :: (ChunkData tIn, ChunkData tOut, Monad m) =>
           InumM tIn tOut m a b -> Inum tIn tOut m a
mkInumM inumm iter0 =
    runInumM inumm defaultInumState { insIter = iter0 }

-- | Used from within the 'InumM' monad to feed data to the target
-- 'Iter'.  Returns @'False'@ if the target 'Iter' is still active and
-- @'True'@ if the iter has finished and the 'Inum' should also
-- return.  (If the @autoDone@ flag is @'True'@, then @ifeed@,
-- @ipipe@, and @irun@ will never actually return @'True'@, but
-- instead just immediately run cleanup functions and exit the
-- 'Inum' when the target 'Iter' stops being active.)
ifeed :: (ChunkData tIn, ChunkData tOut, Monad m) =>
         tOut -> InumM tIn tOut m a Bool
ifeed = ipipe . enumPure

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
ifeed1 dat = if null dat then throwEOFI "ifeed1" else ifeed dat

-- | Apply another 'Inum' to the target 'Iter' from within the 'InumM'
-- monad.  As with 'ifeed', returns @'True'@ when the 'Iter' is
-- finished.
ipipe :: (ChunkData tIn, ChunkData tOut, Monad m) =>
         Inum tIn tOut m a -> InumM tIn tOut m a Bool
ipipe inum = do
  s <- iget
  iter <- liftIterM $ inumRepeat (inumMC (insCtl s) `cat` inumF) |. inum $
          insIter s
  iput s { insIter = iter }
  let done = not $ isIterActive iter
  if done && insAutoDone s then idone else return done

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
ipopresid :: (ChunkData tIn, ChunkData tOut, Monad m) =>
             InumM tIn tOut m a tOut
ipopresid = do
  s <- iget
  case insIter s of
    Done a (Chunk t eof) -> iput s { insIter = Done a $ Chunk mempty eof } >>
                            return t
    _                    -> return mempty

-- | Immediately perform a successful exit from an 'InumM' monad,
-- terminating the 'Inum' and returning the current state of the
-- target 'Iter'.  Can be used to end an 'irepeat' loop.  (Use
-- @'throwI' ...@ for an unsuccessful exit.)
idone :: (ChunkData tIn, Monad m) => InumM tIn tOut m a b
idone = throwI InumDone
