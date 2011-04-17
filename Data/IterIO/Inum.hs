{-# LANGUAGE DeriveDataTypeable #-}

module Data.IterIO.Inum
    (-- * Base types
     Inum, Onum
    -- * concatenation and fusing operators
    , (|$), (.|$), cat, lcat, (|.), (.|)
    -- * Exception functions
    , inumCatch, inumHandler, inumFinally, inumOnException
    , resumeI, verboseResumeI
    -- * Simple Enumerator construction function
    -- $mkInumIntro
    , ResidHandler, CtlHandler
    , mkInum
    -- * Utilities
    , pullupResid
    , noCtl, passCtl, consCtl, mkCtl, mkFlushCtl
    , runIterM, runIterMC, runInum
    -- * Some basic Inums
    , inumNop, inumNull, inumPure, inumRepeat
    -- * Enumerator construction monad
    -- $mkInumMIntro
    , InumM, mkInumM, mkInumAutoM
    , setCtlHandler, setAutoEOF, setAutoDone, addCleanup, withCleanup
    , ifeed, ifeed1, ipipe, irun, irepeat, ipopresid, idone
    ) where

import Prelude hiding (null)
import Control.Exception (Exception(..), SomeException(..))
import Control.Monad.Trans
import Data.Monoid
import Data.Typeable
import System.Environment (getProgName)
import System.IO

import Data.IterIO.Iter
import Data.IterIO.Trans

--
-- Enumerator types
--

-- | The type of an /iterator-enumerator/, which transcodes data from
-- some input type @tIn@ to some output type @tOut@.  An @Inum@ acts
-- as an 'Iter' when consuming data, then acts as an enumerator when
-- feeding transcoded data to another 'Iter'.
--
-- At a high level, one can think of an @Inum@ as a function from
-- 'Iter's to 'Iter's, where an @Inum@'s input and output types are
-- different.  A simpler alternative to @Inum@ might have been:
--
-- > type Inum' tIn tOut m a = Iter tOut m a -> Iter tIn m a
--
-- In fact, given an 'Inum' object @inum@, it is possible to construct
-- a function of type @Inum'@ with @(inum '.|')@.  But sometimes one
-- might like to concatenate 'Inum's.  For instance, consider a
-- network protocol that changes encryption or compression modes
-- midstream.  Transcoding is done by 'Inum's.  To change transcoding
-- methods after applying an @Inum@ to an iteratee requires the
-- ability to \"pop\" the iteratee back out of the 'Inum' so as to be
-- able to hand it to another 'Inum'.  `Inum`'s return type allows the
-- monadic bind operator '>>=' to accomplish this popping.
--
-- An @Inum@ must never feed an EOF chunk to its iteratee.  Instead,
-- upon receiving EOF, the @Inum@ should simply return the state of
-- the inner 'Iter' (this is how \"popping\" the iteratee back out
-- works).  An @Inum@ should also return when the iteratee returns a
-- result or fails, or when the @Inum@ itself fails.  An @Inum@ may
-- return the state of the iteratee earlier, if it has reached some
-- logical message boundary (e.g., many protocols finish processing
-- headers upon reading a blank line).
--
-- @Inum@s are generally constructed with the 'mkInum' function, which
-- hides most of the error handling details.  Most @Inum@s are
-- polymorphic in the last type, @a@, in order to work with iteratees
-- returning any type.
type Inum tIn tOut m a = Iter tOut m a -> Iter tIn m (IterR tOut m a)

-- | An @Onum t m a@ is just an 'Inum' in which the input is
-- @()@--i.e., @'Inum' () t m a@--so that there is no meaningful input
-- data to transcode.  Such an enumerator is called an
-- /outer enumerator/, because it must produce the data it feeds to
-- 'Iter's by either executing actions in monad @m@, or from its own
-- internal pure state (as for 'enumPure').
--
-- Under no circumstances should an @Onum@ ever feed a chunk with the
-- EOF bit set to its 'Iter' argument.  When the @Onum@ runs out of
-- data, it must simply return the current state of the 'Iter'.  This
-- way more data from another source can still be fed to the iteratee,
-- as happens when enumerators are concatenated with the 'cat'
-- function.
--
-- @Onum@s should generally be constructed using the 'mkInum' or
-- 'mkInumM' function, just like 'Inum's, the only difference being
-- that for an @Onum@ the input type is @()@, so executing 'Iter's to
-- consume input will be of little use.
type Onum t m a = Inum () t m a

-- Concatenation and fusing functions

-- | Run an 'Onum' on an 'Iter'.  This is the main way of actually
-- executing IO with 'Iter's.  @|$@ is a type-restricted version of
-- the following code, in which @inum@ must be an 'Onum':
--
-- @
--  inum |$ iter = 'run' (inum .| iter)
--  infixr 2 |$
-- @
(|$) :: (ChunkData t, Monad m) => Onum t m a -> Iter t m a -> m a
(|$) inum iter = run (inum .| iter)
infixr 2 |$

-- | @.|$@ is a variant of '|$' that allows you to apply an 'Onum'
-- from within an 'Iter' monad.  This is often useful in conjuction
-- with 'enumPure', if you want to parse at some coarse-granularity
-- (such as lines), and then re-parse the contents of some
-- coarser-grained parse unit.  For example:
--
-- >     rawcommand <- lineI
-- >     command <- enumPure rawcommand .|$ parseCommandI
-- >     return Request { cmd = command, rawcmd = rawcommand }
--
-- @.|$@ has the same fixity as @|$@, namely:
--
-- > infixr 2 .|$
--
-- As suggested by the types, @enum .|$ iter@ is sort of equivalent to
-- @'lift' (enum |$ iter)@, except that the latter will call 'throw'
-- on failures, causing language-level exceptions that cannot be
-- caught within the outer 'Iter'.  Thus, it is better to use @.|$@
-- than @'lift' (... '|$' ...)@, though in the less general case of
-- the IO monad, @enum .|$ iter@ is equivalent to @'liftIO' (enum '|$'
-- iter)@ as illustrated by the following examples:
--
-- > -- Catches exception, because .|$ propagates failure through the outer
-- > -- Iter Monad, where it can still be caught.
-- > apply1 :: IO String
-- > apply1 = enumPure "test1" |$ iter `catchI` handler
-- >     where
-- >       iter = enumPure "test2" .|$ fail "error"
-- >       handler (SomeException _) _ = return "caught error"
-- > 
-- > -- Does not catch error.  |$ turns the Iter failure into a language-
-- > -- level exception, which can only be caught in the IO Monad.
-- > apply2 :: IO String
-- > apply2 = enumPure "test1" |$ iter `catchI` handler
-- >     where
-- >       iter = lift (enumPure "test2" |$ fail "error")
-- >       handler (SomeException _) _ = return "caught error"
-- > 
-- > -- Catches the exception, because liftIO uses the IO catch function to
-- > -- turn language-level exceptions into monadic Iter failures.  (By
-- > -- contrast, lift works in any Monad, so cannot do this in apply2.)
-- > -- This example illustrates how liftIO is not equivalent to lift.
-- > apply3 :: IO String
-- > apply3 = enumPure "test1" |$ iter `catchI` handler
-- >     where
-- >       iter = liftIO (enumPure "test2" |$ fail "error")
-- >       handler (SomeException _) _ = return "caught error"
(.|$) :: (ChunkData tIn, ChunkData tOut, Monad m) =>
         Onum tOut m a -> Iter tOut m a -> Iter tIn m a
(.|$) enum iter = runI (enum .| iter)
infixr 2 .|$

-- | Concatenate the outputs of two enumerators.  For example,
-- @'enumFile' \"file1\" \`cat\` 'enumFile' \"file2\"@ produces an
-- 'Onum' that outputs the concatenation of files \"file1\" and
-- \"file2\".  Unless there is an 'InumFail' failure, @cat@ always
-- invokes both 'Inum's, as the second 'Inum' may have monadic
-- side-effects that must be executed even when the 'Iter' has already
-- finished.  See 'lcat' if you want to stop when the 'Iter' no longer
-- requires input.  If you want to continue executing even in the
-- event of an 'InumFail' condition, you can wrap the first 'Inum'
-- with 'inumCatch'.
--
-- THE REST OF THIS TEXT NEEDS TO BE ADJUSTED
--
-- @cat@ is useful in right folds.  Say, for example, that @files@ is
-- a list of files that you want to concatenate.  You can use a
-- construct such as:
--
-- @
--  catFiles :: ('MonadIO' m) => ['FilePath'] -> 'Onum' 'L.ByteString' m a
--  catFiles files = 'foldr' ('cat' . 'inumLazy' . 'enumFile') 'return' files
-- @
--
-- Note the use of 'return' as the starting value for 'foldr'.  The
-- type of 'return' as used here in the @'Iter' () m@ monad is @iter
-- -> 'Iter' () m iter@, or more precisely @'Iter' 'L.ByteString' m a
-- -> 'Iter' () m ('Iter' 'L.ByteString' m a)@, which is equivalent to
-- @'Onum' 'L.ByteString' m a@--effectively a dummy no-operation
-- 'Inum'.  ('return' acts like a no-op 'Inum' for concatentation,
-- while 'inumNop' is the no-op for fusing.)  Also note the use of
-- 'inumLazy' as an optimization to avoid processing files once the
-- 'Iter' has finished.
--
-- @cat@ has fixity:
--
-- > infixr 3 `cat`
cat :: (ChunkData tIn, ChunkData tOut, Monad m) =>
       Inum tIn tOut m a      -- ^
    -> Inum tIn tOut m a
    -> Inum tIn tOut m a
cat a b iter = tryI' (runInum a iter) >>= either reRunIter (b . reRunIter)
-- Note this was carefully constructed to preserve InumFail errors.
-- Something like:  cat a b iter = a iter >>= b . reRunIter
-- would not preserve InumFail errors; since the input and output
-- types of >>= do not have to be the same, >>= must convert InumFail
-- errors into IterFail ones).
infixr 3 `cat`

-- | Lazy cat.  Like 'cat', except that it does not run the second
-- 'Inum' if the 'Iter' is no longer active after completion of the
-- first 'Inum'.
lcat :: (ChunkData tIn, ChunkData tOut, Monad m) =>
        Inum tIn tOut m a      -- ^
     -> Inum tIn tOut m a
     -> Inum tIn tOut m a
lcat a b iter = tryI' (runInum a iter) >>= either reRunIter check
    where check r = if isIterActive r then b $ reRunIter r else return r
infixr 3 `lcat`

-- | Transforms the result of an 'Inum' into the result of the 'Iter'
-- that it contains.  Used by '|.' and '.|' to collapse their result
-- types.
joinR :: (ChunkData tIn, ChunkData tMid, Monad m) =>
         IterR tIn m (IterR tMid m a)
      -> IterR tIn m a
joinR (Done i c)       = runIterR (runR i) c
joinR (IterFail e c)   = IterFail e c
joinR (InumFail e i c) = flip onDoneR (runR i) $ \r ->
                          case r of
                            Done a _ -> InumFail e a c
                            _        -> setResid r c
joinR _                = error "joinR: not done"

-- | Fuse two 'Inum's when the output type of the first 'Inum' is the
-- same as the input type of the second.  More specifically, if
-- @inum1@ transcodes type @tIn@ to @tOut@ and @inum2@ transcodes
-- @tOut@ to @tOut2@, then @inum1 |. inum2@ produces a new 'Inum' that
-- transcodes from @tIn@ to @tOut2@.
--
-- Typically @i@ and @iR@ are types @'Iter' tOut2 m a@ and
-- @'IterR' tOut2 m a@ respectively, in which case the second
-- argument and result of @|.@ are also 'Inum's.
--
-- Has fixity:
--
-- > infixl 4 |.
(|.) :: (ChunkData tIn, ChunkData tOut, Monad m) =>
        Inum tIn tOut m iR      -- ^
     -> (i -> Iter tOut m iR)
     -> (i -> Iter tIn m iR)
(|.) outer inner iter = onDone joinR $ outer $ inner iter
infixl 4 |.

-- | Fuse an 'Inum' that transcodes @tIn@ to @tOut@ with an 'Iter'
-- taking type @tOut@ to produce an 'Iter' taking type @tIn@.  Has
-- fixity:
--
-- > infixr 4 .|
(.|) :: (ChunkData tIn, ChunkData tOut, Monad m) =>
         Inum tIn tOut m a     -- ^
      -> Iter tOut m a
      -> Iter tIn m a
(.|) inum iter = onDone joinR $ inum iter
infixr 4 .|

--
-- Exception functions
--

-- | Catches errors thrown by an 'Inum', or a set of fused 'Inum's.
-- Note that only errors in 'Inum's that are lexically within the
-- scope of the argument to 'inumCatch' will be caught.  For example:
--
-- > inumBad :: (ChunkData t, Monad m) => Inum t t m a
-- > inumBad = mkInum $ fail "inumBad"
-- > 
-- > skipError :: (ChunkData tIn, MonadIO m) =>
-- >              SomeException
-- >           -> Iter tIn m (Iter tOut m a)
-- >           -> Iter tIn m (Iter tOut m a)
-- > skipError e iter = do
-- >   liftIO $ hPutStrLn stderr $ "skipping error: " ++ show e
-- >   resumeI iter
-- >
-- > -- Throws an exception, because inumBad was fused outside the argument
-- > -- to inumCatch.
-- > test1 :: IO ()
-- > test1 = inumCatch (enumPure "test") skipError |. inumBad |$ nullI
-- > 
-- > -- Does not throw an exception, because inumBad fused within the
-- > -- argument to enumCatch.
-- > test2 :: IO ()
-- > test2 = inumCatch (enumPure "test" |. inumBad) skipError |$ nullI
-- > 
-- > -- Again no exception, because inumCatch is wrapped around inumBad.
-- > test3 :: IO ()
-- > test3 = enumPure "test" |. inumCatch inumBad skipError |$ nullI
--
-- Note that @\`inumCatch\`@ has the default infix precedence (@infixl
-- 9 \`inumcatch\`@), which binds more tightly than any concatenation
-- or fusing operators.
--
-- As noted for 'catchI', exception handlers receive both the
-- exception thrown and the failing 'Iter'.  Particularly in the case
-- of @inumCatch@, it is important to re-throw exceptions by
-- re-executing the failed 'Iter', not passing the exception itself to
-- 'throwI'.  That way, if the exception is re-caught, 'resumeI' will
-- continue to work properly.  For example, to copy two files to
-- standard output and ignore file not found errors but re-throw any
-- other kind of error, you could use the following:
--
-- @
--  resumeTest :: IO ()
--  resumeTest = doFile \"file1\" ``cat`` doFile \"file2\" |$ 'handleI' stdout
--      where
--        doFile path = inumCatch (`enumFile'` path) $ \\err iter ->
--                        if 'isDoesNotExistError' err
--                          then 'verboseResumeI' iter
--                          else iter
-- @
--
inumCatch :: (Exception e, ChunkData tIn, Monad m) =>
              Inum tIn tOut m a
           -- ^ 'Inum' that might throw an exception
           -> (e -> IterR tIn m (IterR tOut m a) -> Iter tIn m (IterR tOut m a))
           -- ^ Exception handler
           -> Inum tIn tOut m a
inumCatch enum handler iter = catchI (enum iter) check
    where check e r@(InumFail _ _ _) = handler e r
          check _ r                  = reRunIter r

-- | 'inumCatch' with the argument order switched.
inumHandler :: (Exception e, ChunkData tIn, Monad m) =>
               (e -> IterR tIn m (IterR tOut m a)
                  -> Iter tIn m (IterR tOut m a))
            -- ^ Exception handler
            -> Inum tIn tOut m a
            -- ^ 'Inum' that might throw an exception
            -> Inum tIn tOut m a
inumHandler = flip inumCatch

-- | Execute some cleanup action when an 'Inum' finishes.
inumFinally :: (ChunkData tIn, Monad m) =>
               Inum tIn tOut m a -> Iter tIn m b -> Inum tIn tOut m a
inumFinally inum cleanup iter = inum iter `finallyI` cleanup

-- | Execute some cleanup action if an 'Inum' fails.  Does not execute
-- the action if the 'Iter' (or some inner 'Inum') fails.  Has the
-- same scoping rules as 'inumCatch'.
inumOnException :: (ChunkData tIn, Monad m) =>
               Inum tIn tOut m a -> Iter tIn m b -> Inum tIn tOut m a
inumOnException inum cleanup iter = inum iter `onExceptionI` cleanup

-- | Used in an exception handler, after an 'Inum' failure, to resume
-- processing of the 'Iter' by the next enumerator in a 'cat'ed
-- series.  See 'inumCatch' for an example.
resumeI :: (ChunkData tIn, Monad m) =>
           IterR tIn m (IterR tOut m a) -> Iter tIn m (IterR tOut m a)
resumeI (InumFail _ a _) = return a
resumeI _                = error "resumeI: not InumFail"

-- | Like 'resumeI', but if the 'Iter' is resumable, also prints an
-- error message to standard error before running it.
verboseResumeI :: (ChunkData tIn, MonadIO m) =>
                  IterR tIn m (IterR tOut m a) -> Iter tIn m (IterR tOut m a)
verboseResumeI (InumFail e a _) = do
  liftIO $ do prog <- liftIO getProgName
              hPutStrLn stderr $ prog ++ ": " ++ show e
  return a
verboseResumeI _                = error "verboseResumeI: not InumFail"

--
-- Control handlers
--

type ResidHandler tIn tOut = (tIn, tOut) -> (tIn, tOut)

withResidHandler :: ResidHandler tIn tOut
                 -> Chunk tOut
                 -> (Chunk tOut -> Iter tIn mIn a)
                 -> Iter tIn mIn a
withResidHandler adjust (Chunk tOut0 eofOut) cont =
    Iter $ \(Chunk tIn0 eofIn) ->
    case adjust (tIn0, tOut0) of
      (tIn, tOut) -> runIter (cont $ Chunk tOut eofOut) $ Chunk tIn eofIn

-- | Generally the type parameter @m1@ has to be @'Iter' t m'@.  Thus,
-- a control handler maps control requests to 'IterR' results.
type CtlHandler m1 t m a = CtlArg t m a -> m1 (IterR t m a)

-- | Reject all control requests.
noCtl :: (Monad m1) => CtlHandler m1 t m a
noCtl (CtlArg _ n c) = return $ runIter (n Nothing) c

-- | Pass all control requests through to the enclosing 'Iter' monad.
-- The 'ResidHandler' argument sais how to adjust residual data, in
-- case some enclosing 'CtlHandler' decides to flush pending input
-- data, it is advisable to un-translate any data in the output type
-- @tOut@ back to the input type @tIn@.
passCtl :: (Monad mIn) =>
           ResidHandler tIn tOut
        -> CtlHandler (Iter tIn mIn) tOut m a
passCtl adj (CtlArg a n c0) = withResidHandler adj c0 runn
    where runn c = do mcr <- safeCtlI a
                      return $ runIter (n mcr) c

consCtl :: (CtlCmd carg cres) =>
           (carg -> (cres -> Iter t m a) -> Chunk t -> m1 (IterR t m a))
        -> CtlHandler m1 t m a
        -> CtlHandler m1 t m a
consCtl fn fallback ca@(CtlArg a0 n c) = maybe (fallback ca) runfn $ cast a0
    where runfn a = fn a (n . cast) c
infixr 9 `consCtl`

-- | Make a control function suitable for use as the first argument to
-- 'consCtl'.
mkCtl :: (CtlCmd carg cres, Monad m1) =>
         (carg -> m1 cres)
      -> carg -> (cres -> Iter t m a) -> Chunk t -> m1 (IterR t m a)
mkCtl f a n c = do cres <- f a; return $ runIter (n cres) c

-- | Like 'mkCtl', except that it flushes all input and clears the EOF
-- flag in both 'Iter' monads after executing the control function.
mkFlushCtl :: (CtlCmd carg cres, Monad mIn, ChunkData tIn, ChunkData t) =>
              (carg -> Iter tIn mIn cres)
           -> carg -> (cres -> Iter t m a) -> Chunk t
           -> Iter tIn mIn (IterR t m a)
mkFlushCtl f a n _ = do cres <- onDone (flip setResid mempty) $ f a
                        return $ runIter (n cres) mempty
  
--
-- Basic tools
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

-- | Run an 'Iter' just like 'runIter', but then keep executing
-- monadic actions for as long as the result is in the 'IterM' state.
-- 'Inum's should generally use this function, as it is convenient if
-- 'Inum's avoid ever returning 'IterR's in the 'IterM' state.
runIterM :: (Monad m, MonadTrans mt, Monad (mt m)) =>
            Iter t m a -> Chunk t -> mt m (IterR t m a)
runIterM iter c = check $ runIter iter c
    where check (IterM m) = lift m >>= check
          check r         = return r

{-
runIterC :: (Monad m1) =>
            CtlHandler m1 tOut m a
         -> Iter tOut m a -> Chunk tOut -> m1 (IterR tOut m a)
runIterC ch iter c = check $ runIter iter c
    where check (IterC ca) = ch ca >>= check
          check r          = return r
-}

runIterRMC :: (Monad m) =>
              CtlHandler (Iter tIn m) tOut m a
           -> IterR tOut m a -> Iter tIn m (IterR tOut m a)
runIterRMC ch = check
    where check (IterM m)  = lift m >>= check
          check (IterC ca) = ch ca >>= check
          check r          = return r

runIterMC :: (Monad m) =>
             CtlHandler (Iter tIn m) tOut m a
          -> Iter tOut m a -> Chunk tOut -> Iter tIn m (IterR tOut m a)
runIterMC ch iter c = runIterRMC ch $ runIter iter c

-- | Takes an 'Inum' that might return 'IterR's in the 'IterM' state
-- (which is considered impolite--see 'runIterM') and transforms it
-- into an 'Inum' that never returns 'IterR's in the 'IterM' state.
runInum :: (ChunkData tIn, Monad m) =>
           Inum tIn tOut m a -> Inum tIn tOut m a
runInum inum = onDone check . inum
    where
      check (Done (IterM m) c) = IterM $ m >>= \r -> return $ check $ Done r c
      check r = r

-- | Simple (stateless) 'Inum' creation.  Create an 'Inum' given a
-- function to adjust residual data and an 'Iter' that transcodes from
-- the input to the output type.
mkInum :: (ChunkData tIn, ChunkData tOut, Monad m) =>
          ResidHandler tIn tOut
       -- ^ Adjust residual data (use 'id' for no adjustment)
       -> CtlHandler (Iter tIn m) tOut m a
       -- ^ Handle control requests (use 'noCtl' or 'passCtl' if
       -- 'Inum' shouldn't implement any specific control functions).
       -> Iter tIn m tOut
       -- ^ Generate transcoded data chunks
       -> Inum tIn tOut m a
mkInum adj ch codec iter0 = doIter iter0
    where
      doIter iter = tryI codec >>= either (inputErr iter . fst) (doInput iter)
      inputErr iter e | isIterEOF e = return $ IterF iter
                      | otherwise   = Iter $ InumFail e (IterF iter)
      doInput iter input = do
        r <- runIterMC ch iter (Chunk input False)
        stop <- knownEOFI
        case (stop, r) of
          (False, IterF i) -> doIter i
          (_, r1) | isIterActive r1 -> return r1
          _ -> withResidHandler adj (getResid r) $ return . setResid r

pullupResid :: (ChunkData t) => (t, t) -> (t, t)
pullupResid (a, b) = (mappend a b, mempty)

--
-- Basic Inums
--

-- | @inumNop@ passes all data through to the underlying 'Iter'.  It
-- acts as a no-op when fused to other 'Inum's with '|.' or fused to
-- 'Iter's with '.|'.
inumNop :: (ChunkData t, Monad m) => Inum t t m a
inumNop = mkInum pullupResid (passCtl pullupResid) dataI

-- | @inumNull@ feeds empty data to the underlying 'Iter'.  It acts as
-- a no-op when concatenated to other 'Inum's with 'cat' or 'lcat'.
inumNull :: (ChunkData tIn, ChunkData tOut, Monad m) => Inum tIn tOut m a
inumNull = inumPure mempty

-- | Feed pure data to an 'Iter'.
inumPure :: (ChunkData tIn, Monad m) => tOut -> Inum tIn tOut m a
inumPure t iter = runIterM iter $ chunk t

-- | Repeat an 'Inum' until the input receives an EOF condition, the
-- 'Iter' no longer requires input, or the 'Iter' is in an unhandled
-- 'IterC' state (which presumably will continue to be unhandled by
-- the same inum, so no point in executing it again).
inumRepeat :: (ChunkData tIn, Monad m) =>
              Inum tIn tOut m a -> Inum tIn tOut m a
inumRepeat inum iter0 = do
  er <- tryI' $ runInum inum iter0
  stop <- atEOFI
  case (stop, er) of
    (False, Right (IterF iter)) -> inumRepeat inum iter
    (_, Right r) -> return r
    (_, Left r) -> reRunIter r

--
-- Complex Inum creation
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
@'Iter' tIn ('IterStateT' (InumState ...) m) ()@.  That means that to
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
    , insCtl :: !(CtlHandler (Iter tIn m) tOut m a)
    , insIter :: !(IterR tOut m a)
    , insCleanup :: !(InumM tIn tOut m a ())
    , insCleaning :: !Bool
    }

defaultInumState :: (ChunkData tIn, Monad m) => InumState tIn tOut m a
defaultInumState = InumState {
                     insAutoEOF = False
                   , insAutoDone = False
                   , insCtl = noCtl
                   , insIter = IterF $ Iter $ const $ error "insIter"
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
                 CtlHandler (Iter tIn m) tOut m a
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
         -> Iter tIn m (IterR tOut m a)
runInumM inumm s0 = do
  (result1, s1) <- runIterStateT inumm s0 >>= convertFail
  (result2, s2) <- runIterStateT (insCleanup s1) s1 { insAutoDone = False
                                                    , insCleaning = True }
                   >>= convertFail
  let iter = insIter s2
  case (result1, result2) of
    (IterFail e _, _) -> Iter $ InumFail e iter
    (_, IterFail e _) -> Iter $ InumFail e iter
    _                 -> return iter
    where
      convertFail (InumFail e _ c, s) = convertFail (IterFail e c, s)
      convertFail (iter@(IterFail e _), s) =
          if isInumDone e || (insAutoEOF s && isIterEOF e)
          then return (IterF $ Iter $ const $ error "runInumM", s)
          else return (iter, s)
      convertFail is = return is
      isInumDone e = maybe False (\InumDone -> True) $ fromException e


-- | A variant of 'mkInumM' that sets /AutoEOF/ and /AutoDone/ to
-- 'True' by default.  (Equivalent to calling @'setAutoEOF' 'True' >>
-- 'setAutoDone' 'True'@ as the first thing inside 'mkInumM'.)
mkInumAutoM :: (ChunkData tIn, ChunkData tOut, Monad m) =>
               InumM tIn tOut m a b -> Inum tIn tOut m a
mkInumAutoM inumm iter0 =
    runInumM inumm defaultInumState { insIter = IterF iter0
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
    runInumM inumm defaultInumState { insIter = IterF iter0 }

-- | Used from within the 'InumM' monad to feed data to the target
-- 'Iter'.  Returns @'False'@ if the target 'Iter' is still active and
-- @'True'@ if the iter has finished and the 'Inum' should also
-- return.  (If the @autoDone@ flag is @'True'@, then @ifeed@,
-- @ipipe@, and @irun@ will never actually return @'True'@, but
-- instead just immediately run cleanup functions and exit the
-- 'Inum' when the target 'Iter' stops being active.)
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
ifeed1 dat = if null dat then throwEOFI "ifeed1" else ifeed dat

-- | Apply another 'Inum' to the target 'Iter' from within the 'InumM'
-- monad.  As with 'ifeed', returns @'True'@ when the 'Iter' is
-- finished.
--
-- Note that the applied 'Inum' must handle all control requests.  (In
-- other words, ones it passes on are not caught by whatever handler
-- is installed by 'setCtlHandler', but if the 'Inum' returns the
-- 'IterR' in the 'IterC' state, as 'inumPure' does, then requests
-- will be handled.)
ipipe :: (ChunkData tIn, ChunkData tOut, Monad m) =>
         Inum tIn tOut m a -> InumM tIn tOut m a Bool
ipipe inum = do
  s <- iget
  r <- liftIterM (inum $ reRunIter $ insIter s) `catchI` reThrow
       >>= liftIterM . runIterRMC (insCtl s)
  iput s { insIter = r }
  let done = not $ isIterActive r
  if done && insAutoDone s then idone else return done
    where
      reThrow (SomeException _) r@(InumFail _ i _) = do
               imodify $ \s -> s { insIter = i }
               reRunIter r
      reThrow _ r = reRunIter r

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
