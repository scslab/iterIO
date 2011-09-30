{-# LANGUAGE DeriveDataTypeable #-}

module Data.IterIO.Inum
    (-- * Base types
     Inum, Onum
    -- * Concatenation and fusing operators
    , (|$), (.|$), cat, lcat, (|.), (.|)
    -- * Exception functions
    , inumCatch, inumFinally, inumOnException
    , resumeI, verboseResumeI
    -- * Simple enumerator construction function
    -- $mkInumIntro
    , ResidHandler, CtlHandler
    , mkInumC, mkInum, mkInumP
    , inumBracket
    -- * Utilities
    , pullupResid
    , noCtl, passCtl, consCtl, mkCtl, mkFlushCtl
    , runIterM, runIterMC, runInum
    -- * Some basic Inums
    , inumNop, inumNull, inumPure, enumPure, inumRepeat
    , inumTee
    -- * Enumerator construction monad
    -- $mkInumMIntro
    , InumM, mkInumM, mkInumAutoM
    , setCtlHandler, setAutoEOF, setAutoDone, addCleanup, withCleanup
    , ifeed, ifeed1, ipipe, irun, irepeat, ipopresid, idone
    ) where

import Prelude hiding (null)
import Control.Exception (Exception(..))
import Control.Monad
import Control.Monad.Trans
import Data.Maybe
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
-- 'Iter's to 'IterR's, where an @Inum@'s input and output types are
-- different.  A simpler-seeming alternative to @Inum@ might have
-- been:
--
-- > type Inum' tIn tOut m a = Iter tOut m a -> Iter tIn m a
--
-- In fact, given an @Inum@ object @inum@, it is possible to construct
-- a function of type @Inum'@ with @(inum '.|')@.  But sometimes one
-- might like to concatenate @Inum@s.  For instance, consider a
-- network protocol that changes encryption or compression modes
-- midstream.  Transcoding is done by @Inum@s.  To change transcoding
-- methods after applying an @Inum@ to an iteratee requires the
-- ability to \"pop\" the iteratee back out of the @Inum@ so as to be
-- able to hand it to another @Inum@.  @Inum@'s return type (@Iter tIn
-- m (IterR tOut m a)@ as opposed to @Iter tIn m a@) allows the
-- monadic bind operator '>>=' to accomplish this popping in
-- conjunction with the 'tryRI' and 'reRunIter' functions.
--
-- All @Inum@s must obey the following two rules.
--
-- 1. /An/ @Inum@ /may never feed a chunk with the EOF flag set to/
--    /it's target/ 'Iter'. Instead, upon receiving EOF, the @Inum@
--    should simply return the state of the inner 'Iter' (this is how
--    \"popping\" the iteratee back out works--If the @Inum@ passed
--    the EOF through to the 'Iter', the 'Iter' would stop requesting
--    more input and could not be handed off to a new @Inum@).
--
-- 2. /An/ @Inum@ /must always return the state of its target/ 'Iter'.
--    This is true even when the @Inum@ fails, and is why the 'Fail'
--    state contains a @'Maybe' a@ field.
--
-- In addition to returning when it receives an EOF or fails, an
-- @Inum@ should return when the target 'Iter' returns a result or
-- fails.  An @Inum@ may also unilaterally return the state of the
-- iteratee at any earlier point, for instance if it has reached some
-- logical message boundary (e.g., many protocols finish processing
-- headers upon reading a blank line).
--
-- @Inum@s are generally constructed with one of the 'mkInum' or
-- 'mkInumM' functions, which hide most of the error handling details
-- and ensure the above rules are obeyed.  Most @Inum@s are
-- polymorphic in the last type, @a@, in order to work with iteratees
-- returning any type.  There isn't much reason for an @Inum@ to care
-- about the type @a@.  Had this module used the Rank2Types Haskell
-- extension, it would define @Inum@ as:
--
-- > type Inum tIn tOut m = forall a. Iter tOut m a
-- >                               -> Iter tIn m (IterR tOut m a)
type Inum tIn tOut m a = Iter tOut m a -> Iter tIn m (IterR tOut m a)

-- | An @Onum t m a@ is just an 'Inum' in which the input is
-- @()@--i.e., @'Inum' () t m a@--so that there is no meaningful input
-- data to transcode.  Such an enumerator is called an
-- /outer enumerator/, because it must produce the data it feeds to
-- 'Iter's by either executing actions in monad @m@, or from its own
-- internal pure state (as for 'enumPure').
--
-- As with 'Inum's, an @Onum@ should under no circumstances ever feed
-- a chunk with the EOF bit set to its 'Iter' argument.  When the
-- @Onum@ runs out of data, it must simply return the current state of
-- the 'Iter'.  This way more data from another source can still be
-- fed to the iteratee, as happens when enumerators are concatenated
-- with the 'cat' function.
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
-- Note the important distinction between @(.|$)@ and @('.|')@.
-- @(.|$)@ runs an 'Onum' and does not touch the current input, while
-- ('.|') pipes the current input through an 'Inum'.  For instance, to
-- send the contents of a file to standard output (regardless of the
-- current input), you must say @'enumFile' \".signature\" .|$
-- 'stdoutI'@.  But to take the current input, compress it, and send
-- the result to standard output, you must use '.|', as in @'inumGzip'
-- '.|' 'stdoutI'@.
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
-- \"file2\".  Unless the first 'Inum' fails, @cat@ always invokes the
-- second 'Inum', as the second 'Inum' may have monadic side-effects
-- that must be executed even when the 'Iter' has already finished.
-- See 'lcat' if you want to stop when the 'Iter' no longer requires
-- input.  If you want to continue executing even in the event of an
-- 'InumFail' condition, you can wrap the first 'Inum' with
-- 'inumCatch' and invoke 'resumeI' from within the exception handler.
--
-- @cat@ (and 'lcat', described below) are useful in right folds.
-- Say, for instance, that @files@ is a list of files you wish to
-- concatenate.  You can use a construct such as:
--
-- @
--  catFiles :: ('MonadIO' m) => ['FilePath'] -> 'Onum' 'L.ByteString' m a
--  catFiles files = 'foldr' ('cat' . 'enumFile') 'inumNull' files
-- @
--
-- Note the use of 'inumNull' as the starting value for 'foldr'.  This
-- is not to be confused with 'inumNop'.  'inumNull' acts as a no-op
-- for concatentation, producing no output analogously to
-- @\/dev\/null@.  By contrast 'inumNop' is the no-op for fusing (see
-- '|.' and '.|' below) because it passes all data through untouched.
--
-- @cat@ has fixity:
--
-- > infixr 3 `cat`
cat :: (ChunkData tIn, ChunkData tOut, Monad m) =>
       Inum tIn tOut m a      -- ^
    -> Inum tIn tOut m a
    -> Inum tIn tOut m a
cat a b iter = tryRI (runInum a iter) >>= either reRunIter (b . reRunIter)
-- Note this was carefully constructed to preserve the return value in
-- errors.  Something like:  cat a b iter = a iter >>= b . reRunIter
-- would turn a @('Fail' e ('Just' r) c)@ result from @a@ into
-- @('Fail' e 'Nothing' c)@; since the input and output types of >>=
-- do not have to be the same, >>= must convert error results to
-- 'Nothing'.
infixr 3 `cat`

-- | Lazy cat.  Like 'cat', except that it does not run the second
-- 'Inum' if the 'Iter' is no longer active after completion of the
-- first 'Inum'.  Also has fixity @infixr 3 \`lcat\`@.
lcat :: (ChunkData tIn, ChunkData tOut, Monad m) =>
        Inum tIn tOut m a      -- ^
     -> Inum tIn tOut m a
     -> Inum tIn tOut m a
lcat a b iter = tryRI (runInum a iter) >>= either reRunIter check
    where check r = if isIterActive r then b $ reRunIter r else return r
infixr 3 `lcat`

-- | Transforms the result of an 'Inum' into the result of the 'Iter'
-- that it contains.  Used by '|.' and '.|' to collapse their result
-- types.
--
-- Note that because the input type if the inner 'Iter', @tMid@, gets
-- squeezed out of the return type, @joinR@ will feed an EOF to the
-- inner 'Iter' if it is still active.  This is what ensures that
-- active 'Iter's end up seeing an EOF, even though 'Inum's themselves
-- are never supposed to feed an EOF to the underlying 'Iter'.  All
-- 'Iter's in right-hand arguments of '.|' and '|.' get fed an EOF by
-- @joinR@ (if they don't finish on their own), while the outermost
-- 'Inum' is fed an iter by the 'run' function (or by '|$' which
-- invokes 'run' internally).
joinR :: (ChunkData tIn, ChunkData tMid, Monad m) =>
         IterR tIn m (IterR tMid m a)
      -> IterR tIn m a
joinR (Done i c)          = runIterR (runR i) c
joinR (Fail e Nothing c)  = Fail e Nothing c
--
-- Note that 'runR' in the following function is serving two purposes,
-- one of them subtle.  The obvious purpose is to preserve the state
-- of the non-failed target 'Iter' when an 'Inum' has failed.
-- However, a subtler, more important purpose is to guarantee that all
-- (non-failed) 'Iter's eventually receive EOF even when 'Inum's fail.
-- This is critical for things like EOF transmission and file
-- descriptor closing, and is how functions such as 'pairFinalizer'
-- can make sense.
joinR (Fail e (Just i) c) = flip onDoneR (runR i) $ \r ->
                            case r of
                              Done a _    -> Fail e (Just a) c
                              Fail e' a _ -> Fail e' a c
                              _ -> error "joinR"
joinR _                   = error "joinR: not done"

-- | Left-associative pipe operator.  Fuses two 'Inum's when the
-- output type of the first 'Inum' is the same as the input type of
-- the second.  More specifically, if @inum1@ transcodes type @tIn@ to
-- @tOut@ and @inum2@ transcodes @tOut@ to @tOut2@, then @inum1
-- |. inum2@ produces a new 'Inum' that transcodes from @tIn@ to
-- @tOut2@.
--
-- Typically types @i@ and @iR@ are @'Iter' tOut2 m a@ and @'IterR'
-- tOut2 m a@, respectively, in which case the second argument and
-- result of @|.@ are also 'Inum's.
--
-- This function is equivalent to:
--
-- @
--  outer |. inner = \\iter -> outer '.|' inner iter
--  infixl 4 |.
-- @
--
-- But if you like point-free notation, think of it as @outer |. inner
-- = (outer '.|') . inner@, or better yet @(|.) = (.)  . ('.|')@.
(|.) :: (ChunkData tIn, ChunkData tOut, Monad m) =>
        Inum tIn tOut m iR      -- ^
     -> (i -> Iter tOut m iR)
     -> (i -> Iter tIn m iR)
(|.) outer inner = \iter -> onDone joinR $ outer $ inner iter
infixl 4 |.

-- | Right-associative pipe operator.  Fuses an 'Inum' that transcodes
-- @tIn@ to @tOut@ with an 'Iter' taking input type @tOut@ to produce
-- an 'Iter' taking input type @tIn@.  If the 'Iter' is still active
-- when the 'Inum' terminates (either normally or through an
-- exception), then @.|@ sends it an EOF.
--
--  Has fixity:
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
-- >           -> IterR tIn m (IterR tOut m a)
-- >           -> Iter tIn m (IterR tOut m a)
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
-- > -- argument to inumCatch.
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
-- exception thrown and the failed 'IterR'.  Particularly in the case
-- of @inumCatch@, it is important to re-throw exceptions by
-- re-executing the failed 'Iter' with 'reRunIter', not passing the
-- exception itself to 'throwI'.  That way, if the exception is
-- re-caught, 'resumeI' will continue to work properly.  For example,
-- to copy two files to standard output and ignore file not found
-- errors but re-throw any other kind of error, you could use the
-- following:
--
-- @
--  resumeTest :: IO ()
--  resumeTest = doFile \"file1\" ``cat`` doFile \"file2\" |$ 'stdoutI'
--      where
--        doFile path = inumCatch (`enumFile'` path) $ \\err r ->
--                        if 'isDoesNotExistError' err
--                          then 'verboseResumeI' r
--                          else 'reRunIter' r
-- @
--
inumCatch :: (Exception e, ChunkData tIn, Monad m) =>
              Inum tIn tOut m a
           -- ^ 'Inum' that might throw an exception
           -> (e -> IterR tIn m (IterR tOut m a) -> Iter tIn m (IterR tOut m a))
           -- ^ Exception handler
           -> Inum tIn tOut m a
inumCatch enum handler iter = catchI (enum iter) check
    where check e r@(Fail _ (Just _) _) = handler e r
          check _ r                     = reRunIter r

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
resumeI (Fail _ (Just a) _) = return a
resumeI _                   = error "resumeI: not an Inum failure"

-- | Like 'resumeI', but if the 'Iter' is resumable, also prints an
-- error message to standard error before resuming.
verboseResumeI :: (ChunkData tIn, MonadIO m) =>
                  IterR tIn m (IterR tOut m a) -> Iter tIn m (IterR tOut m a)
verboseResumeI (Fail e (Just a) _) = do
  liftIO $ do prog <- liftIO getProgName
              hPutStrLn stderr $ prog ++ ": " ++ show e
  return a
verboseResumeI _ = error "verboseResumeI: not an Inum failure"

--
-- Control handlers
--

-- | A @ResidHandler@ specifies how to handle residual data in an
-- 'Inum'.  Typically, when an 'Inum' finishes executing, there are
-- two kinds of residual data.  First, the 'Inum' itself (in its role
-- as an iteratee) may have left some unconsumed data.  Second, the
-- target 'Iter' being fed by the 'Inum' may have some resitual data,
-- and this data may be of a different type.  A @ResidHandler@ allows
-- this residual data to be adjusted by untranslating the residual
-- data of the target 'Iter' and sticking the result back into the
-- `Inum`'s residual data.
--
-- The two most common @ResidHandler@s are 'pullupResid' (to pull the
-- target `Iter`'s residual data back up to the 'Inum' as is), and
-- 'id' (to do no adjustment of residual data).
--
-- @ResidHandler@s are used by the 'mkInumC' function, and by the
-- 'passCtl' 'CtlHandler'.
type ResidHandler tIn tOut = (tIn, tOut) -> (tIn, tOut)

withResidHandler :: ResidHandler tIn tOut
                 -> Chunk tOut
                 -> (Chunk tOut -> Iter tIn mIn a)
                 -> Iter tIn mIn a
withResidHandler adjust (Chunk tOut0 eofOut) cont =
    Iter $ \(Chunk tIn0 eofIn) ->
    case adjust (tIn0, tOut0) of
      (tIn, tOut) -> runIter (cont $ Chunk tOut eofOut) $ Chunk tIn eofIn

-- | A control handler maps control requests to 'IterR' results.
-- Generally the type parameter @m1@ is @'Iter' t' m@.
type CtlHandler m1 t m a = CtlArg t m a -> m1 (IterR t m a)

-- | Reject all control requests.
noCtl :: (Monad m1) => CtlHandler m1 t m a
noCtl (CtlArg _ n c) = return $ runIter (n CtlUnsupp) c

-- | Pass all control requests through to the enclosing 'Iter' monad.
-- The 'ResidHandler' argument says how to adjust residual data, in
-- case some enclosing 'CtlHandler' decides to flush pending input
-- data, it is advisable to un-translate any data in the output type
-- @tOut@ back to the input type @tIn@.
passCtl :: (Monad mIn) =>
           ResidHandler tIn tOut
        -> CtlHandler (Iter tIn mIn) tOut m a
passCtl adj (CtlArg a n c0) = withResidHandler adj c0 runn
    where runn c = do mcr <- safeCtlI a
                      return $ runIter (n mcr) c

-- | Create a 'CtlHandler' given a function of a particular control
-- argument type and a fallback 'CtlHandler' to run if the argument
-- type does not match.  @consCtl@ is used to chain handlers, with the
-- rightmost handler being either 'noCtl' or 'passCtl'.
--
-- For example, to create a control handler that implements seek on
-- @'SeekC'@ requests, returns the size of the file on @'SizeC'@
-- requests, and passes everything else out to the enclosing
-- enumerator (if any), you could use the following:
--
-- @
-- fileCtl :: (ChunkData t, MonadIO m) => Handle -> CtlHandler (Iter () m) t m a
-- fileCtl h = ('mkFlushCtl' $ \(SeekC mode pos) -> liftIO (hSeek h mode pos))
--             \`consCtl\` ('mkCtl' $ \SizeC -> liftIO (hFileSize h))
--             \`consCtl\` 'passCtl' 'id'
-- @
--
-- Has fixity:
--
-- > infixr 9 `consCtl`
consCtl :: (CtlCmd carg cres, ChunkData tIn, Monad mIn) =>
           (carg -> (cres -> Iter t m a) -> Chunk t
                 -> Iter tIn mIn (IterR t m a))
        -> CtlHandler (Iter tIn mIn) t m a
        -> CtlHandler (Iter tIn mIn) t m a
consCtl fn fallback ca@(CtlArg a0 n c) = maybe (fallback ca) runfn $ cast a0
    where runfn a = fn a (n . CtlDone . fromJust . cast) c
                    `catchI` \e _ -> return $ runIter (n $ CtlFail e) c
infixr 9 `consCtl`

-- | Make a control function suitable for use as the first argument to
-- 'consCtl'.
mkCtl :: (CtlCmd carg cres, Monad m1) =>
         (carg -> Iter t1 m1 cres)
      -> carg -> (cres -> Iter t m a) -> Chunk t -> Iter t1 m1 (IterR t m a)
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

-- | Like 'runIterMC', but only for 'IterM'--may return 'IterC'.
runIterM :: (Monad m, MonadTrans mt, Monad (mt m)) =>
            Iter t m a -> Chunk t -> mt m (IterR t m a)
runIterM iter c = check $ runIter iter c
    where check (IterM m) = lift m >>= check
          check r         = return r

runIterRMC :: (Monad m) =>
              CtlHandler (Iter tIn m) tOut m a
           -> IterR tOut m a -> Iter tIn m (IterR tOut m a)
runIterRMC ch = check
    where check (IterM m)  = lift m >>= check
          check (IterC ca) = ch ca >>= check
          check r          = return r

-- | Run an 'Iter' just like 'runIter', but then keep stepping the
-- result for as long as it is in the 'IterM' or 'IterC' state (using
-- the supplied 'CtlHandler' for 'IterC' states).  'Inum's should
-- generally use this function or 'runIterM' in preference to
-- 'runIter', as it is convenient if 'Inum's avoid ever returning
-- 'IterR's in the 'IterM' state.
runIterMC :: (Monad m) =>
             CtlHandler (Iter tIn m) tOut m a
          -> Iter tOut m a -> Chunk tOut -> Iter tIn m (IterR tOut m a)
runIterMC ch iter c = runIterRMC ch $ runIter iter c

-- | Takes an 'Inum' that might return 'IterR's in the 'IterM' state
-- (which is considered impolite--see 'runIterMC') and transforms it
-- into an 'Inum' that never returns 'IterR's in the 'IterM' state.
runInum :: (ChunkData tIn, Monad m) =>
           Inum tIn tOut m a -> Inum tIn tOut m a
runInum inum = onDone check . inum
    where
      check (Done (IterM m) c) = IterM $ m >>= \r -> return $ check $ Done r c
      check r = r

-- | Create a stateless 'Inum' from a \"codec\" 'Iter' that transcodes
-- the input type to the output type.  The codec is invoked repeately
-- until one of the following occurs:
--
--   1. The input is at an EOF marker AND the codec returns 'null'
--      data.  ('Onum's are always fed EOF, but other 'Inum's might
--      have reason to return 'mempty' data.)
--
--   2. The codec throws an exception.  If the exception is an EOF
--      exception--thrown either by 'throwEOFI', or by some IO action
--      inside 'liftIO'--this is considered normal termination, and is
--      the normal way for a codec to cause the 'Inum' to return.  If
--      the exception is of any other type, then the 'Inum' will
--      further propagate the exception as an 'Inum' failure.
--
--   3. The underlying target 'Iter' either returns a result or throws
--      an exception.
--
-- @mkInumC@ requires two other arguments before the codec.  First, a
-- 'ResidHandler' allows residual data to be adjusted between the
-- input and output 'Iter' monads.  Second, a 'CtlHandler' specifies a
-- handler for control requests.  For example, to pass up control
-- requests and ensure no residual data is lost when the 'Inum' is
-- fused to an 'Iter', the @inumConcat@ function given previously for
-- 'mkInum' at <#mkInumExample> could be re-written:
--
-- > inumConcat :: (Monad m) => Inum [L.ByteString] L.ByteString m a
-- > inumConcat = mkInumC reList (passCtl reList) iterConcat
-- >     where reList (a, b) = (b:a, mempty)
mkInumC :: (ChunkData tIn, ChunkData tOut, Monad m) =>
           ResidHandler tIn tOut
        -- ^ Adjust residual data (use 'id' for no adjustment)
        -> CtlHandler (Iter tIn m) tOut m a
        -- ^ Handle control requests (use 'noCtl' or 'passCtl' if
        -- 'Inum' shouldn't implement any specific control functions).
        -> Iter tIn m tOut
        -- ^ Generate transcoded data chunks
        -> Inum tIn tOut m a
mkInumC adj ch codec iter0 = doIter iter0
    where
      doIter iter = tryEOFI codec >>= maybe (return $ IterF iter) (doInput iter)
      doInput iter input = do
        r <- runIterMC ch iter (Chunk input False)
        eof <- Iter $ \c@(Chunk t eof) -> Done (eof && null t) c
        case r of
          (IterF i) | not (eof && null input) -> doIter i
          _ | isIterActive r -> return r
          _ -> withResidHandler adj (getResid r) $ return . setResid r

-- | Create an 'Inum' based on an 'Iter' that transcodes the input to
-- the output type.  This is a simplified version of 'mkInumC' that
-- rejects all control requests and does not adjust residual data.
--
-- > mkInum = mkInumC id noCtl
mkInum :: (ChunkData tIn, ChunkData tOut, Monad m) =>
          Iter tIn m tOut -> Inum tIn tOut m a
mkInum = mkInumC id noCtl

-- | A simplified version of 'mkInum' that passes all control requests
-- to enclosing enumerators.  It requires a 'ResidHandler' to describe
-- how to adjust residual data.  (E.g., use 'pullupResid' when @tIn@
-- and @tOut@ are the same type.)
--
-- > mkInumP adj = mkInumC adj (passCtl adj)
mkInumP :: (ChunkData tIn, ChunkData tOut, Monad m) =>
           ResidHandler tIn tOut -> Iter tIn m tOut -> Inum tIn tOut m a
mkInumP adj = mkInumC adj (passCtl adj)

-- | @pullupResid (a, b) = (mappend a b, mempty)@.  See 'ResidHandler'.
pullupResid :: (ChunkData t) => (t, t) -> (t, t)
pullupResid (a, b) = (mappend a b, mempty)

-- | Bracket an 'Inum' with a start and end function, which can be
-- used to acquire and release a resource, must like the IO monad's
-- @'bracket'@ function.  For example:
--
-- > enumFile :: (MonadIO m, ChunkData t, LL.ListLikeIO t e) =>
-- >             FilePath -> Onum t m a
-- > enumFile path = inumBracket (liftIO $ openBinaryFile path ReadMode)
-- >                             (liftIO . hClose)
-- >                             enumHandle
inumBracket :: (ChunkData tIn, Monad m) =>
               Iter tIn m b
            -- ^ Computation to run first
            -> (b -> Iter tIn m c)
            -- ^ Computation to run last
            -> (b -> Inum tIn tOut m a)
            -- ^ Inum to bracket
            -> Inum tIn tOut m a
inumBracket start end inum iter = tryFI start >>= check
    where check (Left e)  = Iter $ Fail e (Just $ IterF iter) . Just
          check (Right b) = inum b iter `finallyI` end b

--
-- Basic Inums
--

-- | @inumNop@ passes all data through to the underlying 'Iter'.  It
-- acts as a no-op when fused to other 'Inum's with '|.' or when fused
-- to 'Iter's with '.|'.
--
-- @inumNop@ is particularly useful for conditionally fusing 'Inum's
-- together.  Even though most 'Inum's are polymorphic in the return
-- type, this library does not use the Rank2Types extension, which
-- means any given 'Inum' must have a specific return type.  Here is
-- an example of incorrect code:
--
-- @
-- let enum = if debug then base_enum '|.' 'inumStderr' else base_enum -- Error
-- @
--
-- This doesn't work because @base_enum@ cannot have the same type as
-- @(base_enum |. inumStderr)@.  Instead, you can use the following:
--
-- @
-- let enum = base_enum '|.' if debug then 'inumStderr' else inumNop
-- @
inumNop :: (ChunkData t, Monad m) => Inum t t m a
inumNop = mkInumP pullupResid dataI

-- | @inumNull@ feeds empty data to the underlying 'Iter'.  It pretty
-- much acts as a no-op when concatenated to other 'Inum's with 'cat'
-- or 'lcat'.
--
-- There may be cases where @inumNull@ is required to avoid deadlock.
-- In an expression such as @enum '|$' iter@, if @enum@ immediately
-- blocks waiting for some event, and @iter@ immediately starts out
-- triggering that event before reading any input, then to break the
-- deadlock you can re-write the code as @cat inumNull enum '|$'
-- iter@.
inumNull :: (ChunkData tOut, Monad m) => Inum tIn tOut m a
inumNull = inumPure mempty

-- | Feed pure data to an 'Iter'.
inumPure :: (Monad m) => tOut -> Inum tIn tOut m a
inumPure t iter = runIterM iter $ chunk t

-- | Type-restricted version of 'inumPure'.
enumPure :: (Monad m) => tOut -> Onum tOut m a
enumPure = inumPure

-- | Repeat an 'Inum' until the input receives an EOF condition, the
-- 'Iter' no longer requires input, or the 'Iter' is in an unhandled
-- 'IterC' state (which presumably will continue to be unhandled by
-- the same 'Inum', so no point in executing it again).
inumRepeat :: (ChunkData tIn, Monad m) =>
              Inum tIn tOut m a -> Inum tIn tOut m a
inumRepeat inum iter0 = do
  er <- tryRI $ runInum inum iter0
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
@m@, you have to transform them with the 'liftI' function.

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
functions from 'Iter's to 'IterR's; we want to apply 'runI' to the
result of applying @'enumFile' \".signature\"@ to an 'Iter'.  Spelled
out, the type of @'enumFile'@ is:

@
enumFile :: (MonadIO m, ChunkData t, ListLikeIO t e) =>
            FilePath
         -> 'Iter' t m a
         -> 'Iter' () m a ('IterR' t m a)
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
-- @'lift'@ twice to execute actions in monad @m@, and you must use
-- the 'liftI' function to execute actions in monad @'Iter' t m a@.
type InumM tIn tOut m a = Iter tIn (IterStateT (InumState tIn tOut m a) m)

-- | Set the control handler an 'Inum' should use from within an
-- 'InumM' computation.  (The default is 'noCtl'.)
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
  (err1, s1) <- getErr =<< runIterStateT inumm s0
  (err2, s2) <- getErr =<< runIterStateT (insCleanup s1)
                                 s1 { insAutoDone = False, insCleaning = True }
  let r = insIter s2
  Iter $ maybe (Done r) (\e -> Fail e (Just r) . Just) $ mplus err2 err1
    where
      getErr (Fail (IterEOFErr _) _ _, s) | insAutoEOF s = return (Nothing, s)
      getErr (Fail e _ _, s)                             = return (Just e, s)
      getErr (_, s)                                      = return (Nothing, s)

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
--    will have to lift them with the 'liftI' function.
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
-- For instance, the main loop of @'enumFile'@ could be implemented
-- as:
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
  r <- tryRI (liftI (inum $ reRunIter $ insIter s)) >>= getIter
       >>= liftI . runIterRMC (insCtl s)
  iput s { insIter = r }
  let done = not $ isIterActive r
  if done && insAutoDone s then idone else return done
    where
      getIter (Right i) = return i
      getIter (Left r@(Fail _ (Just i) _)) = do
               imodify $ \s -> s { insIter = i }
               reRunIter r
      getIter (Left r) = reRunIter r

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

-- | If the target 'Iter' being fed by the 'Inum' is no longer active
-- (i.e., if it is in the 'Done' state or in an error state), this
-- funciton pops the residual data out of the 'Iter' and returns it.
-- If the target is in any other state, returns 'mempty'.
ipopresid :: (ChunkData tIn, ChunkData tOut, Monad m) =>
             InumM tIn tOut m a tOut
ipopresid = do
  s <- iget
  case insIter s of
    r | isIterActive r -> return mempty
      | otherwise      -> do let (Chunk t _) = getResid r
                             iput s { insIter = setResid r mempty }
                             return t

-- | Immediately perform a successful exit from an 'InumM' monad,
-- terminating the 'Inum' and returning the current state of the
-- target 'Iter'.  Can be used to end an 'irepeat' loop.  (Use
-- @'throwI' ...@ for an unsuccessful exit.)
idone :: (ChunkData tIn, Monad m) => InumM tIn tOut m a b
idone = setAutoEOF True >> throwEOFI "idone"

-- | An 'Inum' that acts like 'inumNop', except that before passing
-- data on, it feeds a copy to a \"tee\" 'Iter' (by analogy with the
-- Unix @tee@ utility), which may, for instance, transform and log the
-- data.
--
-- The tee `Iter`'s return value is ignored.  If the tee 'Iter'
-- returns before an EOF is received and before the target 'Iter' has
-- finished processing input, then @inumTee@ will continue to pass
-- data to the target 'Iter'.  However, if the tee 'Iter' fails, then
-- this will cause @inumTee@ to fail immediately.
--
-- As an example, one could implement @'inumStderr'@ (from
-- "Data.IterIO.ListLike") as:
--
-- > inumStderr = inumTee $ handleI stderr
--
inumTee :: (ChunkData t, Monad m) =>
           Iter t m b -> Inum t t m a
inumTee tee0 = mkInumM $ setCtlHandler (passCtl pullupResid) >> loop tee0
    where
      loop tee = do
        c <- Iter $ \c'@(Chunk _ eof) -> Done c' (Chunk mempty eof)
        liftI (runIterMC (passCtl pullupResid) tee c) >>= feed c
      feed (Chunk d False) (IterF tee) = do
        done <- ifeed d `onExceptionI` liftI (runI tee)
        if done then liftI (runI tee) >> return () else loop tee
      feed (Chunk d True) (IterF _) = ifeed d >> return ()
      feed _ (Fail r _ _) = Iter $ Fail r Nothing . Just
      feed (Chunk d eof) (Done _ _) = do
        done <- ifeed d
        unless (done || eof) $ ipipe inumNop >> return ()
      feed _ _ = error "inumTee"
