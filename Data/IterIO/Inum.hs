{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module Data.IterIO.Inum
{-
    (-- * Base types
     Inum, Onum
    -- * concatenation and fusing operators
    , (|$), (.|$), cat, (|.), (.|)
    -- * Exception functions
    , inumCatch, inumHandler, inumFinally, inumOnException
    , resumeI, verboseResumeI
    -- * Some basic Inums
    , inumNop
    -- , enumPure, inumF, inumC, inumFC, inumMC, inumLazy, inumRepeat
    ) -} where

import Prelude hiding (null)
import qualified Prelude
import Data.IterIO.Iter

import Control.Exception (SomeException(..), Exception(..))
import Control.Monad
import Control.Monad.Trans
import Data.Monoid
import System.Environment (getProgName)
import System.IO

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
cat a b iter = do
  er <- tryI $ runInumM a iter
  case er of
    Right r                   -> b $ reRunIter r
    Left (SomeException _, r) -> reRunIter r -- Re-throw exception
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
lcat a b iter = do
  er <- tryI $ runInumM a iter
  case er of
    Right (IterF i) -> b i
    Right r -> return r
    Left (SomeException _, r) -> reRunIter r
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
-- Basic tools
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

-- | Takes an 'Inum' that might return 'IterR's in the 'IterM' state
-- (which is considered impolite--see 'runIterM') and transforms it
-- into an 'Inum' then never returns 'IterR's in the 'IterM' state.
runInumM :: (ChunkData tIn, Monad m) =>
            Inum tIn tOut m a -> Inum tIn tOut m a
runInumM inum = onDone check . inum
    where
      -- check (InumFail e (IterM m) c) = IterM $ m >>= \r -> return $ check $ InumFail e r c
      check (Done (IterM m) c) = IterM $ m >>= \r -> return $ check $ Done r c
      check r = r

-- | Create an 'Inum' given a function to adjust residual data and an
-- 'Iter' that transcodes from the input to the output type.
mkInum :: (ChunkData tIn, ChunkData tOut, Monad m) =>
          ((tIn, tOut) -> (tIn, tOut))
       -- ^ Adjust residual data
       -> Iter tIn m tOut
       -- ^ Generate transcoded data chunks
       -> Inum tIn tOut m a
mkInum adjustResid codec iter0 = doIter iter0
    where
      doIter iter = catchOrI codec (inputErr iter) (doInput iter)
      inputErr iter e | isIterEOF e = return $ IterF iter
                      | otherwise   = Iter $ InumFail e (IterF iter)
      doInput iter input = do
        r <- runIterM iter (Chunk input False)
        stop <- knownEOFI
        case (stop || null input, r) of
          (False, IterF i) -> doIter i
          _ -> Iter $ \c -> fixResid r (c, getResid r)
      fixResid r (Chunk tIn0 eofIn, Chunk tOut0 eofOut) =
          let (tIn, tOut) = adjustResid (tIn0, tOut0)
          in Done (setResid r (Chunk tOut eofOut)) (Chunk tIn eofIn)

pullupResid :: (ChunkData t) => (t, t) -> (t, t)
pullupResid (a, b) = (mappend a b, mempty)

--
-- Basic Inums
--

inumNop :: (ChunkData t, Monad m) => Inum t t m a
inumNop = mkInum pullupResid dataI

inumPure :: (ChunkData tIn, Monad m) => tOut -> Inum tIn tOut m a
inumPure t iter = runIterM iter $ chunk t

inumRepeat :: (ChunkData tIn, Monad m) =>
              Inum tIn tOut m a -> Inum tIn tOut m a
inumRepeat inum iter0 = do
  er <- tryI $ runInumM inum iter0
  stop <- knownEOFI
  case (stop, er) of
    (False, Right (IterF iter)) -> inumRepeat inum iter
    (_, Right r) -> return r
    (_, Left (SomeException _, r)) -> reRunIter r
