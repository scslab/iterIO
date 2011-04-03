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

import Control.Exception (Exception(..))
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
{-
(|$) :: (ChunkData t, Monad m) => Onum t m a -> Iter t m a -> m a
(|$) inum iter = run (inum .| iter)
infixr 2 |$
-}

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
{-
(.|$) :: (ChunkData tIn, ChunkData tOut, Monad m) =>
         Onum tOut m a -> Iter tOut m a -> Iter tIn m a
(.|$) enum iter = runI (enum .| iter)
infixr 2 .|$
-}

-- | A function that mostly acts like '>>=', but preserves 'InumFail'
-- failures.  (By contrast, @m '>>=' k@ will translate an 'InumFail'
-- in @m@ into an 'IterFail'.)  Has fixity:
--
-- > infixl 1 `inumBind`
inumBind :: (ChunkData t, Monad m) =>
            Iter t m a -> (a -> Iter t m a) -> Iter t m a
inumBind m k = onDone check m
    where check (Done a c) = runIter (k a) c
          check r          = r
infixl 1 `inumBind`

-- | Concatenate the outputs of two enumerators.  For example,
-- @'enumFile' \"file1\" \`cat\` 'enumFile' \"file2\"@ produces an
-- 'Onum' that outputs the concatenation of files \"file1\" and
-- \"file2\".  Unless there is an 'InumFail' failure, @cat@ always
-- invokes both 'Inum's, as the second 'Inum' may have monadic
-- side-effects that must be executed even when the 'Iter' has already
-- finished.  You can wrap 'inumLazy' around an 'Inum' to prevent this
-- behavior and just return immediately if the 'Iter' has stopped
-- accepting input.  Conversely, if you want to continue executing
-- even in the event of an 'InumFail' condition, you can wrap the
-- first 'Inum' with 'inumCatch'.
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
cat a b iter = a iter `inumBind` b . unRunIter
infixr 3 `cat`

-- | Fuse two 'Inum's when the output type of the first 'Inum' is the
-- same as the input type of the second.  More specifically, if
-- @inum1@ transcodes type @tIn@ to @tOut@ and @inum2@ transcodes
-- @tOut@ to @tOut2@, then @inum1 |. inum2@ produces a new 'Inum' that
-- transcodes from @tIn@ to @tOut2@.
--
-- Note that while ordinarily type @b@ in this signature will be equal
-- to @'Inum' tOut tOut2 m a@, strictly speaking, the second argument
-- (@inum2@ above) does not actually need to be an 'Inum'; it might,
-- for instance, translate between monads as well as transcoding
-- types.
--
-- Has fixity:
--
-- > infixl 4 |.
{-
(|.) :: (ChunkData tIn, ChunkData tOut, Monad m) => 
        Inum tIn tOut m (Iter t m a)
     -- ^ 'Inum' translating from @tIn@ to @tOut@.
     -> (Iter t m' a -> Iter tOut m (IterR t m' a))
     -- ^ 'Inum' translating from @tOut@ to something else.  Typically
     -- @b@ is @'Iter' tOut2 m a@, making the overall type of this
     -- argument equivalent to @'Inum' tOut tOut2 m a@.
     -> (Iter t m' a -> Iter tIn m (IterR t m' a))
     -- ^ Returns a function of type @b -> 'Iter' tIn m b@, which,
     -- when @b@ is @'Iter' tOut2 m a@, is equivalent to an @'Inum'
     -- tIn tOut2 m a@.
(|.) outer inner iter = joinI $ outer $ inner iter
-}
{-
(|.) :: (ChunkData tIn, ChunkData tOut, Monad m) => 
        Inum t1 t2 m x -> Inum t2 t3 m y -> Inum t1 t3 m z
-}
(|.) outer inner iter = onDone joinR $ outer (inner iter)
infixl 4 |.

joinR :: (ChunkData t1, ChunkData t2, Monad m) =>
         IterR t1 m (IterR t2 m a) -> IterR t1 m a
joinR (Done r c)       = runIterR (runR r) c
joinR (IterFail e c)   = IterFail e c
joinR (InumFail e i c) = InumFail e i c
joinR _                = error "joinR: not done"

{-

-- | Fuse an 'Inum' that transcodes @tIn@ to @tOut@ with an 'Iter'
-- taking type @tOut@ to produce an 'Iter' taking type @tIn@.  Has
-- fixity:
--
-- > infixr 4 .|
(.|) :: (ChunkData tIn, ChunkData tOut, Monad m) =>
         Inum tIn tOut m a     -- ^
      -> Iter tOut m a
      -> Iter tIn m a
(.|) inner iter = joinI $ inner iter
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
           -> (e -> Iter tIn m (Iter tOut m a) -> Iter tIn m (Iter tOut m a))
           -- ^ Exception handler
           -> Inum tIn tOut m a
inumCatch enum handler = onDone check . enum
    where check r@(InumFail err a c) =
              case fromException err of
                Just e -> runIter (handler e $ Iter $ InumFail err a) c
                Nothing -> r
          check r                    = r

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

-- | 'inumCatch' with the argument order switched.
inumHandler :: (Exception e, ChunkData tIn, Monad m) =>
               (e -> Iter tIn m (Iter tOut m a) -> Iter tIn m (Iter tOut m a))
            -- ^ Exception handler
            -> Inum tIn tOut m a
            -- ^ 'Inum' that might throw an exception
            -> Inum tIn tOut m a
inumHandler = flip inumCatch

-- | Used in an exception handler, after an 'Inum' failure, to resume
-- processing of the 'Iter' by the next enumerator in a 'cat'ed
-- series.  See 'inumCatch' for an example.
resumeI :: (ChunkData tIn, Monad m) =>
           Iter tIn m (Iter tOut m a) -> Iter tIn m (Iter tOut m a)
resumeI = onDone check
    where check (InumFail _ a c) = Done a c
          check _                = error "resumeI: not InumFail"

-- | Like 'resumeI', but if the 'Iter' is resumable, also prints an
-- error message to standard error before running it.
verboseResumeI :: (ChunkData tIn, MonadIO m) =>
                  Iter tIn m (Iter tOut m a) -> Iter tIn m (Iter tOut m a)
verboseResumeI = onDone check
    where check (InumFail e a c) =
              flip runIter c $ liftIO $ do
                prog <- getProgName
                hPutStrLn stderr $ prog ++ ": " ++ show e
                return a
          check _                = error "verboseResumeI: not InumFail"


--
-- Basic Inums
--

inumMC :: (ChunkData tIn, ChunkData tOut, Monad m) =>
          -- CtlHandler (Iter tIn m) ->
          Inum tIn tOut m a
inumMC i0 = Iter $ \c ->
            let check r@(IterM _)   = stepM r check
                check r             = Done (unRunIter r) c
            in check $ runIter i0 mempty

inumNop :: (ChunkData t, Monad m) => Inum t t m a
inumNop = Iter . nextChunk
    where
      nextChunk i0 (Chunk t eof) = check $ runIter i0 $ chunk t
          where
            setEOF (Chunk t' _) = Chunk t' eof
            check (IterF i) | eof       = Done i chunkEOF
                            | otherwise = IterF $ Iter $ nextChunk i
            check r@(IterM _)           = stepM r check
            check r                     = Done (unRunIter $ setResid r mempty)
                                          (setEOF $ getResid r)

-}
