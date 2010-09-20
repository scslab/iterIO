{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}


{- | This module contains the base Enumerator/Iteratee IO
     abstractions.  See the documentation in the "Data.IterIO" module
     for a high-level tutorial on these abstractions.

     An iteratee is a data sink that is fed chunks of data.  It may
     return a useful result, or its utility may lie in monadic
     side-effects, such as storing received data to a file.  Iteratees
     are represented by the type @'Iter' t m a@.  @t@ is the type of
     the data chunks the iteratee receives as input.  (@t@ must be an
     instance of 'ChunkData', such as 'String' or lazy
     'L.ByteString'.)  @m@ is the 'Monad' in which the iteratee
     runs--for instance 'IO' (or an instance of 'MonadIO') for the
     iteratee to perform IO.  @a@ is the result type of the iteratee,
     for when it has consumed enough input to produce a result.

     An enumerator is a data source that feeds data chunks to an
     iteratee.  In this library, all enumerators are also iteratees.
     We use the type 'Inum' to represent these /iteratee-enumerators/.
     As an iteratee, an 'Inum' sinks data of some input type,
     generally designated @tIn@.  As an enumerator, the 'Inum' feeds
     data of a potentially different type, @tOut@, to another
     iteratee.  Thus, the 'Inum' can be viewed as transcoding data
     from type @tIn@ to type @tOut@ for consumption by another
     iteratee.

     'Inum's are generally constructed using the functions @'mkInum'@
     and @'mkInumM'@ in module "Data.IterIO.Inum".  The first funciton
     uses a simple @'Iter' tIn m tOut@ to translate between input type
     @tIn@ and output type @tOut@.  The second function, @'mkInumM'@,
     allows construction of more complex 'Inum's.

     An important special kind of 'Inum' is an /outer enumerator/,
     which is just an 'Inum' with the void input type @()@.  Outer
     enumerators are sources of data.  Rather than transcode input
     data, they produce data from monadic actions (or from pure data
     in the case of 'inumPure').  The type 'Onum' represents outer
     enumerators and is a synonym for 'Inum' with an input type of
     @()@.

     To execute iteratee-based IO, you must apply an 'Onum' to an
     'Iter' with the '|$' (\"pipe apply\") binary operator.

     An important property of enumerators and iteratees is that they
     can be /fused/.  The '|.' operator fuses two 'Inum's together
     (provided the output type of the first is the input type of the
     second), yielding a new 'Inum' that transcodes from the input
     type of the first to the output type of the second.  Similarly,
     the '.|' operator fuses an 'Inum' to an 'Iter', yielding a new
     'Iter' with a potentially different input type.

     Enumerators of the same type can also be concatenated with the
     'cat' function.  @enum1 ``cat`` enum2@ produces an enumerator
     whose effect is to feed first @enum1@'s data then @enum2@'s data
     to an 'Iter'.

 -}

module Data.IterIO.Base
    (-- * Base types
     ChunkData(..), Chunk(..), chunk, chunkEOF
    , CtlCmd
    , Iter(..), iterF
    , isIterActive, iterShows, iterShow
    , Inum, InumR, Onum, OnumR
    -- * Execution, concatenation and fusing operators
    , run, runI, (|$), (.|$), cat, (|.), (.|)
    -- * Exception and error functions
    , IterNoParse(..), IterEOF(..), isIterEOF
    , IterExpected(..), IterMiscParseErr(..)
    , throwI, throwEOFI
    , tryI, tryBI, catchI, catchOrI, catchBI, handlerI, handlerBI
    , inumCatch, inumHandler
    , resumeI, verboseResumeI, mapExceptionI
    , ifParse, ifNoParse, multiParse
    -- * Some basic Iters
    , nullI, dataI, pureI, chunkI, peekI, atEOFI, ungetI
    -- * Low-level Iter-manipulation functions
    , feedI, finishI, joinI, inumBind
    -- * Some basic Inums
    , inumPure, inumF, inumMC, inumLazy, inumRepeat, inumNop
    -- * Control functions
    , ctlI, safeCtlI
    , CtlHandler, CtlArg(..), CtlRes(..)
    , noCtl, passCtl, consCtl
    -- * Misc debugging function
    , tidTrace
    ) where

import Prelude hiding (null)
import qualified Prelude
import Control.Applicative (Applicative(..))
import Control.Concurrent (myThreadId)
import Control.Exception (SomeException(..), ErrorCall(..), Exception(..)
                         , try, throw)
import Control.Monad
import Control.Monad.Fix
import Control.Monad.Trans
import Data.IORef
import Data.List (intercalate)
import Data.Monoid
import Data.Typeable
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8
import System.Environment
import System.IO
import System.IO.Error (mkIOError, eofErrorType, isEOFError)
import System.IO.Unsafe

import Debug.Trace

--
-- Iteratee types and instances
--

-- | @ChunkData@ is the class of data types that can be output by an
-- enumerator and iterated on with an iteratee.  A @ChunkData@ type
-- must be a 'Monoid', but must additionally provide a predicate,
-- @null@, for testing whether an object is equal to 'mempty'.
-- Feeding a @null@ chunk to an iteratee followed by any other chunk
-- should have the same effect as just feeding the second chunk.
-- @ChunkData@ must also be convertable to a String with the
-- @chunkShow@ method to simplify debugging.
class (Monoid t) => ChunkData t where
    null :: t -> Bool
    chunkShow :: t -> String
instance (Show a) => ChunkData [a] where
    null = Prelude.null
    chunkShow = show
instance ChunkData L.ByteString where
    null = L.null
    chunkShow = show . L8.unpack
instance ChunkData S.ByteString where
    null = S.null
    chunkShow = show . S8.unpack
instance ChunkData () where
    null _ = True
    chunkShow _ = "()"

-- | @Chunk@ is a wrapper around a 'ChunkData' type that also includes
-- an EOF flag that is 'True' if the data is followed by an
-- end-of-file condition.  An 'Iter' that receives a @Chunk@ with EOF
-- 'True' must return a result (or failure); it is an error to demand
-- more data (return 'IterF') after an EOF.
data Chunk t = Chunk !t !Bool deriving (Eq, Typeable)

instance (ChunkData t) => Show (Chunk t) where
    showsPrec _ (Chunk t eof) rest =
        chunkShow t ++ if eof then "+EOF" ++ rest else rest

instance Functor Chunk where
    fmap f (Chunk t eof) = Chunk (f t) eof

instance (ChunkData t) => Monoid (Chunk t) where
    mempty = Chunk mempty False

    -- We mostly want to avoid appending data to a Chunk that has the
    -- EOF bit set, but make an exception for appending a null chunk,
    -- so that code like the following will work:
    --
    --   (Done (Done "" (Chunk "" True)) (Chunk "" False)) >>= id
    --
    -- While the above code may seem arcane, something similar happens
    -- with, for instance:
    --
    -- do iter <- finishI $ feedI (return "") chunkEOF
    --    iter
    --
    -- Also, we expect b to be null a fair fraction of the time, so it
    -- is important to optimize that case.  (E.g., actually computing
    -- longList ++ [] via mappend would be expensive.)
    mappend (Chunk a eofa) (Chunk b eofb)
            | null b    = Chunk a (eofa || eofb)
            | not eofa  = Chunk (mappend a b) eofb
            | otherwise = error "mappend to EOF"

instance (ChunkData t) => ChunkData (Chunk t) where
    null (Chunk t False) = null t
    null (Chunk _ True)  = False
    chunkShow = show

-- | Constructor function that builds a chunk containing data and a
-- 'False' EOF flag.
chunk :: t -> Chunk t
chunk t = Chunk t False

-- | An empty chunk with the EOF flag 'True'.
chunkEOF :: (Monoid t) => Chunk t
chunkEOF = Chunk mempty True


-- | Class of control commands for enclosing enumerators.  The class
-- binds each control argument type to a unique result type.
class (Typeable carg, Typeable cres) => CtlCmd carg cres | carg -> cres

-- | The basic Iteratee type is @Iter t m a@, where @t@ is the type of
-- input (in class 'ChunkData'), @m@ is a monad in which the iteratee
-- may execute actions (using the 'MonadTrans' 'lift' method), and @a@
-- is the result type of the iteratee.
--
-- An @Iter@ is in one of several states:  it may require more input
-- ('IterF'), it may request some control action other than input data
-- from the enclosing enumerators ('IterC'), it may wish to execute
-- monadic actions in the transformed monad ('IterM'), it may have
-- produced a result ('Done'), or it may have failed.  Failure is
-- indicated by 'IterFail' or 'InumFail', depending on whether the
-- failure occured in an iteratee or enumerator.  In the latter case,
-- when an 'Inum' fails, the 'Iter' it is feeding usually will not
-- have failed.  Thus, the 'InumFail' type includes the state of the
-- 'Iter' that the 'Inum' was feeding.
--
-- Note that @Iter t@ is a 'MonadTrans' and @Iter t m@ is a a 'Monad'
-- (as discussed in the documentation for module "Data.IterIO").
data Iter t m a = IterF !(Chunk t -> Iter t m a)
                -- ^ The iteratee requires more input.  Do not call
                -- this function directly; use 'feedI' to feed a
                -- 'Chunk' to an Iter.  Also you should usually use
                -- 'iterF' instead of the 'IterF' constructor
                -- directly.
                | IterM !(m (Iter t m a))
                -- ^ The iteratee must execute monadic bind in monad @m@
                | forall carg cres. (CtlCmd carg cres) =>
                  IterC !carg !(Maybe cres -> Iter t m a)
                -- ^ A control request for enclosing enumerators
                | Done a (Chunk t)
                -- ^ Sufficient input was received; the 'Iter' is
                -- returning a result of type @a@.  In adition, the
                -- 'Iter' has a 'Chunk' containing any residual input
                -- that was not consumed in producing the result.
                | IterFail !SomeException
                -- ^ The 'Iter' failed.
                | InumFail !SomeException a
                -- ^ An 'Inum' failed; this result includes status of
                -- the Iteratee.  (In this case, the type @a@ will
                -- generally be @'Iter' t' m a\'@ for some @t'@ and
                -- @a'@.)

-- | Builds an 'Iter' in the 'IterF' state.  The difference between
-- this and using 'IterF' directly is that @iterF@ returns immediately
-- upon receiving a 'null' chunk.  If you use 'IterF' and do things
-- like invoke 'liftIO' in a tail-recursive 'Iter', you can easily
-- cause an infinite loop with 'IterF', which @iterF@ prevents.
iterF :: (ChunkData t) => (Chunk t -> Iter t m a) -> Iter t m a
iterF f = iter
    where iter = IterF $ \c -> if null c then iter else f c

-- | Show the current state of an 'Iter', prepending it to some
-- remaining input (the standard 'ShowS' optimization), when 'a' is in
-- class 'Show'.  Note that if @a@ is not in 'Show', you can simply
-- use the 'shows' function.
iterShows :: (ChunkData t, Show a) => Iter t m a -> ShowS
iterShows (Done a c) rest = "Done " ++ (shows a $ " " ++ shows c rest)
iterShows (InumFail e a) rest =
    "InumFail " ++ (shows e $ " (" ++ (shows a $ ")" ++ rest))
iterShows iter rest = shows iter rest

-- | Show the current state of an 'Iter' if type @a@ is in the 'Show'
-- class.  (Otherwise, you can simply use the ordinary 'show'
-- function.)
iterShow :: (ChunkData t, Show a) => Iter t m a -> String
iterShow iter = iterShows iter ""

instance (ChunkData t) => Show (Iter t m a) where
    showsPrec _ (IterF _) rest = "IterF _" ++ rest
    showsPrec _ (IterM _) rest = "IterM _" ++ rest
    showsPrec _ (Done _ c) rest = "Done _ " ++ shows c rest
    showsPrec _ (IterC a _) rest =
        "IterC " ++ show (typeOf a) ++ " _" ++ rest
    showsPrec _ (IterFail e) rest = "IterFail " ++ show e ++ rest
    showsPrec _ (InumFail e _) rest = "InumFail " ++ (shows e $ " _" ++ rest)

instance (ChunkData t, Monad m) => Functor (Iter t m) where
    fmap = liftM

instance (ChunkData t, Monad m) => Applicative (Iter t m) where
    pure   = return
    (<*>)  = ap
    (*>)   = (>>)
    a <* b = do r <- a; b >> return r

instance (ChunkData t, Monad m) => Monad (Iter t m) where
    return a = Done a mempty

    m@(IterF _)  >>= k = IterF $ feedI m >=> k
    (IterM m)    >>= k = IterM $ liftM (>>= k) m
    (Done a c)   >>= k = feedI (k a) c
    (IterC a fr) >>= k = IterC a $ fr >=> k
    err          >>= _ = IterFail $ getIterError err

    fail msg = IterFail $ toException $ ErrorCall msg

instance (ChunkData t) => MonadTrans (Iter t) where
    lift m = IterM $ m >>= return . return

-- | The 'Iter' instance of 'MonadIO' handles errors specially.  If the
-- lifted operation throws an exception, 'liftIO' catches the
-- exception and returns it as an 'IterFail' failure.  Moreover, an IO
-- exception satisfying the 'isEOFError' predicate is re-wrapped in an
-- 'IterEOF' type so as to re-parent it below 'IterNoParse' in the
-- exception hierarchy.  ('run' and '|$' un-do the effects of this
-- re-parenting should the exception escape the 'Iter' monad.)  One
-- consequence of this behavior is that with 'Iter', unlike with most
-- monad transformers, 'liftIO' is /not/ equivalent to some number of
-- nested calls to 'lift'.  See the documentation of '.|$' for an
-- example.
instance (ChunkData t, MonadIO m) => MonadIO (Iter t m) where
    liftIO m = do
      result <- lift $ liftIO $ try m
      case result of
        Right ok -> return ok
        Left err -> IterFail $
               case fromException err of
                 Just ioerr | isEOFError ioerr -> toException $ IterEOF ioerr
                 _                             -> err

-- | This is a generalization of 'fixIO' for arbitrary members of the
-- 'MonadIO' class.  
fixMonadIO :: (MonadIO m) =>
              (a -> m a) -> m a
fixMonadIO f = do
  ref <- liftIO $ newIORef $ throw $ toException
         $ ErrorCall "fixMonadIO: non-termination"
  a <- liftIO $ unsafeInterleaveIO $ readIORef ref
  r <- f a
  liftIO $ writeIORef ref r
  return r

instance (ChunkData t, MonadIO m) => MonadFix (Iter t m) where
    mfix f = fixMonadIO f

--
-- Internal utility functions
--

getIterError                 :: Iter t m a -> SomeException
getIterError (IterFail e)   = e
getIterError (InumFail e _) = e
getIterError (IterM _)      = error "getIterError: no error (in IterM state)"
getIterError (IterC _ _)    = error "getIterError: no error (in IterC state)"
getIterError _              = error "getIterError: no error to extract"

-- | True if an 'Iter' is requesting something from an
-- enumerator--i.e., the 'Iter' is not 'Done' and is not in one of the
-- error states.
isIterActive :: Iter t m a -> Bool
isIterActive (IterF _)   = True
isIterActive (IterM _)   = True
isIterActive (IterC _ _) = True
isIterActive _           = False


--
-- Enumerator types
--

-- | Expanding the type alias 'InumR', this is equivalent to:
--
-- @
--type Inum tIn tOut m a = 'Iter' tOut m a -> 'Iter' tIn m ('Iter' tOut m a)
-- @
--
-- The type of an /iterator-enumerator/, which transcodes data from
-- some input type @tIn@ to some output type @tOut@.  An @Inum@ acts
-- as an 'Iter' when consuming data, then acts as an enumerator when
-- feeding transcoded data to another 'Iter'.
--
-- At a high level, one can think of an @Inum@ as a function from
-- 'Iter's to 'Iter's, where an @Inum@'s input and output types are
-- different.  A simpler alternative to @Inum@ might have been:
--
-- >type Inum' tIn tOut m a = Iter tOut m a -> Iter tIn m a
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
type Inum tIn tOut m a = Iter tOut m a -> InumR tIn tOut m a

-- | The result of running an 'Inum' is an 'Iter' containing another
-- 'Iter'.
type InumR tIn tOut m a = Iter tIn m (Iter tOut m a)

-- | An @Onum t m a@ is just an 'Inum' in which the input is
-- @()@--i.e., @'Inum' () t m a@--so that there is no meaningful input
-- data to transcode.  Such an enumerator is called an
-- /outer enumerator/, because it must produce the data it feeds to
-- 'Iter's by either executing actions in monad @m@, or from its own
-- internal pure state (as for 'inumPure').
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

-- | The result of running an 'Onum'.
type OnumR t m a = InumR () t m a


--
-- Core functions
--

unIterEOF :: SomeException -> SomeException
unIterEOF e = case fromException e of
                Just (IterEOF e') -> toException e'
                _                 -> e

-- | Return the result of an iteratee.  If it is still in the 'IterF'
-- state, feed it an EOF to extract a result.  Throws an exception if
-- there has been a failure.
run :: (ChunkData t, Monad m) => Iter t m a -> m a
run iter@(IterF _) = run $ feedI iter chunkEOF
run (IterM m)      = m >>= run
run (Done a _)     = return a
run (IterC _ fr)   = run $ fr Nothing
run (IterFail e)   = throw $ unIterEOF e
run (InumFail e _) = throw $ unIterEOF e

-- | Runs an 'Iter' from within a different 'Iter' monad.  If
-- successful, @runI iter@ will produce the same result as @'lift'
-- ('run' iter)@.  However, if @iter@ fails, 'run' throws a
-- language-level exception, which cannot be caught within other
-- 'Iter' monads.  By contrast, @runI@ throws a monadic exception that
-- can be caught.  In short, use @runI@ in preference to @run@
-- whenever possible.  See a more detailed discussion of the same
-- issue in the documentation for '.|$'.
runI :: (ChunkData t1, ChunkData t2, Monad m) =>
        Iter t1 m a
     -> Iter t2 m a
runI (Done a _)     = return a
runI iter@(IterF _) = runI $ feedI iter chunkEOF
runI (IterFail e)   = IterFail e
runI (InumFail e i) = InumFail e i
runI iter           = inumMC noCtl iter >>= runI

-- | Run an 'Onum' on an 'Iter'.  This is the main way of actually
-- executing IO with 'Iter's.  @|$@ is equivalent to:
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
-- with 'inumPure', if you want to parse at some coarse-granularity
-- (such as lines), and then re-parse the contents of some
-- coarser-grained parse unit.  For example:
--
-- >     rawcommand <- lineI
-- >     command <- inumPure rawcommand .| parseCommandI
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
-- > apply1 = inumPure "test1" |$ iter `catchI` handler
-- >     where
-- >       iter = inumPure "test2" .|$ fail "error"
-- >       handler (SomeException _) _ = return "caught error"
-- > 
-- > -- Does not catch error.  |$ turns the Iter failure into a language-
-- > -- level exception, which can only be caught in the IO Monad.
-- > apply2 :: IO String
-- > apply2 = inumPure "test1" |$ iter `catchI` handler
-- >     where
-- >       iter = lift (inumPure "test2" |$ fail "error")
-- >       handler (SomeException _) _ = return "caught error"
-- > 
-- > -- Catches the exception, because liftIO uses the IO catch function to
-- > -- turn language-level exceptions into monadic Iter failures.  (By
-- > -- contrast, lift works in any Monad, so cannot do this in apply2.)
-- > -- This example illustrates how liftIO is not equivalent to lift.
-- > apply3 :: IO String
-- > apply3 = inumPure "test1" |$ iter `catchI` handler
-- >     where
-- >       iter = liftIO (inumPure "test2" |$ fail "error")
-- >       handler (SomeException _) _ = return "caught error"
(.|$) :: (ChunkData tIn, ChunkData tOut, Monad m) =>
         Onum tOut m a -> Iter tOut m a -> Iter tIn m a
(.|$) enum iter = runI (enum .| iter)
infixr 2 .|$

-- | Concatenate the outputs of two enumerators.  For example,
-- @'enumFile' \"file1\" \`cat\` 'enumFile' \"file2\"@ produces an
-- 'Onum' that outputs the concatenation files \"file1\" and
-- \"file2\".  Unless there is a failure, @cat@ always invokes both
-- 'Inum's, in case the 'Inum' of the second argument has some monadic
-- side-effects that must be executed even when the 'Iter' has already
-- finished.  You can wrap 'inumLazy' around an 'Inum' to prevent this
-- behavior and have it just return immediately if the 'Iter' has
-- stopped accepting input.
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
-- Note the use of 'return' as the starting value for 'foldr'.
-- ('return' acts like a no-operation 'Inum' when concatenating, while
-- 'inumNop' acts as a no-operation in fusing.)  Also note the use of
-- 'inumLazy' as an optimization to avoid processing files once the
-- 'Iter' has finished.
--
-- @cat@ has fixity:
--
-- > infixr 3 `cat`
cat :: (ChunkData tIn, Monad m) =>
        Inum tIn tOut m a      -- ^
     -> Inum tIn tOut m a
     -> Inum tIn tOut m a
cat a b iter = a iter `inumBind` b
infixr 3 `cat`

-- | Fuse two 'Inum's when the inner type of the first 'Inum' is the
-- same as the outer type of the second.  More specifically, if
-- @inum1@ transcodes type @tIn@ to @tMid@ and @inum2@ transcodes
-- @tMid@ to @tOut@, then @inum1 |. inum2@ produces a new 'Inum' that
-- transcodes @tIn@ to @tOut@.  Has fixity:
--
-- > infixl 4 |.
(|.) :: (ChunkData tIn, ChunkData tMid, ChunkData tOut, Monad m) => 
         Inum tIn tMid m (Iter tOut m a) -- ^
      -> Inum tMid tOut m a
      -> Inum tIn tOut m a
(|.) outer inner iter = joinI $ outer $ inner iter
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
(.|) inner iter = joinI $ inner iter
infixr 4 .|



--
-- Exceptions
--

-- | Generalized class of errors that occur when an Iteratee does not
-- receive expected input.  (Catches 'IterEOF', 'IterExpected', and
-- the miscellaneous 'IterMiscParseErr'.)
data IterNoParse = forall a. (Exception a) => IterNoParse a deriving (Typeable)
instance Show IterNoParse where
    showsPrec _ (IterNoParse e) rest = show e ++ rest
instance Exception IterNoParse

noParseFromException :: (Exception e) => SomeException -> Maybe e
noParseFromException s = do IterNoParse e <- fromException s; cast e

noParseToException :: (Exception e) => e -> SomeException
noParseToException = toException . IterNoParse

-- | End-of-file occured in an Iteratee that required more input.
data IterEOF = IterEOF IOError deriving (Typeable)
instance Show IterEOF where
    showsPrec _ (IterEOF e) rest = show e ++ rest
instance Exception IterEOF where
    toException = noParseToException
    fromException = noParseFromException

-- | True if and only if an exception is of type 'IterEOF'.
isIterEOF :: SomeException -> Bool
isIterEOF err = case fromException err of
                  Just (IterEOF _) -> True
                  Nothing          -> False

-- | Iteratee expected particular input and did not receive it.
data IterExpected = IterExpected {
      iexpReceived :: String    -- ^ Input actually received
    , iexpWanted :: [String]    -- ^ List of inputs expected
    } deriving (Typeable)
instance Show IterExpected where
    showsPrec _ (IterExpected saw [token]) rest =
        "Iter expected " ++ token ++ ", saw " ++ saw ++ rest
    showsPrec _ (IterExpected saw tokens) rest =
        "Iter expected one of ["
        ++ intercalate ", " tokens ++ "]," ++ " saw " ++ saw ++ rest
instance Exception IterExpected where
    toException = noParseToException
    fromException = noParseFromException

-- | Miscellaneous Iteratee parse error.
data IterMiscParseErr = IterMiscParseErr String deriving (Typeable)
instance Show IterMiscParseErr where
    showsPrec _ (IterMiscParseErr err) rest =
        "Iteratee parse error: " ++ err ++ rest
instance Exception IterMiscParseErr where
    toException = noParseToException
    fromException = noParseFromException

-- | Throw an exception from an Iteratee.  The exception will be
-- propagated properly through nested Iteratees, which will allow it
-- to be categorized properly and avoid situations in which, for
-- instance, functions holding 'MVar's are prematurely terminated.
-- (Most Iteratee code does not assume the Monad parameter @m@ is in
-- the 'MonadIO' class, and so cannot use 'catch' or @'onException'@
-- to clean up after exceptions.)  Use 'throwI' in preference to
-- 'throw' whenever possible.
throwI :: (Exception e) => e -> Iter t m a
throwI e = IterFail $ toException e

-- | Throw an exception of type 'IterEOF'.  This will be interpreted
-- by 'mkInum' as an end of file chunk when thrown by the codec.  It
-- will also be interpreted by 'ifParse' and 'multiParse' as an
-- exception of type 'IterNoParse'.  If not caught within the 'Iter'
-- monad, the exception will be rethrown by 'run' (and hence '|$') as
-- an 'IOError' of type EOF.
throwEOFI :: String -> Iter t m a
throwEOFI loc = throwI $ IterEOF $ mkIOError eofErrorType loc Nothing Nothing

-- | Internal function used by 'tryI' and 'tryBI' when re-propagating
-- exceptions that don't match the requested exception type.  (To make
-- the overall types of those two funcitons work out, a 'Right'
-- constructor needs to be wrapped around the returned failing
-- iteratee.)
fixError :: (ChunkData t, Monad m) =>
            Iter t m a -> Iter t m (Either x a)
fixError (InumFail e i) = InumFail e $ Right i
fixError iter           = IterFail $ getIterError iter

-- | If an 'Iter' succeeds and returns @a@, returns @'Right' a@.  If
-- the 'Iter' throws an exception @e@, returns @'Left' (e, i)@ where
-- @i@ is the state of the failing 'Iter'.
tryI :: (ChunkData t, Monad m, Exception e) =>
        Iter t m a
     -> Iter t m (Either (e, Iter t m a) a)
tryI = finishI >=> errToEither
    where
      errToEither (Done a c) = Done (Right a) c
      errToEither iter       = case fromException $ getIterError iter of
                                 Just e  -> return $ Left (e, iter)
                                 Nothing -> fixError iter

-- | Run an 'Iter'.  Catch any exception it throws, or feed the result
-- to a continuation.
catchOrI :: (ChunkData t, Monad m, Exception e) =>
            Iter t m a
         -> (e -> Iter t m b) 
         -> (a -> Iter t m b)
         -> (Iter t m b)
catchOrI iter handler cont = finishI iter >>= check
    where check (Done a _) = cont a
          check err        = case fromException $ getIterError err of
                               Just e -> handler e
                               Nothing -> err >>= cont

-- | Runs an 'Iter' until it no longer requests input, keeping a copy
-- of all input that was fed to it (which might be longer than the
-- input that the 'Iter' actually consumed, because fed input includes
-- any residual data returned in the 'Done' state).
copyInput :: (ChunkData t, Monad m) =>
          Iter t m a
       -> Iter t m (Iter t m a, Chunk t)
copyInput iter0 = doit id iter0
    where
      -- It is usually faster to use mappend in a right associative
      -- way (i.e, mappend a1 (mappend a2 (mappand a3 a4)) will be
      -- faster than mappend (mappend (mappend a1 a2) a3) a4).  Thus,
      -- acc is a function of the rest of the input, rather than a
      -- simple prefix of ithe input.  This is the same technique used
      -- by 'ShowS' to optimize the use of (++) on srings.
      doit acc iter@(IterF _) =
          iterF $ \c -> doit (acc . mappend c) (feedI iter c)
      doit acc iter | isIterActive iter = inumMC passCtl iter >>= doit acc
                    | otherwise         = return (iter, acc mempty)

-- | Simlar to 'tryI', but saves all data that has been fed to the
-- 'Iter', and rewinds the input if the 'Iter' fails.  (The @B@ in
-- @tryBI@ stands for \"backtracking\".)  Thus, if @tryBI@ returns
-- @'Left' exception@, the next 'Iter' to be invoked will see the same
-- input that caused the previous 'Iter' to fail.  (For this reason,
-- it makes no sense ever to call 'resumeI' on the 'Iter' you get back
-- from @tryBI@, which is why @tryBI@ does not return the failing
-- Iteratee the way 'tryI' does.)
--
-- Because @tryBI@ saves a copy of all input, it can consume a lot of
-- memory and should only be used when the 'Iter' argument is known to
-- consume a bounded amount of data.
tryBI :: (ChunkData t, Monad m, Exception e) =>
         Iter t m a
      -> Iter t m (Either e a)
tryBI iter1 = copyInput iter1 >>= errToEither
    where
      errToEither (Done a c, _) = Done (Right a) c
      errToEither (iter, c)     = case fromException $ getIterError iter of
                                   Just e  -> Done (Left e) c
                                   Nothing -> fixError iter

-- | Catch an exception thrown by an 'Iter' or an enclosing 'Inum'
-- (for instance one applied with '.|$').  If you wish to catch just
-- errors thrown within 'Inum's, see the function 'inumCatch'.
--
-- On exceptions, @catchI@ invokes a handler passing it both the
-- exception thrown and the state of the failing 'Iter', which may
-- contain more information than just the exception.  In particular,
-- if the exception occured in an 'Inum', the returned 'Iter' will
-- also contain the 'Iter' being fed by that 'Inum', which likely will
-- not have failed.  To avoid discarding this extra information, you
-- should not re-throw exceptions with 'throwI'.  Rather, you should
-- re-throw an exception by re-executing the failed 'Iter'.  For
-- example, you could define an @onExceptionI@ function analogous to
-- the standard library @'onException'@ as follows:
--
-- @
--  onExceptionI iter cleanup =
--      iter \`catchI\` \\('SomeException' _) iter' -> cleanup >> iter'
-- @
--
-- Note that @catchI@ only works for /synchronous/ exceptions, such as
-- IO errors (thrown within 'liftIO' blocks), the monadic 'fail'
-- operation, and exceptions raised by 'throwI'.  It is not possible
-- to catch /asynchronous/ exceptions, such as lazily evaluated
-- divide-by-zero errors, the 'throw' function, or exceptions raised
-- by other threads using @'throwTo'@.
--
-- @\`catchI\`@ has the default infix precedence (@infixl 9
-- \`catchI\`@), which binds more tightly than any concatenation or
-- fusing operators.
catchI :: (Exception e, ChunkData t, Monad m) =>
          Iter t m a
       -- ^ 'Iter' that might throw an exception
       -> (e -> Iter t m a -> Iter t m a)
       -- ^ Exception handler, which gets as arguments both the
       -- exception and the failing 'Iter' state.
       -> Iter t m a
catchI iter0 handler = finishI iter0 >>= check
    where check iter@(Done _ _) = iter
          check iter            = case fromException $ getIterError iter of
                                    Just e  -> handler e iter
                                    Nothing -> iter

-- | Catch exception with backtracking.  This is a version of 'catchI'
-- that keeps a copy of all data fed to the iteratee.  If an exception
-- is caught, the input is re-wound before running the exception
-- handler.  Because this funciton saves a copy of all input, it
-- should not be used on Iteratees that consume unbounded amounts of
-- input.  Note that unlike 'catchI', this function does not return
-- the failing Iteratee, because it doesn't make sense to call
-- 'resumeI' on an Iteratee after re-winding the input.
catchBI :: (Exception e, ChunkData t, Monad m) =>
           Iter t m a
        -- ^ 'Iter' that might throw an exception
        -> (e -> Iter t m a)
        -- ^ Exception handler
        -> Iter t m a
catchBI iter0 handler = copyInput iter0 >>= uncurry check
    where check iter@(Done _ _) _ = iter
          check iter input        = case fromException $ getIterError iter of
                                      Just e  -> feedI (handler e) input
                                      Nothing -> iter

-- | A version of 'catchI' with the arguments reversed, analogous to
-- @'handle'@ in the standard library.  (A more logical name for this
-- function might be @handleI@, but that name is used for the file
-- handle iteratee in "Data.IterIO.ListLike".)
handlerI :: (Exception e, ChunkData t, Monad m) =>
          (e -> Iter t m a -> Iter t m a)
         -- ^ Exception handler
         -> Iter t m a
         -- ^ 'Iter' that might throw an exception
         -> Iter t m a
handlerI = flip catchI

-- | 'catchBI' with the arguments reversed.
handlerBI :: (Exception e, ChunkData t, Monad m) =>
             (e -> Iter t m a)
          -- ^ Exception handler
          -> Iter t m a
          -- ^ 'Iter' that might throw an exception
          -> Iter t m a
handlerBI = flip catchBI

-- | Catches errors thrown by an 'Inum', or a set of fused 'Inum's.
-- Note that only errors in 'Inum's that are lexically within the
-- scope of the argument to 'inumCatch' will be caught.  For example:
--
-- > inumBad :: (ChunkData t, Monad m) => Inum t t m a
-- > inumBad = mkInum' $ fail "inumBad"
-- > 
-- > skipError :: (ChunkData tOut, MonadIO m) =>
-- >              SomeException -> InumR tOut tIn m a -> InumR tOut tIn m a
-- > skipError e iter = do
-- >   liftIO $ hPutStrLn stderr $ "skipping error: " ++ show e
-- >   resumeI iter
-- >
-- > -- Throws an exception, because inumBad was fused outside the argument
-- > -- to inumCatch.
-- > test1 :: IO ()
-- > test1 = inumCatch (inumPure "test") skipError |. inumBad |$ nullI
-- > 
-- > -- Does not throw an exception, because inumBad fused within the
-- > -- argument to enumCatch.
-- > test2 :: IO ()
-- > test2 = inumCatch (inumPure "test" |. inumBad) skipError |$ nullI
-- > 
-- > -- Again no exception, because inumCatch is wrapped around inumBad.
-- > test3 :: IO ()
-- > test3 = inumPure "test" |. (inumCatch inumBad skipError) |$ nullI
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
           -> (e -> InumR tIn tOut m a -> InumR tIn tOut m a)
           -- ^ Exception handler
           -> Inum tIn tOut m a
inumCatch enum handler = finishI . enum >=> check
    where check iter@(InumFail e _) = case fromException e of
                                        Just e' -> handler e' iter
                                        Nothing -> iter
          check iter                = iter

-- | 'inumCatch' with the argument order switched.
inumHandler :: (Exception e, ChunkData tIn, Monad m) =>
               (e -> InumR tIn tOut m a -> InumR tIn tOut m a)
            -- ^ Exception handler
            -> Inum tIn tOut m a
            -- ^ 'Inum' that might throw an exception
            -> Inum tIn tOut m a
inumHandler = flip inumCatch

-- | Used in an exception handler, after an 'Inum' failure, to resume
-- processing of the 'Iter' by the next enumerator in a 'cat'ed
-- series.  See 'inumCatch' for an example.
resumeI :: (ChunkData tIn, Monad m) =>
           InumR tIn tOut m a -> InumR tIn tOut m a
resumeI (InumFail _ iter) = return iter
resumeI iter              = iter

-- | Like 'resumeI', but if the 'Iter' is resumable, also prints an
-- error message to standard error before running it.
verboseResumeI :: (ChunkData tIn, MonadIO m) =>
                  InumR tIn tOut m a -> InumR tIn tOut m a
verboseResumeI (InumFail err iter) = do
  prog <- liftIO $ getProgName
  liftIO $ hPutStrLn stderr $ prog ++ ": " ++ show err
  return iter
verboseResumeI iter = iter

-- | Similar to the standard @'mapException'@ function in
-- "Control.Exception", but operates on exceptions propagated through
-- the 'Iter' monad, rather than language-level exceptions.
mapExceptionI :: (Exception e1, Exception e2, ChunkData t, Monad m) =>
                 (e1 -> e2) -> Iter t m a -> Iter t m a
mapExceptionI f = finishI >=> check
    where check (IterFail e)   = IterFail (doMap e)
          check (InumFail e a) = InumFail (doMap e) a
          check iter           = iter
          doMap e = case fromException e of
                      Just e' -> toException (f e')
                      Nothing -> e

-- | Run an Iteratee, and if it throws a parse error by calling
-- 'expectedI', then combine the exptected tokens with those of a
-- previous parse error.
combineExpected :: (ChunkData t, Monad m) =>
                   IterNoParse
                -- ^ Previous parse error
                -> Iter t m a
                -- ^ Iteratee to run and, if it fails, combine with
                -- previous error
                -> Iter t m a
combineExpected (IterNoParse e) iter =
    case cast e of
      Just (IterExpected saw1 e1) -> mapExceptionI (combine saw1 e1) iter
      _                           -> iter
    where
      combine saw1 e1 (IterExpected saw2 e2) =
          IterExpected (if null saw2 then saw1 else saw2) $ e1 ++ e2

-- | Try two Iteratees and return the result of executing the second
-- if the first one throws an 'IterNoParse' exception.  Note that
-- "Data.IterIO.Parse" defines @'<|>'@ as an infix synonym for this
-- function.
--
-- The statement @multiParse a b@ is similar to @'ifParse' a return
-- b@, but the two functions operate differently.  Depending on the
-- situation, only one of the two formulations may be correct.
-- Specifically:
-- 
--  * @'ifParse' a f b@ works by first executing @a@, saving a copy of
--    all input consumed by @a@.  If @a@ throws a parse error, the
--    saved input is used to backtrack and execute @b@ on the same
--    input that @a@ just rejected.  If @a@ suceeds, @b@ is never run;
--    @a@'s result is fed to @f@, and the resulting action is executed
--    without backtracking (so any error thrown within @f@ will not be
--    caught by this 'ifParse' expression).
--
--  * Instead of saving input, @multiParse a b@ executes both @a@ and
--    @b@ concurrently as input chunks arrive.  If @a@ throws a parse
--    error, then the result of executing @b@ is returned.  If @a@
--    either succeeds or throws an exception not of class
--    'IterNoParse', then the result of running @a@ is returned.
--
--  * With @multiParse a b@, if @b@ returns a value, executes a
--    monadic action via 'lift', or issues a control request via
--    'ctlI', then further processing of @b@ will be suspended until
--    @a@ experiences a parse error, and thus the behavior will be
--    equivalent to @'ifParse' a return b@.
--
-- The main restriction on 'ifParse' is that @a@ must not consume
-- unbounded amounts of input, or the program may exhaust memory
-- saving the input for backtracking.  Note that the second argument
-- to 'ifParse' (i.e., 'return' in @ifParse a return b@) is a
-- continuation for @a@ when @a@ succeeds.
--
-- The advantage of @multiParse@ is that it can avoid storing
-- unbounded amounts of input for backtracking purposes if both
-- 'Iter's consume data.  Another advantage is that with an expression
-- such as @'ifParse' a f b@, sometimes it is not convenient to break
-- the parse target into an action to execute with backtracking (@a@)
-- and a continuation to execute without backtracking (@f@).  The
-- equivalent @multiParse (a >>= f) b@ avoids the need to do this,
-- since it does not do backtracking.
--
-- However, it is important to note that it is still possible to end
-- up storing unbounded amounts of input with @multiParse@.  For
-- example, consider the following code:
--
-- > total :: (Monad m) => Iter String m Int
-- > total = multiParse parseAndSumIntegerList (return -1) -- Bad
--
-- Here the intent is for @parseAndSumIntegerList@ to parse a
-- (possibly huge) list of integers and return their sum.  If there is
-- a parse error at any point in the input, then the result is
-- identical to having defined @total = return -1@.  But @return -1@
-- succeeds immediately, consuming no input, which means that @total@
-- must return all left-over input for the next action (i.e., @next@
-- in @total >>= next@).  Since @total@ has to look arbitrarily far
-- into the input to determine that @parseAndSumIntegerList@ fails, in
-- practice @total@ will have to save all input until it knows that
-- @parseAndSumIntegerList@ suceeds.
--
-- A better approach might be:
--
-- @
--   total = multiParse parseAndSumIntegerList ('nullI' >> return -1)
-- @
--
-- Here 'nullI' discards all input until an EOF is encountered, so
-- there is no need to keep a copy of the input around.  This makes
-- sense so long as @total@ is the last or only Iteratee run on the
-- input stream.  (Otherwise, 'nullI' would have to be replaced with
-- an Iteratee that discards input up to some end-of-list marker.)
--
-- Another approach might be to avoid parsing combinators entirely and
-- use:
--
-- @
--   total = parseAndSumIntegerList ``catchI`` handler
--       where handler \('IterNoParse' _) _ = return -1
-- @
--
-- This last definition of @total@ may leave the input in some
-- partially consumed state (including input beyond the parse error
-- that just happened to be in the chunk that caused the parse error).
-- But this is fine so long as @total@ is the last Iteratee executed
-- on the input stream.
multiParse :: (ChunkData t, Monad m) =>
              Iter t m a -> Iter t m a -> Iter t m a
multiParse a@(IterF _) b
    | useIfParse b = ifParse a return b
    | otherwise    = do c <- chunkI
                        multiParse (feedI a c) (feedI b c)
    where
      -- If b is IterM, IterC, or Done, we will just accumulate all
      -- the input anyway inside 'feedI', so we might as well do it
      -- efficiently with 'copyInput' (which is what 'ifParse' uses,
      -- indirectly, via 'tryBI').
      useIfParse (Done _ _)  = True
      useIfParse (IterM _)   = True
      useIfParse (IterC _ _) = True
      useIfParse _           = False
multiParse a b
    | isIterActive a = inumMC passCtl a >>= flip multiParse b
    | otherwise      = a `catchI` \err _ -> combineExpected err b

-- | @ifParse iter success failure@ runs @iter@, but saves a copy of
-- all input consumed using 'tryBI'.  (This means @iter@ must not
-- consume unbounded amounts of input!  See 'multiParse' for such
-- cases.)  If @iter@ suceeds, its result is passed to the function
-- @success@.  If @iter@ throws an exception of type 'IterNoParse',
-- then @failure@ is executed with the input re-wound (so that
-- @failure@ is fed the same input that @iter@ was).  If @iter@ throws
-- any other type of exception, @ifParse@ passes the exception back
-- and does not execute @failure@.
--
-- See "Data.IterIO.Parse" for a discussion of this function and the
-- related infix operator @\\/@ (which is a synonym for 'ifNoParse').
ifParse :: (ChunkData t, Monad m) =>
           Iter t m a
        -- ^ Iteratee @iter@ to run with backtracking
        -> (a -> Iter t m b)
        -- ^ @success@ function
        -> Iter t m b
        -- ^ @failure@ action
        -> Iter t m b
        -- ^ result
ifParse iter yes no = tryBI iter >>= either (\e -> combineExpected e no) yes

-- | @ifNoParse@ is just 'ifParse' with the second and third arguments
-- reversed.
ifNoParse :: (ChunkData t, Monad m) =>
             Iter t m a -> Iter t m b -> (a -> Iter t m b) -> Iter t m b
ifNoParse iter no yes = ifParse iter yes no


--
-- Some super-basic Iteratees
--

-- | Sinks data like @\/dev\/null@, returning @()@ on EOF.
nullI :: (Monad m, ChunkData t) => Iter t m ()
nullI = IterF $ \(Chunk _ eof) -> if eof then return () else nullI

-- | Returns any non-empty amount of input data, or throws an
-- exception if EOF is encountered and there is no data.
dataI :: (Monad m, ChunkData t) => Iter t m t
dataI = iterF nextChunk
    where nextChunk (Chunk d True) | null d = throwEOFI "dataI"
          nextChunk (Chunk d _)             = return d

-- | A variant of 'dataI' that reads the whole input up to an
-- end-of-file and returns it.
pureI :: (Monad m, ChunkData t) => Iter t m t
pureI = loop id
    where loop acc = do
            Chunk t eof <- chunkI
            if eof then return $ acc t else loop $ acc . mappend t

-- | Returns the next 'Chunk' that either contains non-'null' data or
-- has the EOF bit set.
chunkI :: (Monad m, ChunkData t) => Iter t m (Chunk t)
chunkI = iterF return

-- | Runs an 'Iter' without consuming any input if the 'Iter'
-- succeeds.  (See 'tryBI' if you want to avoid consuming input when
-- the 'Iter' fails.)
peekI :: (ChunkData t, Monad m) => Iter t m a -> Iter t m a
peekI iter0 = copyInput iter0 >>= check
    where check (Done a _, c) = Done a c
          check (iter, _)     = iter

-- | Does not actually consume any input, but returns 'True' if there
-- is no more input data to be had.
atEOFI :: (Monad m, ChunkData t) => Iter t m Bool
atEOFI = iterF $ \c@(Chunk t _) -> Done (null t) c

-- | Place data back onto the input stream, where it will be the next
-- data consumed by subsequent 'Iter's..
ungetI :: t -> Iter t m ()
ungetI = Done () . chunk

--
-- Iter manipulation functions
--

-- | Feeds an 'Iter' with a 'Chunk' of data.  When the 'Iter' is
-- already 'Done', or in some error condition, simulates the behavior
-- appropriately.
--
-- It is important to remember that @feedI@ is a pure transformation
-- of an 'Iter'--it doesn't actually execute anything.  For example,
-- the following attempt to duplicate the input stream to feed two
-- 'Iter's in parallel is a bad idea:
--
-- > iterTeeBad :: (ChunkData t, Monad m) =>
-- >               Iter t m a -> Iter t m b -> Iter t m (a, b)
-- > iterTeeBad a b = do
-- >   c@(Chunk t eof) <- chunkI
-- >   let (a', b') = (feedI a c, feedI b c)  -- Bad idea
-- >   if eof then liftM2 (,) (runI a') (runI b') else iterTeeBad a' b'
--
-- (The purpose of the 'runI' calls is to ensure that any residual
-- input is discarded rather than duplicated.)  The problem is that
-- this code will keep feeding input to the two 'Iter's, but won't
-- actually execute anything.  So not only will monadic side effects
-- fail to happen until all the input has been received (possibly
-- rendering the program incorrect), but the program will end up
-- buffering all the input in memory, when it would likely be better
-- to process it incrementally.
--
-- The fix is to ensure that the 'Iter's are actually processed by
-- passing them through an 'Inum' that will actually execute them.
-- For instance, you could say:
--
-- @
--    a' <- 'inumPure' t a
--    b' <- 'inumPure' t b
-- @
--
-- or:
--
-- @
--    a' <- 'inumMC' 'passCtl' $ feedI a c
--    b' <- 'inumMC' 'passCtl' $ feedI b c
-- @
--
-- Note that @feedI@ asserts the following invariants on the behavior
-- of an 'Iter':
--
--     1. An 'Iter' may not return an 'IterF' (asking for more input)
--        after receiving a 'Chunk' with the EOF bit 'True'.
--
--     2. An 'Iter' returning 'Done' must not set the EOF bit if it
--        did not receive the EOF bit.
--
-- It /is/, however, okay for an 'Iter' to return 'Done' without the
-- EOF bit even if the EOF bit was set on the input chunk, as
-- @feedI@ will adjust the 'Done' response to set the EOF bit.
--
-- As an example, the following code is valid:
--
-- @
--      feedI (return ()) 'chunkEOF'
-- @
--
-- Even though it is equivalent to:
--
-- @
--      feedI ('Done' () ('Chunk' 'mempty' False)) ('Chunk' 'mempty' True)
-- @
--
-- in which the first argument to @feedI@ appears to be discarding
-- the EOF bit from the input chunk.  @feedI@ will propagate the EOF
-- bit, making the above code equivalent to to @'Done' () 'chunkEOF'@.
--
-- On the other hand, the following code is illegal, as it violates
-- invariant 2 above:
--
-- @
--      feedI ('Done' () 'chunkEOF') $ 'Chunk' \"some data\" False -- Bad
-- @
feedI :: (ChunkData t, Monad m) => Iter t m a -> Chunk t -> Iter t m a
feedI (IterF f) c@(Chunk _ eof) = (if eof then forceEOF else noEOF) $ f c
    where
      noEOF (Done _ (Chunk _ True)) = error "feedI: illegal EOF"
      noEOF iter                    = iter
      forceEOF (IterF _)            = error "feedI: IterF returned after EOF"
      forceEOF (Done a (Chunk t _)) = Done a (Chunk t True)
      forceEOF iter                 = feedI iter chunkEOF
feedI iter c | null c           = iter
feedI (IterM m) c               = IterM $ flip feedI c `liftM` m
feedI (Done a c) c'             = Done a (mappend c c')
feedI (IterC a fr) c            = IterC a $ flip feedI c . fr
feedI err _                     = err

-- | Runs an 'Iter' until it is no longer active (meaning not in the
-- 'IterF', 'IterM', or 'IterC' states), then 'return's the 'Iter'
-- much like an 'Inum'.  This function looks a lot like 'inumNop', but
-- is actually not real 'Inum' because, upon receiving an EOF,
-- @finishI@ feeds the EOF to the 'Iter'.  'Inum's are supposed to
-- return 'Iter's upon receiving EOF, but are not supposed to feed the
-- EOF to the 'Iter', as this breaks functions like 'cat'.
--
-- The purpose of this function is to allow one to execute an 'Iter'
-- and look at its state without worrying about the 'Iter' throwing an
-- exception, and without worrying about the 'IterF', 'IterM', and
-- 'IterC' states (which are guaranteed not to be returned by
-- @finishI@).  For example, to execute an 'Iter' but handle errors
-- specially, you can run:
--
-- >  do iter' <- finishI iter
-- >     case iter' of
-- >       IterFail e   -> ... handle error ...
-- >       InumFail e i -> ... handle error ...
-- >       Done a _     -> ... do something with a ...
--
-- Note that @finishI@ pulls any left-over data up to the enclosing
-- 'Iter' when it returns 'Done'.  In other words, spelled out, a
-- successful result is always of the form:
--
-- >       Done (Done a (Chunk mempty eof) t eof)
--
-- This makes it safe to ignore the residual data from the inner
-- 'Done', as with @Done a _ -> ...@ in the example above.  No
-- residual data is lost in this case.
finishI :: (ChunkData t, Monad m) => Inum t t m a
finishI iter@(IterF _)           = iterF $ finishI . feedI iter
finishI (IterM m)                = IterM $ finishI `liftM` m
finishI (IterC a fr)             = IterC a $ finishI . fr
finishI (Done a c@(Chunk _ eof)) = Done (Done a (Chunk mempty eof)) c
finishI iter                     = return iter

-- | A function that mostly acts like '>>=', but preserves 'InumFail'
-- failures.  (By contrast, @m '>>=' k@ will translate an 'InumFail'
-- in @m@ into an 'IterFail'.)  Has fixity:
--
-- > infixl 1 `inumBind`
inumBind :: (ChunkData t, Monad m) =>
            Iter t m a -> (a -> Iter t m a) -> Iter t m a
inumBind iter0 next = finishI iter0 >>= check
    where check iter@(InumFail _ _) = iter
          check iter                = iter >>= next
infixl 1 `inumBind`

-- | Join the result of an 'Inum', turning it into an 'Iter'.  The
-- behavior of @joinI@ is similar to what one would obtain by defining
-- @joinI iter = iter >>= 'runI'@, but with more precise error
-- handling.  Specifically, with @iter >>= 'runI'@, the @'>>='@
-- operator for 'Iter' translates enumerator failures ('InumFail')
-- into iteratee failures ('IterFail'), discarding the state of the
-- 'Iter' even when the failure occured in an 'Inum'.  By contrast,
-- @joinI@ preserves enumerator failures, allowing the state of the
-- non-failed 'Iter' to be resumed by 'resumeI'.  (The fusing
-- operators '|.' and '.|' use @joinI@ internally, and it is this
-- error preserving property that allows 'inumCatch' and 'resumeI' to
-- work properly around fused 'Inum's.)
joinI :: (ChunkData tOut, ChunkData tIn, Monad m) =>
         InumR tIn tOut m a
      -> Iter tIn m a
joinI (Done i c)     = runI i `inumBind` flip Done c
joinI (IterFail e)   = IterFail e
joinI (InumFail e i) = runI i `inumBind` InumFail e
joinI iter           = finishI iter >>= joinI

--
-- Basic Inums
--

-- | An 'Inum' that will feed pure data to 'Iter's.
inumPure :: (Monad m, ChunkData tIn, ChunkData tOut) =>
            tOut -> Inum tIn tOut m a
inumPure t iter = inumMC passCtl $ feedI iter $ chunk t

-- | An 'Inum' that passes data straight through to an 'Iter' in the
-- 'IterF' state and returns the 'Iter' as soon as it enters any
-- state other than 'IterF'.
--
-- When the 'Iter' returns 'Done', @inumF@ pulls the residual data up
-- to the enclosing 'Iter', ensuring the 'Done' returned will always
-- have 'mempty' residual data (like 'finishI').  This makes it
-- convenient to 'cat' @inumF@ at the end of other 'Inum's that have
-- the same input and output types.  For example, one can implement
-- 'inumNop' as follows:
--
-- @
-- inumNop = 'inumRepeat' ('inumMC' 'passCtl' ``cat`` 'inumF')
-- @
--
-- By contrast, it would be incorrect to implement it as follows:
--
-- > inumNop' = inumRepeat (inumF `cat` inumMC passCtl) -- Bad
--
-- The reason is that fusing an @inum@ to an iter should have no
-- effect:  @'inumNop' .| iter@ behaves identially to @iter@.  On the
-- other hand, @inumNop' .| iter@ discards any residual data.  For
-- example, @inumNop' .| ('Done' () ('chunk' \"abcde\"))@ becomes
-- @'Done' () ('chunk' \"\")@
inumF :: (ChunkData t, Monad m) => Inum t t m a
inumF iter@(IterF _)           = iterF dochunk
    where dochunk (Chunk t True) = return $ feedI iter (chunk t)
          dochunk c              = inumF $ feedI iter c
inumF (Done a c@(Chunk _ eof)) = Done (Done a (Chunk mempty eof)) c
inumF iter                     = return iter

-- | An 'Inum' that only processes 'IterM' and 'IterC' requests and
-- returns the 'Iter' as soon as it requests input (via 'IterF'),
-- returns a result, or experiences an error.  The first argument
-- specifies what to do with 'IterC' requests, and can be 'noCtl' to
-- reject requests, 'passCtl' to pass them up to the @'Iter' tIn m@
-- monad, or a custom 'CtlHandler' function for the @'Iter' tIn m@
-- monad.
--
-- @inumMC@ is useful for partially executing and iter up to the point
-- where it requires more input.  For example, to duplicate the input
-- stream and execute two 'Iter's incrementally on the same input:
--
-- @
--  iterTee :: ('ChunkData' t, 'Monad' m) =>
--             'Iter' t m a -> 'Iter' t m b -> 'Iter' t m (a, b)
--  iterTee a0 b0 = loop (a0 '<*' 'nullI') (b0 '<*' 'nullI')
--      where loop a b = do
--              c\@(Chunk _ eof) <- 'chunkI'
--              a' <- inumMC 'passCtl' $ 'feedI' a c
--              b' <- inumMC 'passCtl' $ 'feedI' b c
--              if eof then 'liftM2' (,) a' b' else loop a' b'
-- @
--
-- (Note the use of @iter '<*' 'nullI'@, which is equivalent to
-- @iter >>= \\r -> 'nullI' >> r@ and discards any residual input.)
-- See the description of 'feedI' for a discussion of why this
-- function is not a good idea without @inumMC@ or something
-- comparable.
inumMC :: (ChunkData tIn, ChunkData tOut, Monad m) =>
          CtlHandler (Iter tIn m) -> Inum tIn tOut m a
inumMC ch (IterM m) = IterM $ inumMC ch `liftM` m
inumMC ch iter@(IterC a fr) = catchOrI (ch $ CtlArg a) (flip InumFail iter) $
                              \(CtlRes cres) -> inumMC ch $ fr $ cast cres
inumMC _ iter = return iter

-- | Runs an 'Inum' only if the 'Iter' is still active.  Otherwise
-- just returns the 'Iter'.  See an example at 'cat'.
inumLazy :: (ChunkData tIn, Monad m) =>
            Inum tIn tOut m a -> Inum tIn tOut m a
inumLazy inum iter | isIterActive iter = inum iter
                   | otherwise         = return iter

-- | Repeat an 'Inum' until an end of file is received, the 'Inum'
-- fails, or the 'Iter' terminates.  Runs the 'Inum' at least once
-- even if the 'Iter' is no longer active.
inumRepeat :: (ChunkData tIn, Monad m) =>
              (Inum tIn tOut m a) -> (Inum tIn tOut m a)
inumRepeat inum = runinum
    where runinum = inum `cat` inumLazy again
          again iter@(IterF _) = do
            eof <- atEOFI
            if eof then return iter else runinum iter
          again iter           = do
            -- Here we might miss an EOF we haven't read yet, but but
            -- don't want to request more than residual input in case
            -- monadic side-effects are required to produce that
            -- input.  (Note the capital IterF, which makes the next
            -- line different from atEOFI.)
            eof <- IterF $ \c@(Chunk t eof) -> Done (eof && null t) c
            if eof then return iter else runinum iter
          
-- | The dummy 'Inum' which passes all data straight through to the
-- 'Iter'.  Note that this is a \"Nop\" in the sense that if you fuse
-- it to an iter with @inumNop '.|' iter@, you get a result
-- indistinguishable from @iter@ on its own.  This is useful, for
-- instance, when switching between various processing at runtime,
-- with a construct such as:
--
-- > enum |. (if debug then inumLog "debug.log" else inumNop) |$ iter
--
-- Another type of \"Nop\" you might want is an 'Iter' that literally
-- does nothing (not even pass through data) and immediately returns
-- the 'Iter'.  Such a nop is useful as the initial value of a fold.
-- This second \"Nop\" functionaly is already provided by the monadic
-- 'return' function.  See an example at 'cat'.
inumNop :: (ChunkData t, Monad m) => Inum t t m a
inumNop = inumRepeat (inumMC passCtl `cat` inumF)

--
-- Support for control operations
--

-- | Issue a control request, return 'Just' the result if the request
-- is supported by enclosing enumerators, otherwise return 'Nothing'.
safeCtlI :: (CtlCmd carg cres, ChunkData t, Monad m) =>
            carg -> Iter t m (Maybe cres)
safeCtlI carg = IterC carg return

-- | Issue a control request, and return the result.  Throws an
-- exception if the operation did not succeed.
ctlI :: (CtlCmd carg cres, ChunkData t, Monad m) =>
        carg -> Iter t m cres
ctlI carg = safeCtlI carg >>= returnit
    where
      returnit (Just res) = return res
      returnit Nothing    = fail $ "Unsupported CtlCmd " ++ show (typeOf carg)

-- | Internal private type which is guaranteed not to be the result of
-- a 'CtlCmd'.  Thus, it can be used by a 'CtlHandler' that did not
-- match the argument type to return Nothing (when it is 'cast' to the
-- actual @cres@ type expected by an 'IterC').
data CtlBad = CtlBad deriving (Typeable)

-- | A wrapper around an arbitrary 'CtlCmd' argument, used so that the
-- 'CtlHandler' type can work with all 'CtlCmd' argument types.  It is
-- best to use 'consCtl' instead of employing the 'CtlArg' and
-- 'CtlRes' types directly.
data CtlArg = forall carg cres. (CtlCmd carg cres) => CtlArg !carg

-- | Use to wrap the result of a 'CtlHandler'.  Without Rank2Types,
-- the CtlHandler result has to be dynamic like this.  It is best to
-- use 'consCtl' instead of employing the 'CtlArg' and 'CtlRes' types
-- directly.
data CtlRes = forall cres. (Typeable cres) => CtlRes cres

-- | A control request handler maps control requests to 'Iter's.
type CtlHandler m = CtlArg -> (m CtlRes)

-- | A control request handler that ignores the request argument and
-- always fails immediately (thereby not passing the control request
-- up further to other enclosing enumerators).
--
-- One use of this is for 'Inum's that change the data in such a way
-- that control requests would not make sense to outer enumerators.
-- Suppose @inumGunzip@ is a codec that uncompresses a file in gzip
-- format.  It should probably be call @'setCtlHandler' noCtl@ before
-- producing any output.  Otherwise, problems would likely ensue in
-- the event of any seek requests, as whatever enumerator surrounds
-- @inumGunzip@ might seek around in the file, confusing the
-- @inumGunzip@.
noCtl :: (Monad m) => CtlHandler m
noCtl _ = return $ CtlRes CtlBad

-- | A control request hander that simply passes control requests
-- straight through from the @'Iter' tOut m@ monad to the enclosing
-- @'Iter' tIn m@ monad.
passCtl :: (ChunkData t, Monad m) => CtlHandler (Iter t m)
passCtl (CtlArg carg) = safeCtlI carg >>= return . maybe (CtlRes CtlBad) CtlRes

-- | Construct a 'CtlHandler', from a function of a particular
-- 'CtlCmd' type and a fallback 'CtlHandler' if the type of the
-- argument doesn't match the function.  The fallback handler should
-- be 'passCtl', 'noCtl', or the result of a previous 'consCtl' call.
-- Has fixity:
--
-- > infixr 9 `consCtl`
consCtl :: (CtlCmd carg cres, Monad m) =>
           (carg -> m cres) -> CtlHandler m -> CtlHandler m
consCtl fn fallback arg@(CtlArg carg) =
    maybe (fallback arg) (fn >=> return . CtlRes) (cast carg)
infixr 9 `consCtl`

--
-- Debugging
--

-- | For debugging.  Print the current thread ID and a message.
tidTrace :: (MonadIO m) => String -> m ()
tidTrace msg = liftIO $ do
                 tid <- myThreadId
                 putTraceMsg $ show tid ++ ": " ++ msg
