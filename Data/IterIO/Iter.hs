{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module Data.IterIO.Iter
    (-- * Base types
     ChunkData(..), Chunk(..), chunk, chunkEOF
    , Iter(..), IterR(..), iterF
    , isIterActive, iterShows, iterShow
    -- * Execution
    , run, runI
    -- * Exception types
    , IterNoParse(..), IterEOF(..), mkIterEOF, isIterEOF
    , IterExpected(..), IterMiscParseErr(..)
    -- * Exception-related functions
    , throwI, throwEOFI
    , tryI, tryBI, catchI, finallyI, onExceptionI, catchOrI
    , catchBI, handlerI, handlerBI
    , mapExceptionI
    , ifParse, ifNoParse, multiParse
    -- * Some basic Iters
    , nullI, dataI, pureI, chunkI, peekI, atEOFI, ungetI, joinI
    -- * Internal functions
    , onDone, stepM, stepR, unRunIter, setResid, getResid
    -- * Misc debugging functions
    , traceInput, traceI
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
import Data.ByteString.Internal (inlinePerformIO)
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

    mappend ca@(Chunk a eofa) cb@(Chunk b eofb)
        | eofa      = error $ "mappend to EOF: " ++ show ca
                                           ++ " `mappend` " ++ show cb
        | null b    = Chunk a eofb -- Just an optimization for below case
        | otherwise = Chunk (mappend a b) eofb

-- | A 'Chunk' is 'null' when its data is 'null' and its EOF flag is
-- 'False'.
instance (ChunkData t) => ChunkData (Chunk t) where
    null (Chunk t False) = null t
    null (Chunk _ True)  = False
    chunkShow = show

-- | Constructor function that builds a chunk containing data and a
-- 'False' EOF flag.
chunk :: t -> Chunk t
chunk t = Chunk t False

-- | An chunk with 'mempty' data and the EOF flag 'True'.
chunkEOF :: (Monoid t) => Chunk t
chunkEOF = Chunk mempty True

-- | The basic Iteratee type is @Iter t m a@, where @t@ is the type of
-- input (in class 'ChunkData'), @m@ is a monad in which the iteratee
-- may execute actions (using the 'MonadTrans' 'lift' method), and @a@
-- is the result type of the iteratee.
--
-- Internally, an @Iter@ is a function from an input 'Chunk' to a
-- result of type 'IterR'.
newtype Iter t m a = Iter { runIter :: Chunk t -> IterR t m a }

-- | Builds an 'Iter' that keeps requesting input until it receives a
-- non-'null' chunk.
iterF :: (ChunkData t) => (Chunk t -> IterR t m a) -> Iter t m a
iterF f = Iter $ \c -> if null c then IterF $ iterF f else f c

-- | An @IterR@ is in one of several states:  it may require more
-- input ('IterF'), it may wish to execute monadic actions in the
-- transformed monad ('IterM'), it may have produced a result
-- ('Done'), or it may have failed.  Failure is indicated by
-- 'IterFail' or 'InumFail', depending on whether the failure occured
-- in an iteratee or enumerator.  In the latter case, when an 'Inum'
-- fails, the 'Iter' it is feeding usually will not have failed.
-- Thus, the 'InumFail' type includes the state of the 'Iter' that the
-- 'Inum' was feeding.
--
-- Note that @IterR t@ is a 'MonadTrans' and @IterR t m@ is a a 'Monad'.
data IterR t m a = IterF !(Iter t m a)
                 -- ^ The iteratee requires more input.
                 | IterM !(m (IterR t m a))
                 -- ^ The iteratee must execute monadic bind in monad @m@
                 | Done a (Chunk t)
                 -- ^ Sufficient input was received; the 'Iter' is
                 -- returning a result of type @a@.  In adition, the
                 -- 'Iter' has a 'Chunk' containing any residual input
                 -- that was not consumed in producing the result.
                 | IterFail !SomeException (Chunk t)
                 -- ^ The 'Iter' failed.
                 | InumFail !SomeException a (Chunk t)
                 -- ^ An 'Inum' failed; this result includes status of
                 -- the `Inum`'s target 'Iter' at the time of the
                 -- failure.  (The type @a@ will generally be @'Iter'
                 -- t' m a\'@ for some @t'@ and @a'@.)

-- | True if an 'IterR' is requesting something from an
-- enumerator--i.e., the 'Iter' is not 'Done' and is not in one of the
-- error states.
isIterActive :: IterR t m a -> Bool
isIterActive (IterF _)   = True
isIterActive (IterM _)   = True
isIterActive _           = False

-- | Show the current state of an 'IterR', prepending it to some
-- remaining input (the standard 'ShowS' optimization), when 'a' is in
-- class 'Show'.  Note that if @a@ is not in 'Show', you can simply
-- use the 'shows' function.
iterShows :: (ChunkData t, Show a) => IterR t m a -> ShowS
iterShows (Done a c) rest = "Done " ++ (shows a $ " " ++ shows c rest)
iterShows (InumFail e a c) rest =
    "InumFail " ++ (shows e $ " (" ++ (shows a $ ") " ++ shows c rest))
iterShows iter rest = shows iter rest

-- | Show the current state of an 'Iter' if type @a@ is in the 'Show'
-- class.  (Otherwise, you can simply use the ordinary 'show'
-- function.)
iterShow :: (ChunkData t, Show a) => IterR t m a -> String
iterShow iter = iterShows iter ""

instance (ChunkData t) => Show (IterR t m a) where
    showsPrec _ (IterF _) rest = "IterF _" ++ rest
    showsPrec _ (IterM _) rest = "IterM _" ++ rest
    showsPrec _ (Done _ c) rest = "Done _ " ++ shows c rest
    showsPrec _ (IterFail e c) rest =
        "IterFail " ++ (shows e $ " " ++ shows c rest)
    showsPrec _ (InumFail e _ c) rest =
        "InumFail " ++ (shows e $ " _ " ++ shows c rest)

iterTc :: TyCon
iterTc = mkTyCon "Iter"
instance (Typeable t, Typeable1 m) => Typeable1 (Iter t m) where
    typeOf1 iter = mkTyConApp iterTc [typeOf $ t iter, typeOf1 $ m iter]
        where t :: Iter t m a -> t; t _ = undefined
              m :: Iter t m a -> m (); m _ = undefined

instance (Typeable t, Typeable1 m, Typeable a) => Typeable (Iter t m a) where
    typeOf = typeOfDefault

instance (ChunkData t, Monad m) => Functor (Iter t m) where
    fmap = liftM

instance (ChunkData t, Monad m) => Applicative (Iter t m) where
    pure   = return
    (<*>)  = ap
    (*>)   = (>>)
    a <* b = do r <- a; b >> return r

stepM :: (ChunkData t1, ChunkData t2, Monad m) =>
         IterR t1 m a -> (IterR t1 m a -> IterR t2 m b) -> IterR t2 m b
stepM (IterM m) f = IterM $ liftM f m
stepM r f         = f r

stepR :: (ChunkData t, Monad m) =>
         IterR t m a -> (IterR t m a -> IterR t m b) -> IterR t m b
stepR (IterF (Iter i)) f = IterF $ Iter $ f . i
stepR (IterM m) f        = IterM $ liftM f m
stepR r f                = f r

-- | Run an 'Iter' until it enters the 'Done', 'IterFail', or
-- 'InumFail' state, then use a function to transform the 'IterR'.
onDone :: (ChunkData t, Monad m) =>
          (IterR t m a -> IterR t m b) -> Iter t m a -> Iter t m b
onDone f i = Iter $ check . runIter i
    where check r | isIterActive r = stepR r check
                  | otherwise      = f r

instance (ChunkData t, Monad m) => Monad (Iter t m) where
    return a = Iter $ Done a

    m >>= k = onDone check m
        where check (Done a c)       = runIter (k a) c
              check (IterFail e c)   = IterFail e c
              check (InumFail e _ c) = IterFail e c
              check _                = error "Iter >>="

    fail msg = Iter $ IterFail (toException $ ErrorCall msg)

instance (ChunkData t, Monad m) => Monad (IterR t m) where
    return a = Done a mempty

    (Done a c) >>= k       = runIterR (k a) c
    (IterF (Iter i)) >>= k = IterF $ Iter $ i >=> k
    (IterM m) >>= k        = IterM $ liftM (>>= k) m
    err >>= _              = IterFail (getIterError err) (getResid err)

    fail msg = IterFail (toException $ ErrorCall msg) mempty

instance (ChunkData t) => MonadTrans (IterR t) where
    lift m = IterM $ m >>= return . return

instance (ChunkData t, Monad m) => MonadPlus (Iter t m) where
    mzero = throwI $ IterMiscParseErr "mzero"
    mplus a b = ifParse a return b

instance (ChunkData t) => MonadTrans (Iter t) where
    lift m = Iter $ \c -> IterM $ m >>= return . flip Done c

translateIterEOF :: (ChunkData t) => IO a -> IO (IterR t m a)
translateIterEOF m = do
  result <- try m
  case result of
    Right ok -> return $ Done ok mempty
    Left err -> return $ flip IterFail mempty $
        case fromException err of
          Just ioerr | isEOFError ioerr -> toException $ IterEOF ioerr
          _                             -> err

instance (ChunkData t, MonadIO m) => MonadIO (IterR t m) where
    liftIO m = IterM $ liftIO $ translateIterEOF m

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
    liftIO m = Iter $ runIterR $ IterM $ liftIO $ translateIterEOF m

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
run i0 = check $ runIter i0 chunkEOF
    where check (Done a _)       = return a
          check (IterF i)        = run i
          check (IterM m)        = m >>= check
          check (IterFail e _)   = throw $ unIterEOF e
          check (InumFail e _ _) = throw $ unIterEOF e

-- | Runs an 'Iter' from within a different 'Iter' monad.  If
-- successful, @runI iter@ will produce the same result as @'lift'
-- ('run' iter)@.  However, if @iter@ fails, 'run' throws a
-- language-level exception, which cannot be caught within other
-- 'Iter' monads.  By contrast, @runI@ throws a monadic exception that
-- can be caught.  In short, use @runI@ in preference to @run@
-- whenever possible.  See a more detailed discussion of the same
-- issue in the documentation for '.|$'.
runI :: (ChunkData t1, ChunkData t2, Monad m) => Iter t1 m a -> Iter t2 m a
runI i0 = Iter $ \c ->
          let check (Done a _)       = Done a c
              check (IterF i)        = check $ runIter i chunkEOF
              check (IterM m)        = IterM $ m >>= return . check
              check (IterFail e _)   = IterFail e c
              check (InumFail e i _) = InumFail e i c
          in check $ runIter i0 chunkEOF

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

-- | Make an 'IterEOF' from a String.
mkIterEOF :: String -> IterEOF
mkIterEOF loc = IterEOF $ mkIOError eofErrorType loc Nothing Nothing

-- | True if and only if an exception is of type 'IterEOF'.
isIterEOF :: SomeException -> Bool
isIterEOF err = maybe False (\(IterEOF _) -> True) $ fromException err

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

--
-- Exception functions
--

-- | Throw an exception from an Iteratee.  The exception will be
-- propagated properly through nested Iteratees, which will allow it
-- to be categorized properly and avoid situations in which, for
-- instance, functions holding 'MVar's are prematurely terminated.
-- (Most Iteratee code does not assume the Monad parameter @m@ is in
-- the 'MonadIO' class, and so cannot use 'catch' or @'onException'@
-- to clean up after exceptions.)  Use 'throwI' in preference to
-- 'throw' whenever possible.
throwI :: (ChunkData t, Exception e) => e -> Iter t m a
throwI e = Iter $ IterFail (toException e)

-- | Throw an exception of type 'IterEOF'.  This will be interpreted
-- by 'mkInum' as an end of file chunk when thrown by the codec.  It
-- will also be interpreted by 'ifParse' and 'multiParse' as an
-- exception of type 'IterNoParse'.  If not caught within the 'Iter'
-- monad, the exception will be rethrown by 'run' (and hence '|$') as
-- an 'IOError' of type EOF.
throwEOFI :: (ChunkData t) => String -> Iter t m a
throwEOFI = throwI . mkIterEOF

-- | Internal function used by 'tryI' and 'tryBI' when re-propagating
-- exceptions that don't match the requested exception type.  (To make
-- the overall types of those two functions work out, a 'Right'
-- constructor needs to be wrapped around the returned failing
-- iteratee.)
addRightToFail :: (ChunkData t) => IterR t m a -> IterR t m (Either e a)
addRightToFail (InumFail e a c) = InumFail e (Right a) c
addRightToFail (IterFail e c)   = IterFail e c
addRightToFail i                = error $ "addRightToFail: " ++ show i

-- | If an 'Iter' succeeds and returns @a@, returns @'Right' a@.  If
-- the 'Iter' throws an exception @e@, returns @'Left' (e, i)@ where
-- @i@ is the state of the failing 'Iter'.
tryI :: (ChunkData t, Monad m, Exception e) =>
        Iter t m a -> Iter t m (Either (e, Iter t m a) a)
tryI = onDone errToEither
    where
      errToEither (Done a c) = Done (Right a) c
      errToEither iter =
          case fromException $ getIterError iter of
            Nothing -> addRightToFail iter
            Just e  -> Done (Left (e, Iter $ setResid iter)) (getResid iter)

-- | Run an 'Iter'.  Catch any exception it throws, or feed the result
-- to a continuation.
catchOrI :: (ChunkData t, Monad m, Exception e) =>
            Iter t m a
         -> (e -> Iter t m b) 
         -> (a -> Iter t m b)
         -> (Iter t m b)
catchOrI iter handler cont = onDone check iter
    where check (Done a c) = runIter (cont a) c
          check err        =
              case fromException $ getIterError err of
                Just e  -> runIter (handler e) (getResid err)
                Nothing -> IterFail (getIterError err) (getResid err)

-- | Like 'onDone', but also keeps a copy of all input consumed.  (The
-- residual input on the 'IterR' returned will be a suffix of the
-- input returned.)
onDoneInput :: (ChunkData t, Monad m) =>
               (IterR t m a -> Chunk t -> IterR t m b)
            -> Iter t m a
            -> Iter t m b
onDoneInput f = Iter . next id
    where
      next acc iter c = check $ runIter iter c
          where check (IterF i) = IterF $ Iter $ next (acc . mappend c) i
                check r | isIterActive r = stepR r check
                        | otherwise      = f r $ acc c

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
         Iter t m a -> Iter t m (Either e a)
tryBI = onDoneInput errToEither
    where errToEither (Done a c) _ = Done (Right a) c
          errToEither iter c = case fromException $ getIterError iter of
                                 Nothing -> addRightToFail iter
                                 Just e  -> Done (Left e) c

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
catchI iter handler = tryI iter >>= either (uncurry handler) return

-- | Execute an 'Iter', then perform a cleanup action regardless of
-- whether the 'Iter' threw an exception or not.  Analogous to the
-- standard library function @'finally'@.
finallyI :: (ChunkData t, Monad m) =>
            Iter t m a -> Iter t m b -> Iter t m a
finallyI iter cleanup = onDone runclean iter
    where runclean r = runIter (cleanup >> Iter (setResid r)) (getResid r)

-- | Execute an 'Iter' and perform a cleanup action if the 'Iter'
-- threw an exception.  Analogous to the standard library function
-- @'onException'@.
onExceptionI :: (ChunkData t, Monad m) =>
                Iter t m a -> Iter t m b -> Iter t m a
onExceptionI iter cleanup = catchI iter $ \(SomeException _) -> (cleanup >>)

-- | Catch exception with backtracking.  This is a version of 'catchI'
-- that keeps a copy of all data fed to the iteratee.  If an exception
-- is caught, the input is re-wound before running the exception
-- handler.  Because this function saves a copy of all input, it
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
catchBI iter0 handler = onDoneInput check iter0
    where check r@(Done _ _) _ = r
          check r c            = case fromException $ getIterError r of
                                   Nothing -> r
                                   Just e  -> runIter (handler e) c

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

{-
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
-}

mapExceptionR :: (ChunkData t, Monad m, Exception e1, Exception e2) =>
                 (e1 -> e2) -> IterR t m a -> IterR t m a
mapExceptionR f = check
    where check r@(Done _ _)     = r
          check (IterFail e c)   = IterFail (doMap e) c
          check (InumFail e a c) = InumFail (doMap e) a c
          check r                = stepR r check
          doMap e = maybe e (toException . f) $ fromException e

-- | Similar to the standard @'mapException'@ function in
-- "Control.Exception", but operates on exceptions propagated through
-- the 'Iter' monad, rather than language-level exceptions.
mapExceptionI :: (Exception e1, Exception e2, ChunkData t, Monad m) =>
                 (e1 -> e2) -> Iter t m a -> Iter t m a
mapExceptionI = onDone . mapExceptionR

-- | Run an Iteratee, and if it throws a parse error by calling
-- 'expectedI', then combine the exptected tokens with those of a
-- previous parse error.
combineExpected :: (ChunkData t, Monad m) =>
                   IterNoParse
                -- ^ Previous parse error
                -> IterR t m a
                -- ^ Iteratee to run and, if it fails, combine with
                -- previous error
                -> IterR t m a
combineExpected (IterNoParse e) r =
    case cast e of
      Just (IterExpected saw1 e1) -> mapExceptionR (combine saw1 e1) r
      _                           -> r
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
-- partially consumed state.  This is fine so long as @total@ is the
-- last 'Iter' executed on the input stream.  Otherwise, before
-- throwing the parse error, @parseAndSumIntegerList@ would need to
-- ensure the input is at some reasonable boundary point for whatever
-- comes next.  (The 'ungetI' function is sometimes helpful for this
-- purpose.)
multiParse :: (ChunkData t, Monad m) =>
              Iter t m a -> Iter t m a -> Iter t m a
multiParse a b = Iter $ \c -> check (runIter a c) (runIter b c)
    where
      check ra@(Done _ _) _ = ra
      check (IterF ia) (IterF ib) = IterF $ multiParse ia ib
      check (IterF ia) rb =
          IterF $ onDoneInput (\ra c -> check ra (runIterR rb c)) ia
      check ra rb
          | isIterActive ra = stepR ra $ flip check rb
          | otherwise       = maybe ra (flip combineExpected rb) $
                              fromException $ getIterError ra

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
ifParse iter yes no =
    tryBI iter >>= either (\e -> onDone (combineExpected e) no) yes

-- | @ifNoParse@ is just 'ifParse' with the second and third arguments
-- reversed.
ifNoParse :: (ChunkData t, Monad m) =>
             Iter t m a -> Iter t m b -> (a -> Iter t m b) -> Iter t m b
ifNoParse = flip . ifParse


--
-- Some super-basic Iteratees
--

-- | Sinks data like @\/dev\/null@, returning @()@ on EOF.
nullI :: (Monad m, ChunkData t) => Iter t m ()
nullI = Iter $ \(Chunk _ eof) ->
        if eof then Done mempty chunkEOF else IterF nullI

-- | Returns any non-empty amount of input data, or throws an
-- exception if EOF is encountered and there is no data.
dataI :: (Monad m, ChunkData t) => Iter t m t
dataI = iterF nextChunk
    where eoferr = toException $ mkIterEOF "dataI"
          nextChunk c@(Chunk d True) | null d = IterFail eoferr c
          nextChunk (Chunk d _)               = Done d mempty

-- | A variant of 'dataI' that reads the whole input up to an
-- end-of-file and returns it.
pureI :: (Monad m, ChunkData t) => Iter t m t
pureI = loop id
    where loop acc = Iter $ \(Chunk t eof) ->
                     if eof then Done (acc t) chunkEOF
                            else IterF $ loop $ acc . mappend t

-- | Returns the next 'Chunk' that either contains non-'null' data or
-- has the EOF bit set.
chunkI :: (Monad m, ChunkData t) => Iter t m (Chunk t)
chunkI = iterF $ \c -> Done c mempty

-- | Runs an 'Iter' without consuming any input.  (See 'tryBI' if you
-- want to avoid consuming input just when the 'Iter' fails.)
peekI :: (ChunkData t, Monad m) => Iter t m a -> Iter t m a
peekI = onDoneInput setResid

-- | Does not actually consume any input, but returns 'True' if there
-- is no more input data to be had.
atEOFI :: (Monad m, ChunkData t) => Iter t m Bool
atEOFI = iterF $ \c@(Chunk t _) -> Done (null t) c

-- | Place data back onto the input stream, where it will be the next
-- data consumed by subsequent 'Iter's..
ungetI :: (ChunkData t) => t -> Iter t m ()
ungetI t = Iter $ \c -> Done () (mappend (chunk t) c)

--
-- Iter manipulation functions
--

getIterError :: IterR t m a -> SomeException
getIterError (IterFail e _)   = e
getIterError (InumFail e _ _) = e
getIterError (IterM _)        = error "getIterError: no error (in IterM state)"
getIterError _                = error "getIterError: no error to extract"

getResid :: (ChunkData t) => IterR t m a -> Chunk t
getResid (Done _ c)       = c
getResid (IterFail _ c)   = c
getResid (InumFail _ _ c) = c
getResid r                = error $ "getResid: not done (" ++ show r ++ ")"

setResid :: (ChunkData t1) => IterR t1 m1 a -> Chunk t2 -> IterR t2 m2 a
setResid (Done a _)       = Done a
setResid (IterFail e _)   = IterFail e
setResid (InumFail e a _) = InumFail e a
setResid r                = error $ "setResid: not done (" ++ show r ++ ")"

runIterR :: (ChunkData t, Monad m) => IterR t m a -> Chunk t -> IterR t m a
runIterR iter0 c = if null c then iter0 else check iter0
    where check (Done a c0)       = Done a (mappend c0 c)
          check (IterF (Iter i))  = i c
          check (IterM m)         = IterM $ liftM check m
          check (IterFail e c0)   = IterFail e (mappend c0 c)
          check (InumFail e a c0) = InumFail e a (mappend c0 c)

unRunIter :: (ChunkData t, Monad m) => IterR t m a -> Iter t m a
unRunIter = Iter . runIterR

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
         Iter tIn m (Iter tOut m a)
      -> Iter tIn m a
joinI = onDone check
    where check (Done i c)        = runIter (runI i) c
          check (IterFail e c)    = IterFail e c
          check (InumFail e i c0) = runIter (onDone setErr $ runI i) c0
              where setErr (Done a c) = InumFail e a c
                    setErr err        = err
          check _                 = error "joinI"

--
-- Debugging
--

-- | For debugging, print a tag along with the current residual input.
-- Not referentially transparent.
traceInput :: (ChunkData t, Monad m) => String -> Iter t m ()
traceInput tag = Iter $ \c -> trace (tag ++ ": " ++ show c) $ Done () c

-- | For debugging.  Print the current thread ID and a message.  Not
-- referentially transparent.
traceI :: (ChunkData t, Monad m) => String -> Iter t m ()
traceI msg = return $ inlinePerformIO $ do
               tid <- myThreadId
               putTraceMsg $ show tid ++ ": " ++ msg
