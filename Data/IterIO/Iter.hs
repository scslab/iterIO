{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module Data.IterIO.Iter
    (-- * Base types
     ChunkData(..), Chunk(..), chunk, chunkEOF
    , Iter(..), CtlCmd, CtlRes(..), CtlArg(..), IterR(..), iterF
    , isIterActive, iterShows, iterShow
    -- * Execution
    , run, runI
    -- * Exception types
    , IterNoParse(..), IterEOF(..), mkIterEOF, isIterEOF
    , IterExpected(..), IterMiscParseErr(..)
    , IterCUnsupp(..)
    -- * Exception-related functions
    , throwI, throwEOFI
    , genCatchI, catchI, handlerI, tryI, tryIe, tryIr, finallyI, onExceptionI
    , catchBI, handlerBI, tryBI 
    , mapExceptionI
    , ifParse, ifNoParse, multiParse
    -- * Some basic Iters
    , nullI, data0I, dataI, pureI, chunkI
    , peekI, atEOFI, ungetI
    , safeCtlI, ctlI
    -- * Internal functions
    , onDone, fmapI
    , onDoneR, stepR, stepR', runR, fmapR, reRunIter, runIterR
    , getResid, setResid
    ) where

import Prelude hiding (null)
import qualified Prelude
import Control.Applicative (Applicative(..))
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
import System.IO.Error (mkIOError, eofErrorType, isEOFError)
import System.IO.Unsafe

--
-- Iteratee types and instances
--

-- | @ChunkData@ is the class of data types that can be output by an
-- enumerator and iterated on with an iteratee.  A @ChunkData@ type
-- must be a 'Monoid', but must additionally provide a predicate,
-- @null@, for testing whether an object is equal to 'mempty'.
-- Feeding a @null@ chunk to an iteratee followed by any other chunk
-- should have the same effect as just feeding the second chunk.  To
-- simplify debugging, there is an additional requirement that
-- @ChunkData@ be convertable to a String with the @chunkShow@ method.
--
-- Note that because the "Prelude" contains a function 'Prelude.null'
-- for lists, you may wish to include the import:
--
-- > import Prelude hiding (null)
--
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
    {-# INLINE mempty #-}
    mempty = Chunk mempty False

    mappend ca@(Chunk a eofa) cb@(Chunk b eofb)
        | eofa      = error $ "mappend to EOF: " ++ show ca
                                           ++ " `mappend` " ++ show cb
        | null b    = Chunk a eofb -- Just an optimization for case below
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
{-# INLINE chunk #-}
chunk t = Chunk t False

-- | An chunk with 'mempty' data and the EOF flag 'True'.
chunkEOF :: (Monoid t) => Chunk t
{-# INLINE chunkEOF #-}
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
-- non-'null' 'Chunk'.  In other words, the 'Chunk' fed to the
-- argument function is guaranteed either to contain data or to have
-- the EOF flag true (or both).
iterF :: (ChunkData t) => (Chunk t -> IterR t m a) -> Iter t m a
{-# INLINE iterF #-}
iterF f = Iter $ \c -> if null c then IterF $ iterF f else f c

-- | Class of control commands for enclosing enumerators.  The class
-- binds each control argument type to a unique result type.
class (Typeable carg, Typeable cres) => CtlCmd carg cres | carg -> cres

-- | The outcome of an 'IterC' request.
data CtlRes a = CtlUnsupp
              -- ^ The request type was not supported by the enumerator.
              | CtlFail !SomeException
              -- ^ The request was supported, and executing it caused
              -- an exception to be thrown.
              | CtlDone !a
              -- ^ The result of the control operation.
              deriving (Typeable)

-- | Used when an 'Iter' is issuing a control request to an enclosing
-- enumerator.  Note that unlike 'IterF' or 'IterM', control requests
-- expose the residual data, which is ordinarily fed right back to the
-- continuation upon execution of the request.  This allows certain
-- control operations (such as seek and tell) to flush, check the
-- length of, or adjust the residual data.
data CtlArg t m a = forall carg cres. (CtlCmd carg cres) =>
    CtlArg !carg (CtlRes cres -> Iter t m a) (Chunk t)

-- | An @IterR@ is the result of feeding a chunk of data to an 'Iter'.
-- An @IterR@ is in one of several states:  it may require more input
-- ('IterF'), it may wish to execute monadic actions in the
-- transformed monad ('IterM'), it may have a control request for an
-- enclosing enumerator ('IterC'), it may have produced a result
-- ('Done'), or it may have failed.  Failure is indicated by
-- 'IterFail' or 'InumFail', depending on whether the failure occured
-- in an iteratee or enumerator.  In the latter case, when an 'Inum'
-- fails, the 'Iter' it is feeding usually will not have failed.
-- Thus, the 'InumFail' type includes the state of the 'Iter' that the
-- 'Inum' was feeding.
data IterR t m a = IterF !(Iter t m a)
                 -- ^ The iteratee requires more input.
                 | IterM !(m (IterR t m a))
                 -- ^ The iteratee must execute monadic bind in monad @m@
                 | IterC !(CtlArg t m a)
                 -- ^ A control request (see 'CtlArg').
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
-- enumerator--i.e., the 'IterR' is not 'Done' and is not in one of
-- the error states.
isIterActive :: IterR t m a -> Bool
{-# INLINE isIterActive #-}
isIterActive (IterF _)     = True
isIterActive (IterM _)     = True
isIterActive (IterC _)     = True
isIterActive _             = False

-- | Show the current state of an 'IterR', prepending it to some
-- remaining input (the standard 'ShowS' optimization), when 'a' is in
-- class 'Show'.  Note that if @a@ is not in 'Show', you can simply
-- use the 'shows' function.
iterShows :: (ChunkData t, Show a) => IterR t m a -> ShowS
iterShows (IterC (CtlArg a _ c)) rest =
    "IterC " ++ (shows (typeOf a) $ " _ " ++ shows c rest)
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
    showsPrec _ (IterC (CtlArg a _ c)) rest =
        "IterC " ++ (shows (typeOf a) $ " _" ++ shows c rest)
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

instance (Monad m) => Functor (Iter t m) where
    fmap = fmapI

-- | @fmapI@ is like 'liftM', but differs in one important respect:
-- it preserves 'InumFail' states (and in fact maps the non-failed
-- iter state).  By contrast, 'liftM', which is equivalent to
-- @'liftM' f i = i '>>=' 'return' . f@, transforms 'InumFail' states
-- to 'IterFail' ones because of its use of '>>='.
fmapI :: (Monad m) => (a -> b) -> Iter t m a -> Iter t m b
fmapI = onDone . fmapR

-- | Maps the result of an 'IterR' like 'fmap', but only if the
-- 'IterR' is no longer active.  It is an error to call this function
-- on an 'IterR' in the 'IterF', 'IterM', or 'IterC' state.  Because
-- of this restriction, @fmapR@ does not require the input and output
-- 'Monad' types (@m1@ and @m2@) to be the same.
fmapR :: (a -> b) -> IterR t m1 a -> IterR t m2 b
fmapR f (Done a c)       = Done (f a) c
fmapR _ (IterFail e c)   = IterFail e c
fmapR f (InumFail e a c) = InumFail e (f a) c
fmapR _ (IterF _)        = error "fmapR (IterF)"
fmapR _ (IterM _)        = error "fmapR (IterM)"
fmapR _ (IterC _)        = error "fmapR (IterC)"

instance (Monad m) => Functor (IterR t m) where
    fmap = onDoneR . fmapR

instance (Monad m) => Applicative (Iter t m) where
    pure   = return
    (<*>)  = ap
    (*>)   = (>>)
    a <* b = do r <- a; b >> return r

instance (Monad m) => Monad (Iter t m) where
    {-# INLINE return #-}
    return a = Iter $ Done a

    {-# INLINE (>>=) #-}
    -- Let's hope we can at least optimize the common case of Done,
    -- since check calls itself and (>>=) recursively, IterF and IterM
    -- likely cannot be inlined.
    m >>= k = Iter $ \c0 -> case runIter m c0 of
                              (Done a c) -> runIter (k a) c
                              r          -> check r
        where check (IterM mm)             = IterM $ mm >>= return . check
              check (IterF i)              = IterF $ i >>= k
              check (IterC (CtlArg a n c)) = IterC $ CtlArg a (n >=> k) c
              check (IterFail e c)         = IterFail e c
              check (InumFail e _ c)       = IterFail e c
              check (Done a c)             = runIter (k a) c

    fail msg = Iter $ IterFail (toException $ ErrorCall msg)

instance (ChunkData t, Monad m) => MonadPlus (Iter t m) where
    mzero = throwI $ IterMiscParseErr "mzero"
    mplus a b = ifParse a return b

instance MonadTrans (Iter t) where
    {-# INLINE lift #-}
    lift m = Iter $ \c -> IterM $ m >>= return . flip Done c

translateIterEOF :: IO a -> IO (Iter t m a)
translateIterEOF m = do
  result <- try m
  case result of
    Right ok -> return $ Iter $ Done ok
    Left err -> return $ Iter $ IterFail $
        case fromException err of
          Just ioerr | isEOFError ioerr -> toException $ IterEOF ioerr
          _                             -> err

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
instance (MonadIO m) => MonadIO (Iter t m) where
    liftIO m = Iter $ \c -> IterM $ do i <- liftIO $ translateIterEOF m
                                       return $ runIter i c

-- | This is a generalization of 'fixIO' for arbitrary members of the
-- 'MonadIO' class.
fixMonadIO :: (MonadIO m) => (a -> m a) -> m a
fixMonadIO f = do
  ref <- liftIO $ newIORef $ throw $ toException
         $ ErrorCall "fixMonadIO: non-termination"
  a <- liftIO $ unsafeInterleaveIO $ readIORef ref
  r <- f a
  liftIO $ writeIORef ref r
  return r

instance (MonadIO m) => MonadFix (Iter t m) where
    mfix = fixMonadIO

--
-- Core functions
--

unIterEOF :: SomeException -> SomeException
unIterEOF e = case fromException e of
                Just (IterEOF e') -> toException e'
                _                 -> e

-- | Feed an EOF to an 'Iter' and return the result.  Throws an
-- exception if there has been a failure.
run :: (ChunkData t, Monad m) => Iter t m a -> m a
run i0 = check $ runIter i0 chunkEOF
    where check (Done a _)             = return a
          check (IterF i)              = run i
          check (IterM m)              = m >>= check
          check (IterC (CtlArg _ n c)) = check $ runIter (n CtlUnsupp) c
          check (IterFail e _)         = throw $ unIterEOF e
          check (InumFail e _ _)       = throw $ unIterEOF e

-- | The equivalent for 'runI' for 'IterR's.
runR :: (ChunkData t1, ChunkData t2, Monad m) => IterR t1 m a -> IterR t2 m a
runR (Done a _)             = Done a mempty
runR (IterF i)              = runR $ runIter i chunkEOF
runR (IterM m)              = IterM $ liftM runR m
runR (IterC (CtlArg _ n c)) = runR $ runIter (n CtlUnsupp) c
runR (IterFail e _)         = IterFail e mempty
runR (InumFail e i _)       = InumFail e i mempty

-- | Runs an 'Iter' from within a different 'Iter' monad.  If
-- successful, @runI iter@ will produce the same result as @'lift'
-- ('run' iter)@.  However, if @iter@ fails, 'run' throws a
-- language-level exception, which cannot be caught within other
-- 'Iter' monads.  By contrast, @runI@ throws a monadic exception that
-- can be caught.  In short, use @runI@ in preference to @run@ in
-- situations where both are applicable.  See a more detailed
-- discussion of the same issue with examples in the documentation for
-- @'.|$'@ in "Data.IterIO.Inum".
runI :: (ChunkData t1, ChunkData t2, Monad m) => Iter t1 m a -> Iter t2 m a
runI i = Iter $ runIterR (runR $ runIter i chunkEOF)

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

-- | An end-of-file occured in an Iteratee that required more input.
-- Note that the 'liftIO' function re-parents all IO EOF errors under
-- this exception.  Thus, if you are attempting to catch an EOF
-- 'IOError' outside of the 'liftIO' block, you will need to match
-- @IterEOF@ exceptions rather, e.g.:
--
-- @
--  ('liftIO' $ ...) ``catchI`` \\(IterEOF e) _ -> ...
-- @
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

-- | Exception thrown by 'CtlI' when the type of the control request
-- is not supported by the enclosing enumerator.
data IterCUnsupp = forall carg cres. (CtlCmd carg cres) =>
                   IterCUnsupp carg deriving (Typeable)
instance Show IterCUnsupp where
    showsPrec _ (IterCUnsupp carg) rest =
        "Unsupported control request " ++ shows (typeOf carg) rest
instance Exception IterCUnsupp

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
throwI :: (Exception e) => e -> Iter t m a
{-# INLINE throwI #-}
throwI e = Iter $ IterFail (toException e)

-- | Throw an exception of type 'IterEOF'.  This will be interpreted
-- by 'mkInum' as an end of file chunk when thrown by the codec.  It
-- will also be interpreted by 'ifParse' and 'multiParse' as an
-- exception of type 'IterNoParse'.  If not caught within the 'Iter'
-- monad, the exception will be rethrown by 'run' (and hence '|$') as
-- an 'IOError' of type EOF.
throwEOFI :: String -> Iter t m a
{-# INLINE throwEOFI #-}
throwEOFI = throwI . mkIterEOF

-- | Run an 'Iter'.  Catch any exception it throws (and return the
-- failing iter state).  Transform successful results with a function.
--
-- This function is slightly more general than 'catchI'.  For
-- instance, we can't implement 'tryI' in terms of just 'catchI'.
-- Something like
--
-- > tryI iter = catchI (iter >>= return . Right) ...
--
-- would turn 'InumFail' states into 'IterFail' states, because the
-- '>>=' operator has this effect.  (I.e., even if @iter@ is
-- 'InumFail', the expression @iter >>= return . Right@ will be
-- 'IterFail'.)  This could be particularly bad in cases where the
-- exception is not even of a type caught by the 'tryI' expression.
--
-- Similarly, trying to implement 'catchI' in terms of 'tryI' doesn't
-- quite work.  Something like
--
-- > catchI iter handler = tryI iter >>= either (uncurry handler) return
--
-- would convert any 'InumFail' exceptions /not/ caught by the handler
-- into 'IterFail' states.
genCatchI :: (ChunkData t, Monad m, Exception e) =>
             Iter t m a
          -- ^ 'Iter' that might throw an exception
          -> (e -> IterR t m a -> Iter t m b) 
          -- ^ Exception handler
          -> (a -> b)
          -- ^ Conversion function for result and 'InumFail' errors.
          -> Iter t m b
genCatchI iter0 handler conv = onDone check iter0
    where check (Done a c) = Done (conv a) c
          check r = case fromException $ getIterError r of
                      Just e  -> runIter (handler e (setResid r mempty))
                                 (getResid r)
                      Nothing -> fmap conv r

-- | Catch an exception thrown by an 'Iter', including exceptions
-- thrown by any 'Inum's fused to the 'Iter' (or applied to it with
-- '.|$').  If you wish to catch just errors thrown within 'Inum's,
-- see the function @'inumCatch'@ in "Data.IterIO.Inum".
--
-- On exceptions, @catchI@ invokes a handler passing it both the
-- exception thrown and the state of the failing 'IterR', which may
-- contain more information than just the exception.  In particular,
-- if the exception occured in an 'Inum', the returned 'IterR' will
-- also contain the 'IterR' being fed by that 'Inum', which likely
-- will not have failed.  To avoid discarding this extra information,
-- you should not re-throw exceptions with 'throwI'.  Rather, you
-- should re-throw an exception by re-executing the failed 'IterR'
-- with 'reRunIter'.  For example, you could define an @onExceptionI@
-- function analogous to the standard library @'onException'@ as
-- follows:
--
-- @
--  onExceptionI iter cleanup =
--      iter \`catchI\` \\('SomeException' _) r -> cleanup >> 'reRunIter' r
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
       -> (e -> IterR t m a -> Iter t m a)
       -- ^ Exception handler, which gets as arguments both the
       -- exception and the failing 'Iter' state.
       -> Iter t m a
catchI iter handler = genCatchI iter handler id

-- | A version of 'catchI' with the arguments reversed, analogous to
-- @'handle'@ in the standard library.  (A more logical name for this
-- function might be @handleI@, but that name is used for the file
-- handle iteratee in "Data.IterIO.ListLike".)
handlerI :: (Exception e, ChunkData t, Monad m) =>
          (e -> IterR t m a -> Iter t m a)
         -- ^ Exception handler
         -> Iter t m a
         -- ^ 'Iter' that might throw an exception
         -> Iter t m a
handlerI = flip catchI

-- | If an 'Iter' succeeds and returns @a@, returns @'Right' a@.  If
-- the 'Iter' throws an exception @e@, returns @'Left' (e, r)@ where
-- @r@ is the state of the failing 'Iter'.
tryI :: (ChunkData t, Monad m, Exception e) =>
        Iter t m a -> Iter t m (Either (e, IterR t m a) a)
tryI iter = genCatchI iter (curry $ return . Left) Right

-- | A version of 'tryI' that returns only the exception, not the
-- result.  The usage is thus identicaly to the standard IO 'try'
-- function, except that the exception cannot be re-thrown without
-- risk of losing information.
tryIe :: (ChunkData t, Monad m, Exception e) =>
         Iter t m a -> Iter t m (Either e a)
tryIe iter = genCatchI iter (\e _ -> return $ Left e) Right

-- | A version of 'tryI' that catches all exceptions.  Instead of
-- returning the exception caught, it returns the failing 'IterR'
-- (from which you can extract the exception if you really want it).
-- The main use of this is for doing some kind of clean-up action,
-- then re-throwing the exception with 'reRunIter'.
--
-- For example, the following is a possible implementation of 'finallyI':
--
-- > finallyI iter cleanup = do
-- >   er <- tryIr iter
-- >   cleanup
-- >   either reRunIter return er
--
tryIr :: (ChunkData t, Monad m) =>
         Iter t m a -> Iter t m (Either (IterR t m a) a)
tryIr iter = genCatchI iter (\(SomeException _) r -> return $ Left r) Right

-- | Execute an 'Iter', then perform a cleanup action regardless of
-- whether the 'Iter' threw an exception or not.  Analogous to the
-- standard library function @'finally'@.
finallyI :: (ChunkData t, Monad m) =>
            Iter t m a -> Iter t m b -> Iter t m a
finallyI iter cleanup = do er <- tryIr iter
                           cleanup >> either reRunIter return er

-- | Execute an 'Iter' and perform a cleanup action if the 'Iter'
-- threw an exception.  Analogous to the standard library function
-- @'onException'@.
onExceptionI :: (ChunkData t, Monad m) =>
                Iter t m a -> Iter t m b -> Iter t m a
onExceptionI iter cleanup =
    catchI iter $ \(SomeException _) r -> cleanup >> reRunIter r

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

-- | 'catchBI' with the arguments reversed.
handlerBI :: (Exception e, ChunkData t, Monad m) =>
             (e -> Iter t m a)
          -- ^ Exception handler
          -> Iter t m a
          -- ^ 'Iter' that might throw an exception
          -> Iter t m a
handlerBI = flip catchBI

-- | Simlar to 'tryI', but saves all data that has been fed to the
-- 'Iter', and rewinds the input if the 'Iter' fails.  (The @B@ in
-- @tryBI@ stands for \"backtracking\".)  Thus, if @tryBI@ returns
-- @'Left' exception@, the next 'Iter' to be invoked will see the same
-- input that caused the previous 'Iter' to fail.  (For this reason,
-- it makes no sense ever to call @'resumeI'@ on the 'Iter' you get
-- back from @tryBI@, which is why @tryBI@ does not return the failing
-- Iteratee the way 'tryI' does.)
--
-- Because @tryBI@ saves a copy of all input, it can consume a lot of
-- memory and should only be used when the 'Iter' argument is known to
-- consume a bounded amount of data.
tryBI :: (ChunkData t, Monad m, Exception e) =>
         Iter t m a -> Iter t m (Either e a)
tryBI = onDoneInput errToEither
    where errToEither (Done a c) _ = Done (Right a) c
          errToEither r c = case fromException $ getIterError r of
                              Just e  -> Done (Left e) c
                              Nothing -> fmap Right r

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
    maybe r (\e' -> mapExceptionR (combine e') r) $ cast e
    where combine (IterExpected saw1 e1) (IterExpected saw2 e2) =
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
        if eof then Done () chunkEOF else IterF nullI

-- | Returns a non-empty amount of input data if there is any input
-- left.  Returns 'mempty' on an end of file condition.
data0I :: (ChunkData t) => Iter t m t
{-# INLINE data0I #-}
data0I = iterF $ \(Chunk d eof) -> Done d (Chunk mempty eof)

-- | Like 'data0I', but always returns non-empty data.  Throws an
-- exception on an EOF condition.
dataI :: (ChunkData t) => Iter t m t
{-# INLINE dataI #-}
dataI = iterF nextChunk
    where nextChunk c@(Chunk d True) | null d = IterFail eoferr c
          nextChunk (Chunk d eof)             = Done d (Chunk mempty eof)
          eoferr = toException $ mkIterEOF "dataI"

-- | A variant of 'dataI' that reads the whole input up to an
-- end-of-file and returns it.
pureI :: (Monad m, ChunkData t) => Iter t m t
pureI = do peekI nullI; Iter $ \(Chunk t _) -> Done t chunkEOF

-- | Returns the next 'Chunk' that either contains non-'null' data or
-- has the EOF bit set.
chunkI :: (Monad m, ChunkData t) => Iter t m (Chunk t)
{-# INLINE chunkI #-}
chunkI = iterF $ \c@(Chunk _ eof) -> Done c (Chunk mempty eof)

-- | Runs an 'Iter' without consuming any input.  (See 'tryBI' if you
-- want to avoid consuming input just when the 'Iter' fails.)
peekI :: (ChunkData t, Monad m) => Iter t m a -> Iter t m a
peekI = onDoneInput setResid

-- | Does not actually consume any input, but returns 'True' if there
-- is no more input data to be had.
atEOFI :: (Monad m, ChunkData t) => Iter t m Bool
atEOFI = iterF $ \c@(Chunk t _) -> Done (null t) c

-- | Place data back onto the input stream, where it will be the next
-- data consumed by subsequent 'Iter's.
ungetI :: (ChunkData t) => t -> Iter t m ()
{-# INLINE ungetI #-}
ungetI t = Iter $ \c -> Done () (mappend (chunk t) c)

-- | Issue a control request, return 'Nothing' if the request is
-- unsupported.  Otherwise, return @'Just' . 'Right'@ the result if
-- the request is supported by enclosing enumerators.  If the request
-- is supported but executing it caused an exception @e@ to be thrown,
-- then returns @'Just' ('Left' e)@.
safeCtlI :: (CtlCmd carg cres, Monad m) =>
            carg -> Iter t m (CtlRes cres)
safeCtlI carg = Iter $ IterC . CtlArg carg return

-- | Issue a control request and return the result.  Throws an
-- exception if the operation type was not supported by an enclosing
-- enumerator.
ctlI :: (CtlCmd carg cres, ChunkData t, Monad m) =>
        carg -> Iter t m cres
ctlI carg = do
  res <- safeCtlI carg
  case res of
    CtlUnsupp    -> throwI $ IterCUnsupp carg
    CtlFail e    -> throwI e
    CtlDone cres -> return cres

--
-- Iter manipulation functions
--

-- | A variant of 'IterR' that only works for the 'IterF' and 'IterC'
-- states, not the 'IterM' state.  (Because of this additional
-- restriction, the input and output 'Monad' types @m1@ and @m2@ do
-- not need to be the same.)
stepR' :: IterR t m1 a
       -- ^ The 'IterR' that needs to be stepped.
       -> (IterR t m1 a -> IterR t m2 b)
       -- ^ Transformation function if the 'IterR' is in the 'IterF'
       -- or 'IterC' state.
       -> IterR t m2 b
       -- ^ Fallback if the 'IterR' is no longer active.
       -> IterR t m2 b
stepR' (IterF (Iter i)) f _       = IterF $ Iter $ f . i
stepR' (IterC (CtlArg a n c)) f _ =
    IterC $ CtlArg a (Iter . (f .) . runIter . n) c
stepR' (IterM _) _ _              = error "stepR' (IterM)"
stepR' _ _ notActive              = notActive

-- | Step an active 'IterR' (i.e., one in the 'IterF', 'IterM', or
-- 'IterC' state) to its next state, and pass the result through a
-- function.
stepR :: (Monad m) =>
         IterR t m a -> (IterR t m a -> IterR t m b) -> IterR t m b
stepR (IterM m) f = IterM $ liftM f m
stepR r f         = stepR' r f (error "stepR")

-- | The equivalent of 'onDone' for 'IterR's.
onDoneR :: (Monad m) =>
           (IterR t m a -> IterR t m b) -> IterR t m a -> IterR t m b
onDoneR f = check
    where check r = if isIterActive r then stepR r check else f r

-- | Run an 'Iter' until it enters the 'Done', 'IterFail', or
-- 'InumFail' state, then use a function to transform the 'IterR'.
onDone :: (Monad m) =>
          (IterR t m a -> IterR t m b) -> Iter t m a -> Iter t m b
onDone f i = Iter $ onDoneR f . runIter i

-- | Like 'onDone', but also keeps a copy of all input consumed.  (The
-- residual input on the 'IterR' returned will be a suffix of the
-- input returned.)
onDoneInput :: (ChunkData t, Monad m) =>
               (IterR t m a -> Chunk t -> IterR t m b)
            -> Iter t m a
            -> Iter t m b
onDoneInput f = Iter . next id
    where next acc iter c =
              let check (IterF i) = IterF $ Iter $ next (acc . mappend c) i
                  check r | isIterActive r = stepR r check
                          | otherwise      = f r $ acc c
              in check $ runIter iter c

getIterError :: IterR t m a -> SomeException
getIterError (IterFail e _)   = e
getIterError (InumFail e _ _) = e
getIterError (IterC _ )       = error "getIterError: no error (in IterC state)"
getIterError (IterM _)        = error "getIterError: no error (in IterM state)"
getIterError (IterF _)        = error "getIterError: no error (in IterF state)"
getIterError _                = error "getIterError: no error to extract"

-- | Get the residual data for an 'IterR' that is in no longer active
-- or that is in the 'IterC' state.  (It is an error to call this
-- function on an 'IterR' in the 'IterF' or 'IterM' state.)
getResid :: IterR t m a -> Chunk t
getResid (Done _ c)             = c
getResid (IterFail _ c)         = c
getResid (InumFail _ _ c)       = c
getResid (IterC (CtlArg _ _ c)) = c
getResid (IterF _)              = error "getResid (IterF)"
getResid (IterM _)              = error "getResid (IterM)"

-- | Set residual data for an 'IterR' that is not active.  (It is an
-- error to call this on an 'IterR' in the 'Done', 'IterM', or 'IterC'
-- states.)
setResid :: IterR t1 m1 a -> Chunk t2 -> IterR t2 m2 a
setResid (Done a _)       = Done a
setResid (IterFail e _)   = IterFail e
setResid (InumFail e a _) = InumFail e a
setResid (IterF _)        = error "setResid (IterF)"
setResid (IterM _)        = error "setResid (IterM)"
setResid (IterC _)        = error "setResid (IterC)"

-- | Feed more input to an 'Iter' that has already been run (and hence
-- is already an 'IterR').  In the event that the 'IterR' is
-- requesting more input (i.e., is in the 'IterF' state), this is
-- straight forward.  However, if the 'Iter' is in some other state
-- such as 'IterM', this function needs to save the input until such
-- time as the 'IterR' is stepped to a new state (e.g., with 'stepR'
-- or 'reRunIter').
runIterR :: (ChunkData t, Monad m) => IterR t m a -> Chunk t -> IterR t m a
runIterR r c = if null c then r else check r
    where check (Done a c0)             = Done a (mappend c0 c)
          check (IterF i)               = runIter i c
          check (IterM m)               = IterM $ liftM check m
          check (IterC (CtlArg a n c0)) = IterC $ CtlArg a n (mappend c0 c)
          check (IterFail e c0)         = IterFail e (mappend c0 c)
          check (InumFail e a c0)       = InumFail e a (mappend c0 c)

-- | Turn an 'IterR' back into an 'Iter'.
reRunIter :: (ChunkData t, Monad m) => IterR t m a -> Iter t m a
reRunIter (IterF i) = i
reRunIter r         = Iter $ runIterR r
