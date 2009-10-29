-- {-# LANGUAGE ForeignFunctionInterface #-}


{-   Alternate Enumerator/Iteratee take by David Mazieres.

     An iteratee is a data sink that is fed chunks of data.  It may
     return a useful result, or its use may like in the side-effects
     it has, such as storing the data to a file.  Iteratees are
     represented by the type @'Iter' t m a@.  @t@ is the type of the
     data chunks (which must be a 'ChunkData', such as 'String' or
     lazy 'L.ByteString').  @m@ is the 'Monad' in which the iteratee
     runs--for instance 'IO' (or an instance of 'MonadIO') for the
     iteratee to perform IO.  @a@ is the result type of the iteratee,
     for when it has consumed enough input to produce a result.

     An Enumerator is a data source that feeds data chunks to an
     iteratee.  There are two types of Enumerator.

       * An /outer enumerator/, represented by the type 'EnumO',
         generates data from some external source, such as IO (or
         potentially some internal state such as a somepseudo-random
         generator).  Outer enumerators are generally constructed
         using 'enumO', which repeatedly runs a computation that
         generates chunks of data.  When the enumerator is out of
         data, data generating computation returns an EOF chunk, and
         'enumO' returns the iteratee so that it can potentially be
         passed to a different enumerator for more data.  (An
         enumerator should not feed 'EOF' to an iteratee--only the
         '|$' operator, 'run', and 'runI' functions do this.)  If the
         iteratee returns a result or fails, the enumerator also
         returns it immediately.

       * An /inner enumerator/, represented by the type 'EnumI', gets
         its data from another enumerator, then feeds this to an
         iteratee.  Thus, an 'EnumI' behaves as an iteratee when
         interfacing to the outer enumerator, and behaves as an
         enumerator when feeding data to some \"inner\" iteratee.
         Inner are build using the function 'enumI', which is
         analogous to 'enumO' for outer enumerators, except that the
         chunk generating computation can use iteratees to process
         data from the outer enumerator.  An inner enumerator, when
         done, returns the inner iteratee's state, as well as its own
         Iteratee state.  An inner enumerator that receives EOF should
         /not/ feed the EOF to its iteratee, as the iteratee may
         subsequently be passed to another enumerator for more input.
         (This is convention is respected by the 'enumI' function.)

    IO is performed by applying an outer enumerator to an iteratee,
    using the '|$' (\"pipe apply\") binary operator.

    An important property of enumerators and iteratees is that they
    can be /fused/.  The '|..' operator fuses an outer enumerator with
    an inner enumerator, yielding an outer enumerator, e.g.,
    @enumo '|..' enumi@.  Similarly, the '..|' operator fuses an inner
    enumerator with an iteratee to yield another iteratee, e.g.,
    @enumi '..|' iter@.  Finally, two inner enumerators may be fused
    into one with the '..|..' operator.

    Enumerators may also be concatenated.  Two outer enumerators may
    be concatenated using the 'cat' function.  Thus,
    @enumO1 ``cat`` enumO2@ produces an outer enumerator whose effect
    is to feed first @enumO1@'s data then @enumO2@'s data to an
    iteratee.  Inner enumerators may similarly be concatenated using
    the 'catI' function.

-}

-- | Enumerator/Iteratee IO abstractions.  See the documentation for
-- "Data.IterIO" for a high-level overview of these abstractions.
module Data.IterIO.Base
    (-- * Base types
     ChunkData(..), Chunk(..), Iter(..), EnumO, EnumI
    -- * Core functions
    , (|$)
    , runIter, run
    , chunk, chunkEOF, isChunkEOF
    -- * Concatenation functions
    , cat, catI
    -- * Fusing operators
    , (|..), (..|..), (..|)
    -- * Enumerator construction functions
    , enumO, enumO', enumObracket, enumI, enumI'
    -- * Predicates on iteratees
    , isIterError, isEnumError
    -- * Other functions
    , iterLoop
    , fixIterPure, fixMonadIO
    -- * Some basic Iteratees
    , throwI, throwEOFI
    , tryI, catchI, handlerI
    , resumeI, verboseResumeI
    , nullI, chunkI
    , wrapI, runI, joinI
    , headI, safeHeadI
    , putI, sendI
    -- * Some basic Enumerators
    , enumPure
    , enumCatch, enumHandler, inumCatch
    , inumNop, inumSplit
    ) where

import Prelude hiding (null)
import qualified Prelude
import Control.Concurrent.MVar
import Control.Exception (SomeException(..), ErrorCall(..), Exception(..)
                         , try, throw)
import Control.Monad
import Control.Monad.Fix
import Control.Monad.Trans
import Data.IORef
import Data.Monoid
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import System.Environment
import System.IO
import System.IO.Error (mkIOError, eofErrorType, isEOFError)
import System.IO.Unsafe

--
-- Iteratee types
--

-- | @ChunkData@ is the class of data types that can be output by an
-- enumerator and iterated on with an iteratee.  A @ChunkData@ type
-- must be a 'Monoid', but must additionally provide a predicate,
-- @null@, for testing whether an object is equal to 'mempty'.
-- Feeding a @null@ chunk to an iteratee followed by any other chunk
-- should have the same effect as just feeding the second chunk,
-- except that some or all of the effects may happen at the time the
-- iteratee receives the @null@ chunk, which is why sometimes the
-- library explicitly feeds @null@ chunks to iteratees.
class (Monoid t) => ChunkData t where
    null :: t -> Bool
instance ChunkData [a] where
    null = Prelude.null
instance ChunkData L.ByteString where
    null = L.null
instance ChunkData S.ByteString where
    null = S.null
instance ChunkData () where
    null _ = True

-- | @Chunk@ is a wrapper around a 'ChunkData' type that also includes
-- an EOF flag that is 'True' if the data is followed by an
-- end-of-file condition.  An iteratee that receives a @Chunk@ with
-- EOF 'True' must return a result (or failure); it is an error to
-- demand more data after an EOF.
data Chunk t = Chunk t Bool deriving (Eq, Show)

-- | Constructor function that builds a chunk containing data and a
-- 'False' EOF flag.
chunk :: t -> Chunk t
chunk t = Chunk t False

-- | An empty chunk with the EOF flag 'True'.
chunkEOF :: (Monoid t) => Chunk t
chunkEOF = Chunk mempty True

-- | Returns True if a chunk is the result of 'chunkEOF' (i.e., has no
-- data and a 'True' EOF bit).
isChunkEOF :: (ChunkData t) => Chunk t -> Bool
isChunkEOF (Chunk t eof) = eof && null t

instance (ChunkData t) => Monoid (Chunk t) where
    mempty = Chunk mempty False
    mappend (Chunk a False) (Chunk b eof) = Chunk (mappend a b) eof
    mappend a (Chunk b True) | null b     = a
    mappend _ _                           = error "mappend to EOF"

instance (ChunkData t) => ChunkData (Chunk t) where
    null (Chunk t False) = null t
    null (Chunk _ True)  = False

-- | The basic Iteratee type is @Iter t m a@, where @t@ is the type of
-- input (in class 'ChunkData'), @m@ is a monad in which the iteratee
-- may execute actions (using the monad transformer 'lift' method),
-- and @a@ is the result type of the iteratee.
--
-- An @Iter@ is in one of three states:  it may require more input, it
-- may have produced a result, or it may have failed.  The first case
-- is signaled by the 'IterF' constructor, which contains a function
-- from a 'Chunk' of data to a new state of the iteratee (in monad
-- @m@).  The second case is signaled by the 'Done' constructor, which
-- returns both a result of type @a@, and a 'Chunk' containing any
-- residual input the iteratee did not consume.  Finally, failure is
-- signaled by either 'IterFail', 'EnumOFail', or 'EnumIFail',
-- depending on whether the failure occured in an iteratee, an outer
-- enumerator, or an inner enumerator.  (In the last two cases, when
-- an enumerator failed, the result also includes the state of the
-- iteratee, which usually has not failed.)
--
-- Note that @Iter t@ is a monad transformer.
data Iter t m a = IterF (Chunk t -> m (Iter t m a))
                -- ^ The iteratee requires more input.
                | Done a (Chunk t)
                -- ^ Sufficient input was received; the iteratee is
                -- returning a result (of type @a@) and a 'Chunk'
                -- containing any unused input.
                | IterFail SomeException
                -- ^ The iteratee failed.
                | EnumOFail SomeException (Iter t m a)
                -- ^ An 'EnumO' failed; the result includes the status
                -- of the iteratee at the time the enumerator failed.
                | EnumIFail SomeException a
                -- ^ An 'EnumI' failed; this result includes status of
                -- the Iteratee.  (The type @a@ will always be
                -- @'Iter' t m a\'@ for some @a'@ in the result of an
                -- 'EnumI'.)

instance (ChunkData t) => Show (Iter t m a) where
    showsPrec _ (IterF _) rest = "IterF _" ++ rest
    showsPrec _ (Done _ (Chunk t eof)) rest =
        "Done _ (Chunk " ++ (if null t then "mempty " else "_ ")
                         ++ show eof ++ ")" ++ rest
    showsPrec _ (IterFail e) rest = "IterFail " ++ show e ++ rest
    showsPrec _ (EnumOFail e i) rest =
        "EnumOFail " ++ show e ++ " (" ++ (shows i $ ")" ++ rest)
    showsPrec _ (EnumIFail e _) rest =
        "EnumIFail " ++ show e ++ " _" ++ rest

{-
instance (Show t, Show a) => Show (Iter t m a) where
    showsPrec _ (IterF _) rest = "IterF" ++ rest
    showsPrec _ (Done a c) rest = "Done (" ++ show a ++ ") " ++ show c ++ rest
    showsPrec _ (IterFail e) rest = "IterFail " ++ show e ++ rest
    showsPrec _ (EnumOFail e i) rest =
        "EnumOFail " ++ show e ++ " (" ++ (shows i $ ")" ++ rest)
    showsPrec _ (EnumIFail e i) rest =
        "EnumIFail " ++ show e ++ " (" ++ (shows i $ ")" ++ rest)
-}

-- | Runs an 'Iter' on a 'Chunk' of data.  When the 'Iter' is
-- already 'Done', or in some error condition, simulates the behavior
-- appropriately.
--
-- Note that this function asserts the following invariants on the
-- behavior of an 'Iter':
--
--     1. An 'Iter' may not return an 'IterF' (asking for more input)
--        if it received a 'Chunk' with the EOF bit 'True'.
--
--     2. An 'Iter' returning 'Done' must not set the EOF bit if it
--        did not receive the EOF bit.
--
runIter :: (ChunkData t, Monad m) =>
           Iter t m a
        -> Chunk t
        -> m (Iter t m a)
runIter (IterF f) c@(Chunk _ eof) = f c >>= setEOF
    where
      setEOF :: (Monad m) => Iter t m a -> m (Iter t m a)
      setEOF (Done a (Chunk t _)) | eof = return $ Done a $ Chunk t eof
      setEOF (Done _ (Chunk _ True)) = error "runIter: IterF returned bogus EOF"
      setEOF (IterF _) | eof = error "runIter: IterF returned after EOF"
      setEOF iter = return iter
runIter (Done a c) c' = return $ Done a (mappend c c')
runIter err _         = return err

instance (ChunkData t, Monad m) => Functor (Iter t m) where
    fmap = liftM

instance (ChunkData t, Monad m) => Monad (Iter t m) where
    return a = Done a mempty
    -- Could get rid of ChunkData requirement with the next definition
    -- return a = IterF $ \c -> return $ Done a c
    m >>= k  = IterF $ \c ->
               do m' <- runIter m c
                  case m' of
                    IterF _   -> return $ m' >>= k
                    Done a c' -> runIter (k a) c'
                    err       -> return $ IterFail $ getIterError err
    fail msg = IterFail $ mkError msg

getIterError                 :: Iter t m a -> SomeException
getIterError (IterFail e)    = e
getIterError (EnumOFail e _) = e
getIterError (EnumIFail e _) = e
getIterError _               = error "getIterError: no error to extract"

-- | True if an iteratee /or/ an enclosing enumerator has experienced
-- a failure.  (@isIterError@ is always 'True' when 'isEnumError' is
-- 'True', but the converse is not true.
isIterError :: Iter t m a -> Bool
isIterError (IterF _)       = False
isIterError (Done _ _)      = False
isIterError _               = True

-- | True if an enumerator enclosing an iteratee has experience a
-- failure (but not if the iteratee itself failed).
isEnumError :: Iter t m a -> Bool
isEnumError (EnumOFail _ _) = True
isEnumError (EnumIFail _ _) = True
isEnumError _               = False

isIterEOFError :: Iter t m a -> Bool
isIterEOFError (IterF _) = False
isIterEOFError (Done _ _) = False
isIterEOFError err = case fromException $ getIterError err of
                       Just e -> isEOFError e
                       _      -> False

mkError :: String -> SomeException
mkError msg = toException $ ErrorCall msg

{- fixIterPure and fixIterIO allow MonadFix instances, which support
   out-of-order name bindings in an "mdo" block, provided your file
   has {-# LANGUAGE RecursiveDo #-} at the top.  A contrived example
   would be:

fixtest :: IO Int
fixtest = enumPure [10] `cat` enumPure [1] |$ fixee
    where
      fixee :: Iter [Int] IO Int
      fixee = mdo
        liftIO $ putStrLn "test #1"
        c <- return $ a + b
        liftIO $ putStrLn "test #2"
        a <- headI
        liftIO $ putStrLn "test #3"
        b <- headI
        liftIO $ putStrLn "test #4"
        return c

-- A very convoluted way of computing factorial
fixtest2 :: Int -> IO Int
fixtest2 i = do
  f <- enumPure [2] `cat` enumPure [1] |$ mfix fact
  run $ f i
    where
      fact :: (Int -> Iter [Int] IO Int)
           -> Iter [Int] IO (Int -> Iter [Int] IO Int)
      fact f = do
               ignore <- headI
               liftIO $ putStrLn $ "ignoring " ++ show ignore
               base <- headI
               liftIO $ putStrLn $ "base is " ++ show base
               return $ \n -> if n <=  0
                              then return base
                              else liftM (n *) (f $ n - 1)
-}

-- | This is a fixed point combinator for iteratees over monads that
-- have no side effects.  If you wish to use @mdo@ with such a monad,
-- you can define an instance of 'MonadFix' in which
-- @'mfix' = fixIterPure@.  However, be warned that this /only/ works
-- when computations in the monad have no side effects, as
-- @fixIterPure@ will repeatedly re-invoke the funciton passsed in
-- when more input is required (thereby also repeating side-effects).
-- For cases in which the monad may have side effects, if the monad is
-- in the 'MonadIO' class then there is already an 'mfix' instance
-- defined using 'fixMonadIO'.
fixIterPure :: (ChunkData t, MonadFix m) =>
               (a -> Iter t m a) -> Iter t m a
fixIterPure f' = dofix mempty f'
    where
      dofix c0 f = IterF $ \c1 -> do
         let c = mappend c0 c1
         iter <- mfix $ \ ~(Done a _) -> runIter (f a) c
         case iter of
           IterF _ -> return $ dofix c f -- Warning: repeats side effects
           _       -> return iter

-- | This is a generalization of 'fixIO' for arbitrary members of the
-- 'MonadIO' class.  
fixMonadIO :: (MonadIO m) =>
              (a -> m a) -> m a
fixMonadIO f = do
  ref <- liftIO $ newIORef $ throw $ mkError "fixMonadIO: non-termination"
  a <- liftIO $ unsafeInterleaveIO $ readIORef ref
  r <- f a
  liftIO $ writeIORef ref r
  return r

instance (ChunkData t, MonadIO m) => MonadFix (Iter t m) where
    mfix f = fixMonadIO f

instance MonadTrans (Iter t) where
    lift m = IterF $ \c -> m >>= return . flip Done c

-- | Lift an IO operation into an 'Iter' monad, but if the IO
-- operation throws an error, catch the exception and return it as a
-- failure of the Iteratee.
instance (ChunkData t, MonadIO m) => MonadIO (Iter t m) where
    liftIO m = do
      result <- lift $ liftIO $ try m
      case result of
        Left err -> IterFail $ toException (err :: IOError)
        Right ok -> return ok

-- | Return the result of an iteratee.  If it is still in the 'IterF'
-- state, feed it an EOF to extract a result.  Throws an exception if
-- there has been a failure.
run :: (ChunkData t, Monad m) => Iter t m a -> m a
run iter@(IterF _)  = runIter iter chunkEOF >>= run
run (Done a _)      = return a
run (IterFail e)    = throw e
run (EnumOFail e _) = throw e
run (EnumIFail e _) = throw e


--
-- Some super-basic Iteratees
--

-- | Throw an exception from an Iteratee.  The exception will be
-- propagated properly through nested Iteratees, which will allow it
-- to be categorized properly and avoid situations in which, for
-- instance, functions holding 'MVar's are prematurely terminated.
-- (Most Iteratee code does not assume the Monad parameter @m@ is in
-- the 'MonadIO' class, and so cannot use 'catch' or 'onException' to
-- clean up after exceptions.)  Use 'throwI' in preference to 'throw'
-- whenever possible.
throwI :: (Exception e, Monad m) => e -> Iter t m a
throwI e = IterFail $ toException e

-- | Throw an 'IOError' of type EOF, which will be interpreted by
-- 'enumO' and 'enumI' as an end of file chunk when thrown by the
-- generator/codec.
throwEOFI :: (Monad m) => String -> Iter t m a
throwEOFI loc = throwI $ mkIOError eofErrorType loc Nothing Nothing

-- | If an 'Iter' succeeds and returns @a@, returns @'Right' a@.  If
-- the 'Iter' throws an exception @e@, returns @'Left' e@.
tryI :: (ChunkData t, Monad m, Exception e) =>
        Iter t m a
     -> Iter t m (Either (e, Iter t m a) a)
tryI = wrapI errToEiter
    where
      errToEiter (Done a c) = Done (Right a) c
      errToEiter iter       = case fromException $ getIterError iter of
                                Just e  -> return $ Left (e, iter)
                                Nothing -> fixError iter
      fixError (EnumIFail e i) = EnumIFail e $ Right i
      fixError (EnumOFail e i) = EnumOFail e $ liftM Right i
      fixError iter            = IterFail $ getIterError iter

-- | Catch an exception thrown by an 'Iter'.  Returns the failed
-- 'Iter' state, which may contain more information than just the
-- exception.  For instance, if the exception occured in an
-- enumerator, the returned 'Iter' will also contain an inner 'Iter'
-- that has not failed.  To avoid discarding this extra information,
-- you should not re-throw exceptions with 'throwI'.  Rather, you
-- should re-throw an exception by re-executing the failed 'Iter'.
-- For example, you could define an @onExceptionI@ function analogous
-- to the standard library 'onException' as follows:
--
-- @
--  onExceptionI iter cleanup =
--      iter \`catchI\` \('SomeException' _) iter' -> cleanup >> iter'
-- @
--
-- If you wish to continue processing the iteratee after a failure in
-- an enumerator, use the 'resumeI' function.  For example:
--
-- @
--  action \`catchI\` \('SomeException' e) iter ->
--      if 'isEnumError' iter
--        then do liftIO $ putStrLn $ \"ignoring enumerator failure: \" ++ show e
--                'resumeI' iter
--        else iter
-- @
--
-- @catchI@ catches both iteratee and enumerator failures.  However,
-- because enumerators are functions on iteratees, you must apply
-- @catchI@ to the /result/ of executing an enumerator.  For example,
-- the following code modifies 'enumPure' to catch and ignore an
-- exception thrown by a failing 'Iter':
--
-- > catchTest1 :: IO ()
-- > catchTest1 = myEnum |$ fail "bad Iter"
-- >     where
-- >       myEnum :: EnumO String IO ()
-- >       myEnum iter = catchI (enumPure "test" iter) handler
-- >       handler (SomeException _) iter = do
-- >         liftIO $ hPutStrLn stderr "ignoring exception"
-- >         return ()
--
-- Note that @myEnum@ is an 'EnumO', but it actually takes an
-- argument, @iter@, reflecting the usually hidden fact that 'EnumO's
-- are actually functions.  Thus, @catchI@ is wrapped around the
-- result of applying @'enumPure' \"test\"@ to an 'Iter'.
--
-- Another subtlety to keep in mind is that, when fusing enumerators,
-- the type of the outer enumerator must reflect the fact that it is
-- wrapped around an inner numerator.  Consider the following test, in
-- which an exception thrown by an inner enumerator is caught:
--
-- > inumBad :: (ChunkData t, Monad m) => EnumI t t m a
-- > inumBad = enumI $ fail "inumBad"
-- > 
-- > catchTest2 :: IO ()
-- > catchTest2 = myEnum |.. inumBad |$ nullI
-- >     where
-- >       myEnum :: EnumO String IO (Iter String IO ())
-- >       myEnum iter = catchI (enumPure "test" iter) handler
-- >       handler (SomeException _) iter = do
-- >         liftIO $ hPutStrLn stderr "ignoring exception"
-- >         return $ return ()
--
-- Note the type of @myEnum :: EnumO String IO (Iter String IO ())@
-- reflects that it has been fused to an inner enumerator.  Usually
-- these enumerator result types are computed automatically and you
-- don't have to worry about them as long as your enumreators are
-- polymorphic in the result type.  However, to return a result that
-- suppresses the exception here, we must run @return $ return ()@,
-- invoking @return@ twice, once to create an @Iter String IO ()@, and
-- a second time to create an @Iter String IO (Iter String IO ())@.
-- (To avoid such nesting proliferation in 'EnumO' types, it is
-- sometimes easier to fuse multiple 'EnumI's together with '..|..',
-- or to fuse an 'EnumI' to the 'Iter' with '..|', though the latter
-- option changes the exception semantics somewhat.)
--
-- If you are only interested in catching enumerator failures, see the
-- functions 'enumCatch' and `inumCatch`, which catch enumerator but
-- not iteratee failures.
--
-- Note that @catchI@ only works for /synchronous/ exceptions, such as
-- IO errors (thrown within 'liftIO' blocks), the monadic 'fail'
-- operation, and exceptions raised by 'throwI'.  It is not possible
-- to catch /asynchronous/ exceptions, such as lazily evaluated
-- divide-by-zero errors, the 'throw' function, or exceptions raised
-- by other threads using @'throwTo'@.
catchI :: (Exception e, ChunkData t, Monad m) =>
          Iter t m a
       -- ^ 'Iter' that might throw an exception
       -> (e -> Iter t m a -> Iter t m a)
       -- ^ Exception handler, which gets as arguments both the
       -- exception and the failing 'Iter' state.
       -> Iter t m a
catchI iter handler = wrapI check iter
    where
      check iter'@(IterF _)  = catchI iter' handler
      check iter'@(Done _ _) = iter'
      check err              = case fromException $ getIterError err of
                                 Just e  -> handler e err
                                 Nothing -> err

-- | A version of 'catchI' with the arguments reversed, analogous to
-- @'handle'@ in the standard library.  (A more logical name for this
-- function might be @handleI@, but that name is used for the file
-- handle iteratee.)
handlerI :: (Exception e, ChunkData t, Monad m) =>
          (e -> Iter t m a -> Iter t m a)
       -- ^ Exception handler
       -> Iter t m a
       -- ^ 'Iter' that might throw an exception
       -> Iter t m a
handlerI = flip catchI

-- | Used in an exception handler, after an enumerator fails, to
-- resume processing of the 'Iter' by the next enumerator in a
-- concatenated series.  See 'catchI' for an example.
resumeI :: (ChunkData t, Monad m) => Iter t m a -> Iter t m a
resumeI (EnumOFail _ iter) = iter
resumeI (EnumIFail _ iter) = return iter
resumeI iter               = iter

-- | Like 'resumeI', but if the failure was in an enumerator and the
-- iteratee is resumable, prints an error message to standard error
-- before invoking 'resumeI'.
verboseResumeI :: (ChunkData t, MonadIO m) => Iter t m a -> Iter t m a
verboseResumeI iter | isEnumError iter = do
  prog <- liftIO $ getProgName
  liftIO $ hPutStrLn stderr $ prog ++ ": " ++ show (getIterError iter)
  resumeI iter
verboseResumeI iter = iter

-- | Sinks data like @\/dev\/null@, returning @()@ on EOF.
nullI :: (Monad m, Monoid t) => Iter t m ()
nullI = IterF $ return . check
    where
      check (Chunk _ True) = Done () chunkEOF
      check _              = nullI

-- | Returns a non-empty 'Chunk' or an EOF 'Chunk'.
chunkI :: (Monad m, ChunkData t) => Iter t m (Chunk t)
chunkI = IterF $ \c@(Chunk _ eof) -> return $
         if null c then chunkI else Done c (Chunk mempty eof)

-- | Wrap a function around an 'Iter' to transform its result.  The
-- 'Iter' will be fed 'Chunk's as usual for as long as it remains in
-- the 'IterF' state.  When the 'Iter' enters a state other than
-- 'IterF', @wrapI@ passes it through the tranformation function.
wrapI :: (ChunkData t, Monad m) =>
         (Iter t m a -> Iter t m b) -- ^ Transformation function
      -> Iter t m a                 -- ^ Original 'Iter'
      -> Iter t m b                 -- ^ Returns an 'Iter' whose
                                    -- result will be transformed by
                                    -- the transformation function
wrapI f iter@(IterF _) =
    IterF $ \c@(Chunk _ eof) -> runIter iter c >>= rewrap eof
    where
      rewrap _ iter'@(IterF _) = return $ wrapI f iter'
      rewrap eof iter'         =
          case f iter' of
            i@(IterF _) -> runIter i (Chunk mempty eof)
            i           -> return i
wrapI f iter = f iter

-- | Runs an Iteratee from within another iteratee (feeding it EOF if
-- it is in the 'IterF' state) so as to extract a return value.  The
-- return value is lifted into the invoking Iteratee monadic type.  If
-- the iteratee being run fails, then @runI@ will propagate the
-- failure by also failing.  In the event that the failure is an
-- enumerator failure (either 'EnumIFail' or 'EnumOFail'), @runI@
-- returns an 'EnumIFail' failure and includes the state of the
-- iteratee.
runI :: (ChunkData t1, ChunkData t2, Monad m) =>
        Iter t1 m a
     -> Iter t2 m a
runI iter@(IterF _)  = lift (runIter iter chunkEOF) >>= runI
runI (Done a _)      = return a
runI (IterFail e)    = IterFail e
runI (EnumIFail e i) = EnumIFail e i
runI (EnumOFail e i) = runI i >>= EnumIFail e

-- | Pop an 'Iter' back out of an 'EnumI', propagating any failure.
-- Any enumerator failure ('EnumIFail' or 'EnumOFail') will be
-- translated to an 'EnumOFail' state.
joinI :: (ChunkData tIn, ChunkData tOut, Monad m) =>
         Iter tOut m (Iter tIn m a)
      -> Iter tIn m a
joinI iter@(IterF _)  = lift (runIter iter chunkEOF) >>= joinI
joinI (Done i _)      = i
joinI (IterFail e)    = IterFail e
joinI (EnumIFail e i) = EnumOFail e i
joinI (EnumOFail e i) = EnumOFail e $ joinI i

-- | Allows you to look at the state of an 'Iter' by returning it into
-- an 'Iter' monad.  This is just like the monadic 'return' method,
-- except that, if the 'Iter' is in the 'IterF' state, then @returnI@
-- additionally feeds it an empty chunk.  Thus 'Iter's that do not
-- require data, such as @returnI $ liftIO $ ...@, will execute and
-- return a result (possibly reflecting exceptions) immediately.
returnI :: (ChunkData tOut, ChunkData tIn, Monad m) =>
           Iter tIn m a
        -> Iter tOut m (Iter tIn m a)
returnI iter@(IterF _) =
    IterF $ \c -> runIter iter mempty >>= return . flip Done c
returnI iter = return iter

-- | Return the the first element when the Iteratee data type is a list.
headI :: (Monad m) => Iter [a] m a
headI = IterF $ return . dohead
    where
      dohead (Chunk [] True)    = throwEOFI "headI"
      dohead (Chunk [] _)       = headI
      dohead (Chunk (a:as) eof) = Done a $ Chunk as eof

-- | Return 'Just' the the first element when the Iteratee data type
-- is a list, or 'Nothing' on EOF.
safeHeadI :: (Monad m) => Iter [a] m (Maybe a)
safeHeadI = IterF $ return . dohead
    where
      dohead c@(Chunk [] True)  = Done Nothing c
      dohead (Chunk [] _)       = safeHeadI
      dohead (Chunk (a:as) eof) = Done (Just a) $ Chunk as eof

-- | An Iteratee that puts data to a consumer function, then calls an
-- eof function.  For instance, @'handleI'@ could be defined as:
--
-- > handleI :: (MonadIO m) => Handle -> Iter L.ByteString m ()
-- > handleI h = putI (liftIO . L.hPut h) (liftIO $ hShutdown h 1)
--
putI :: (ChunkData t, Monad m) =>
        (t -> Iter t m a)
     -> Iter t m b
     -> Iter t m ()
putI putfn eoffn = do
  Chunk t eof <- chunkI
  unless (null t) $ putfn t >> return ()
  if eof then eoffn >> return () else putI putfn eoffn

-- | Send datagrams using a supplied function.  The datagrams are fed
-- as a list of packets, where each element of the list should be a
-- separate datagram.
sendI :: (Monad m) =>
         (t -> Iter [t] m a)
      -> Iter [t] m ()
sendI sendfn = do
  dgram <- safeHeadI
  case dgram of
    Just pkt -> sendfn pkt >> sendI sendfn
    Nothing  -> return ()

--
-- Enumerator types
--

-- | An @EnumO t m a@ is an outer enumerator that gets data of type
-- @t@ by executing actions in monad @m@, then feeds the data in
-- chunks to an iteratee of type @'Iter' t m a@.  Most enumerators are
-- polymorphic in the last type, @a@, so as work with iteratees
-- returning any type.
--
-- An @EnumO@ is a function from iteratees to iteratees.  It
-- transforms an iteratee by repeatedly feeding it input until one of
-- four outcomes:  the iteratee returns a result, the iteratee fails,
-- the @EnumO@ runs out of input, or the @EnumO@ fails.  When one of
-- these four termination conditions holds, the @EnumO@ returns the
-- new state of the iteratee.
--
-- Under no circumstances should an @EnumO@ ever feed a chunk with the
-- EOF bit set to an iteratee.  When the @EnumO@ runs out of data, it
-- must simply return the current state of the iteratee.  This way
-- more data from another source can still be fed to the iteratee, as
-- happens when enumerators are concatenated with the 'cat' function.
--
-- @EnumO@s should generally constructed using the 'enumO' function,
-- which handles most of the error-handling details.
type EnumO t m a = Iter t m a -> Iter t m a

-- | Concatenate two outer enumerators, forcing them to be executed in
-- turn in the monad @m@.  Note that the deceptively simple definition:
--
--  >  cat a b = b . a
--
-- wouldn't necessarily do the right thing, as in this case @a@'s
-- monadic actions would not actually get to run until @b@ executess
-- a, and @b@ might start out, before feeding any input to its
-- iteratee, by waiting for some event that is triggered by a
-- side-effect of @a@.  Has fixity:
--
-- > infixr 3 `cat`
cat :: (Monad m, ChunkData t) => EnumO t m a -> EnumO t m a -> EnumO t m a
cat a b iter = do
  iter' <- returnI $ a iter
  case iter' of
    IterF _ -> b iter'
    _       -> iter'
infixr 3 `cat`

-- | Run an outer enumerator on an iteratee.  Any errors in inner
-- enumerators that have been fused to the iteratee (in the second
-- argument of @|$@) will be considered iteratee failures.  Any
-- failures that are not caught by 'catchI', 'enumCatch', or
-- 'inumCatch' will be thrown as exceptions.  Has fixity:
--
-- > infixr 2 |$
(|$) :: (ChunkData t, Monad m) => EnumO t m a -> Iter t m a -> m a
(|$) enum iter = run $ enum $ wrapI (>>= return) iter
-- The purpose of the wrapI (>>= return) is to convert any EnumIFail
-- (or, less likely, EnumOFail) errors thrown by iter to IterFail
-- errors, so that enumCatch statements only catch enumerator
-- failures.
infixr 2 |$

-- | An inner enumerator or transcoder.  Such a function accepts data
-- from some outer enumerator (acting like an Iteratee), then
-- transcodes the data and feeds it to another Iter (hence also acting
-- like an enumerator towards that inner Iter).  Note that data is
-- viewed as flowing inwards from the outermost enumerator to the
-- innermost iteratee.  Thus @tOut@, the \"outer type\", is actually
-- the type of input fed to an @EnumI@, while @tIn@ is what the
-- @EnumI@ feeds to an iteratee.
--
-- As with @EnumO@, an @EnumI@ is a function from iteratees to
-- iteratees.  However, an @EnumI@'s input and output types are
-- different.  A simpler alternative to @EnumI@ might have been:
--
-- > type EnumI' tOut tIn m a = Iter tIn m a -> Iter tOut m a
--
-- In fact, given an @EnumI@ object @enumI@, it is possible to
-- construct such a function as @(enumI '..|')@.  But sometimes one
-- might like to concatenate @EnumI@s.  For instance, consider a
-- network protocol that changes encryption or compression modes
-- midstream.  Transcoding is done by @EnumI@s.  To change transcoding
-- methods after applying an @EnumI@ to an iteratee requires the
-- ability to \"pop\" the iteratee back out of the @EnumI@ so as to be
-- able to hand it to another @EnumI@.  The 'joinI' function provides
-- this functionality in its most general form, though if one only
-- needs 'EnumI' concatenation, the simpler 'catI' function serves
-- this purpose.
--
-- As with 'EnumO's, an @EnumI@ must never feed an EOF chunk to its
-- iteratee.  Instead, upon receiving EOF, the @EnumI@ should simply
-- return the state of the inner iteratee (this is how \"popping\" the
-- iteratee back out works).  An @EnumI@ should also return when the
-- iteratee returns a result or fails, or when the @EnumI@ fails.  An
-- @EnumI@ may return the state of the iteratee earlier, if it has
-- reached some logical message boundary (e.g., many protocols finish
-- processing headers upon reading a blank line).
--
-- @EnumI@s are generally constructed with the 'enumI' function, which
-- hides most of the error handling details.
type EnumI tOut tIn m a = Iter tIn m a -> Iter tOut m (Iter tIn m a)

-- | Concatenate two inner enumerators.  Has fixity:
--
-- > infixr 3 `catI`
catI :: (ChunkData tOut, ChunkData tIn, Monad m) =>
        EnumI tOut tIn m a      -- ^
     -> EnumI tOut tIn m a
     -> EnumI tOut tIn m a
catI a b = a >=> b
infixr 3 `catI`

-- | Fuse an outer enumerator, producing chunks of some type @tOut@,
-- with an inner enumerator that transcodes @tOut@ to @tIn@, to
-- produce a new outer enumerator producing chunks of type @tIn@.  Has
-- fixity:
--
-- > infixl 4 |..
(|..) :: (ChunkData tOut, ChunkData tIn, Monad m) =>
         EnumO tOut m (Iter tIn m a) -- ^
      -> EnumI tOut tIn m a
      -> EnumO tIn m a
(|..) outer inner iter = joinI $ outer $ inner iter
infixl 4 |..

-- | Fuse two inner enumerators into one.  Has fixity:
--
-- > infixl 5 ..|..
(..|..) :: (ChunkData tOut, ChunkData tMid, ChunkData tIn, Monad m) => 
           EnumI tOut tMid m (Iter tIn m a) -- ^
        -> EnumI tMid tIn m a
        -> EnumI tOut tIn m a
(..|..) outer inner iter = wrapI (return . joinI . joinI) $ outer $ inner iter
infixl 5 ..|..

-- | Fuse an inner enumerator that transcodes @tOut@ to @tIn@ with an
-- iteratee taking type @tIn@ to produce an iteratee taking type
-- @tOut@.  Has fixity:
--
-- > infixr 4 ..|
(..|) :: (ChunkData tOut, ChunkData tIn, Monad m
         , Show tOut, Show tIn, Show a) =>
         EnumI tOut tIn m a     -- ^
      -> Iter tIn m a
      -> Iter tOut m a
(..|) inner iter = wrapI (runI . joinI) $ inner iter
infixr 4 ..|

-- | Build an 'EnumO' from a @before@ action, an @after@ function, and
-- an @input@ function in a manner analogous to the IO 'bracket'
-- function.  For instance, you could implement `enumFile'` as
-- follows:
--
-- >   enumFile' :: (MonadIO m) => FilePath -> EnumO L.ByteString m a
-- >   enumFile' path =
-- >     enumObracket (liftIO $ openFile path ReadMode) (liftIO . hClose) doGet
-- >       where
-- >         doGet h = do
-- >           buf <- liftIO $ L.hGet h 8192
-- >           if (L.null buf)
-- >             then return chunkEOF
-- >             else return $ chunk buf
--
enumObracket :: (Monad m, ChunkData t) =>
                (Iter () m b)
             -- ^ Before action
             -> (b -> Iter () m c)
             -- ^ After action, as function of before action result
             -> (b -> Iter () m (Chunk t))
             -- ^ Chunk generating function, as a funciton of before
             -- aciton result
             -> EnumO t m a
enumObracket before after input iter = do
  eb <- tryI $ runI before
  case eb of
    Left (e,_) -> EnumOFail e iter
    Right b    -> do
            iter' <- returnI $ enumO (input b) iter
            ec <- tryI $ runI (after b)
            case ec of
              Left (e,_) | not $ isIterError iter' -> EnumOFail e iter'
              _                                    -> iter'

-- | Construct an outer enumerator given a function that produces
-- 'Chunk's of type @t@.
enumO :: (Monad m, ChunkData t) =>
         Iter () m (Chunk t)
         -- ^ This is the computation that produces input.  It is run
         -- with EOF, and never gets fed any input.  The type of this
         -- argument could alternatively have been just @m t@, but
         -- then there would be no way to signal failure.  (We don't
         -- want to assume @m@ is a member of @MonadIO@; thus we
         -- cannot catch exceptions that aren't propagated via monadic
         -- types.)
      -> EnumO t m a
         -- ^ Returns an outer enumerator that feeds input chunks
         -- (obtained from the first argument) into an iteratee.
enumO input iter@(IterF _) = do
  input' <- lift $ runIter input chunkEOF
  case input' of
    Done (Chunk t eof) _ ->
        lift (runIter iter $ chunk t) >>= if eof then id else enumO input
    _ | isIterEOFError input' -> iter
    _ -> EnumOFail (getIterError input') iter
enumO _ iter = iter

-- | Like 'enumO', but the input function returns raw data, not
-- 'Chunk's.  The only way to signal EOF is therefore to raise an
-- EOF exception.
enumO' :: (Monad m, ChunkData t) =>
          Iter () m t
       -> EnumO t m a
enumO' input iter = enumO (liftM (flip Chunk False) input) iter

-- | Build an inner enumerator given a codec 'Iter' that returns
-- chunks of the appropriate type.  Makes an effort to send an EOF to
-- the codec if the inner 'Iter' fails, so as to facilitate cleanup.
-- However, if a containing 'EnumO' or 'EnumI' fails, code handling
-- that failure will have to send an EOF or the codec will not be able
-- to clean up.
enumI :: (Monad m, ChunkData tOut, ChunkData tIn) =>
         Iter tOut m (Chunk tIn)
      -- ^ This Iteratee will be executed repeatedly to produce
      -- transcoded chunks.
      -> EnumI tOut tIn m a
enumI codec0 = enumI1 codec0
    where
      enumI1 codec iter@(IterF _) = IterF $ feedCodec codec iter
      enumI1 _ iter               = return iter
      feedCodec codec iter cOut@(Chunk _ eof) = do
        codec' <- runIter codec cOut
        case codec' of
          IterF _ -> return $ enumI1 codec' iter
          Done (Chunk tIn eof') cOut' -> do
                iter' <- runIter iter $ chunk tIn
                case iter' of
                  _ | eof || eof' -> return $ Done iter' cOut'
                  IterF _ | null cOut' -> return $ enumI1 codec0 iter'
                  IterF _ -> feedCodec codec0 iter' cOut'
                  _ -> do codec'' <- runIter codec' chunkEOF
                          if isIterError codec'' && not (isIterEOFError codec'')
                            then return $ EnumIFail (getIterError codec'') iter'
                            else return $ Done iter' cOut'
          _ | isIterEOFError codec' -> return $ return iter
          _ -> return $ EnumIFail (getIterError codec') iter

-- | Transcode (until codec throws an EOF error, or until after it has
-- received EOF).
enumI' :: (Monad m, ChunkData tOut, ChunkData tIn) =>
          Iter tOut m tIn
       -- ^ This Iteratee will be executed repeatedly to produce
       -- transcoded chunks.
       -> EnumI tOut tIn m a
enumI' codec iter = enumI (liftM (flip Chunk False) codec) iter

--
-- Basic outer enumerators
--

-- | An 'EnumO' that will feed pure data to 'Iter's.
enumPure :: (Monad m, ChunkData t) => t -> EnumO t m a
enumPure t = enumO $ return $ Chunk t True

-- | Like 'catchI', but applied to 'EnumO's and 'EnumI's instead of
-- 'Iter's, and does not catch errors thrown by 'Iter's.
--
-- There are three 'catch'-like functions in the iterIO library,
-- catching varying numbers of types of failures.  @inumCatch@ is the
-- middle option.  By comparison:
--
-- * 'catchI' catches the most errors, including those thrown by
--   'Iter's.  'catchI' can be applied to 'Iter's, 'EnumI's, or
--   'enumO's, and is useful both to the left and to the right of
--   '|$'.
--
-- * @inumCatch@ catches 'EnumI' or 'EnumO' failures, but not 'Iter'
--   failures.  It can be applied to 'EnumI's or 'EnumO's, to the left
--   or to the right of '|$'.  When applied to the left of '|$', will
--   not catch any errors thrown by 'EnumI's to the right of '|$'.
--
-- * 'enumCatch' only catches 'EnumO' failures, and should only by
--   applied to the left of '|$'.  (You /can/ apply 'enumCatch' to
--   'EnumI's or to the right of '|$', but this is not useful because
--   it ignores 'Iter' and 'EnumI' failures so won't catch anything.)
--
-- One potentially unintuitive apsect of @inumCatch@ is that, when
-- applied to an enumerator, it catches any enumerator failure to the
-- right that is on the same side of '|$'--even enumerators not
-- lexically scoped within the argument of @inumCatch@.  See
-- 'enumCatch' for some examples of this behavior.
inumCatch :: (Exception e, ChunkData t, Monad m) =>
              EnumO t m a
           -- ^ 'EnumO' that might throw an exception
           -> (e -> Iter t m a -> Iter t m a)
           -- ^ Exception handler
           -> EnumO t m a
inumCatch enum handler = wrapI check . enum
    where
      check iter'@(Done _ _)   = iter'
      check iter'@(IterFail _) = iter'
      check err                = case fromException $ getIterError err of
                                   Just e  -> handler e err
                                   Nothing -> err

-- | Like 'catchI', but for 'EnumO's instead of 'Iter's.  Catches
-- errors thrown by an 'EnumO', but /not/ those thrown by 'EnumI's
-- fused to the 'EnumO' after @enumCatch@ has been applied, and not
-- exceptions thrown from an 'Iter'.  If you want to catch all
-- enumerator errors, including those from subsequently fused
-- 'EnumI's, see the `inumCatch` function.  For example, comare
-- @test1@ (which throws an exception) to @test2@ and @test3@ (which
-- do not):
--
-- >    inumBad :: (ChunkData t, Monad m) => EnumI t t m a
-- >    inumBad = enumI $ fail "inumBad"
-- >    
-- >    skipError :: (ChunkData t, MonadIO m) =>
-- >                 SomeException -> Iter t m a -> Iter t m a
-- >    skipError e iter = do
-- >      liftIO $ hPutStrLn stderr $ "skipping error: " ++ show e
-- >      resumeI iter
-- >    
-- >    -- Throws an exception
-- >    test1 :: IO ()
-- >    test1 = enumCatch (enumPure "test") skipError |.. inumBad |$ nullI
-- >    
-- >    -- Does not throw an exception, because inumCatch catches all
-- >    -- enumerator errors on the same side of '|$', including from
-- >    -- subsequently fused inumBad.
-- >    test2 :: IO ()
-- >    test2 = inumCatch (enumPure "test") skipError |.. inumBad |$ nullI
-- >    
-- >    -- Does not throw an exception, because enumCatch was applied
-- >    -- after inumBad was fused to enumPure.
-- >    test3 :: IO ()
-- >    test3 = enumCatch (enumPure "test" |.. inumBad) skipError |$ nullI
--
-- Note that both @\`enumCatch\`@ and ``inumCatch`` have the default
-- infix precedence (9), which binds more tightly than any
-- concatenation or fusing operators.
enumCatch :: (Exception e, ChunkData t, Monad m) =>
              EnumO t m a
           -- ^ 'EnumO' that might throw an exception
           -> (e -> Iter t m a -> Iter t m a)
           -- ^ Exception handler
           -> EnumO t m a
enumCatch enum handler = wrapI check . enum
    where
      check iter@(EnumOFail e _) =
          case fromException e of
            Just e' -> handler e' iter
            Nothing -> iter
      check iter = iter

-- | 'enumCatch' with the argument order switched.
enumHandler :: (Exception e, ChunkData t, Monad m) =>
               (e -> Iter t m a -> Iter t m a)
            -- ^ Exception handler
            -> EnumO t m a
            -- ^ 'EnumO' that might throw an exception
            -> EnumO t m a
enumHandler = flip enumCatch

-- | Create a loopback @('Iter', 'EnumO')@ pair.  The iteratee and
-- enumerator can be used in different threads.  Any data fed into the
-- 'Iter' will in turn be fed by the 'EnumO' into whatever 'Iter' it
-- is given.  This is useful for testing a protocol implementation
-- against itself.
iterLoop :: (MonadIO m, ChunkData t, Show t) =>
            m (Iter t m (), EnumO t m a)
iterLoop = do
  -- The loopback is implemented with an MVar (MVar Chunk).  The
  -- enumerator waits on the inner MVar, while the iteratee uses the outer 
  -- iteratee to avoid races when appending to the stored chunk.
  pipe <- liftIO $ newEmptyMVar >>= newMVar
  return (IterF $ iterf pipe, enum pipe)
    where
      iterf pipe c@(Chunk _ eof) = do
             liftIO $ withMVar pipe $ \p ->
                 do mp <- tryTakeMVar p
                    putMVar p $ case mp of
                                  Nothing -> c
                                  Just c' -> mappend c' c
             return $ if eof
                      then Done () chunkEOF
                      else IterF $ iterf pipe

      enum pipe = enumO $ do
             p <- liftIO $ readMVar pipe
             c <- liftIO $ takeMVar p
             return c

--
-- Basic inner enumerators
--

-- | The null 'EnumI', which passes data through to another iteratee
-- unmodified.
inumNop :: (ChunkData t, Monad m) => EnumI t t m a
inumNop = enumI chunkI

-- | Returns an 'Iter' that always returns itself until a result is
-- produced.  You can fuse @inumSplit@ to an 'Iter' to produce an
-- 'Iter' that can safely be written from multiple threads.
inumSplit :: (MonadIO m, ChunkData t) => EnumI t t m a
inumSplit iter1 = do
  mv <- liftIO $ newMVar $ iter1
  IterF $ iterf mv
    where
      iterf mv (Chunk t eof) = do
        rold <- liftIO $ takeMVar mv
        rnew <- runIter rold $ chunk t
        liftIO $ putMVar mv rnew
        return $ case rnew of
                   IterF _ | not eof -> IterF $ iterf mv
                   _                 -> return rnew


