-- {-# LANGUAGE ForeignFunctionInterface #-}


{- | Alternate Enumerator/Iteratee take by David Mazieres.

     An iteratee is a data sink that is fed chunks of data.  It may
     return a useful result, or its use may be in the side-effects it
     has, such as storing the data to a file.  Iteratees are
     represented by the type @'Iter' t m a@.  @t@ is the type of the
     data chunks (which must be a 'ChunkData', such as 'String' or lazy
     'ByteString').  @m@ is the 'Monad' in which the iteratee
     runs--for instance 'IO' (or an instance of 'MonadIO') for the
     iteratee to perform IO.  'a' is the result type of the iteratee,
     for when it has consumed enough input to produce a result.

     An Enumerator is a data source that feeds data chunks to an
     iteratee.  There are two types of Enumerator.

       * An /outer enumerator/, represented by the type 'EnumO',
         generates data from some external source, such as IO (or
         potentially some internal state such as a somepseudo-random
         generator).  An outer enumerator use the 'feedO' function to
         feed data chunks to an iteratee.  When the enumerator is out
         of data, it returns the iteratee so that the iteratee can
         potentially be passed to a different enumerator for more
         data.  (An enumerator should not feed 'EOF' to an
         iteratee--only the 'run' and 'runI' functions do this.)  If
         the iteratee returns a result, the enumerator also returns it
         immediately.

       * An /inner enumerator/, represented by the type 'EnumI', gets
         its data from another enumerator, then feeds this to an
         iteratee.  Thus, an 'EnumI' appears as an iteratee to the
         outer enumerator, and apears as an enumerator to some "inner"
         iteratee to which it is feeding data.  Inner enumerators use
         the function 'feedI' to feed data chunks to an iteratee.  An
         inner enumerator, when done, returns the inner iteratee's
         state, as well as its own Iteratee state.  An inner
         enumerator that receives EOF should /not/ feed the EOF to its
         iteratee, as the iteratee may subsequently be passed to
         another enumerator for more input.  This is convention is
         respected by the 'feedI' function, which takes data rather
         than a chunk.

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
    , enumCatch, enumHandler
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

-- | ChunkData is a 'Monoid' type that additionally has a predicate
-- for testing whether an object is equal to 'mempty'.
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

-- | A chunk of data, plus a flag which is 'True' if the data is
-- followed by an end-of-file.
data Chunk t = Chunk t Bool deriving (Eq, Show)

chunk :: t -> Chunk t
chunk t = Chunk t False

chunkEOF :: (Monoid t) => Chunk t
chunkEOF = Chunk mempty True

-- | Returns True if a chunk has no data and its EOF bit is true.
isChunkEOF :: (ChunkData t) => Chunk t -> Bool
isChunkEOF (Chunk t eof) = eof && null t

instance (ChunkData t) => Monoid (Chunk t) where
    mempty = Chunk mempty False
    mappend (Chunk a False) (Chunk b eof) = Chunk (mappend a b) eof
    mappend a (Chunk b True) | null b     = a
    mappend _ _                           = error "mappend to EOF"

instance (ChunkData t) => ChunkData (Chunk t) where
    null (Chunk t _) = null t

-- | The basic Iteratee type.  An @Iter@ is a function that iterates
-- over input, potentially returning a result of type @a@.  @t@ is the
-- type of input chunks.  Note that @Iter t@ is a monad transformer.
data Iter t m a = IterF (Chunk t -> m (Iter t m a))
                -- ^ The iteratee required more input
                | Done a (Chunk t)
                -- ^ Sufficient input was received; the iteratee is
                -- returning both a result and any unused input.
                | IterFail SomeException
                -- ^ The iteratee failed
                | EnumOFail SomeException (Iter t m a)
                -- ^ Enumerator failed, includes status of Iteratee
                | EnumIFail SomeException a
                -- ^ Inner enumerator failed, includes status of Iteratee

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
-- state, feed it an EOF to extract a result.
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
     -> Iter t m (Either e a)
tryI = wrapI errToEiter
    where
      errToEiter (Done a c) = Done (Right a) c
      errToEiter iter       = case fromException $ getIterError iter of
                                Just e  -> return $ Left e
                                Nothing -> fixError iter
      fixError (EnumIFail e i) = EnumIFail e $ Right i
      fixError (EnumOFail e i) = EnumOFail e $ liftM Right i
      fixError iter            = IterFail $ getIterError iter

-- | Catch an exception thrown by an 'Iter'.  Returns the failed
-- 'Iter' state, which may contain more information than just the
-- exception.  For instance, if the exception occured in an
-- enumerator, the returned 'Iter' will also contain an unfailed inner
-- 'Iter' that has not failed.  To avoid discarting this extra
-- information, you should not re-throw exceptions with 'throwI'.
-- Rather, you should re-throw an exception by re-executing the failed
-- 'Iter'.  For example, you could define an @onExceptionI@ function
-- analogous to the standard library 'onException' as follows:
--
-- > onExceptionI iter cleanup =
-- >     iter `catchI` \(SomeException _) iter' -> cleanup >> iter'
--
-- If you wish to continue processing the iteratee after a failure in
-- an enumerator, use the 'resumeI' function.  For example:
--
-- > action `catchI` \(SomeException e) iter ->
-- >     if isEnumError iter
-- >       then do liftIO $ putStrLn $ "enumerator failure: " ++ show e
-- >               resumeI iter
-- >       else iter
--
-- Note that @catchI@ only works for /synchronous/ exceptions, such as
-- IO errors (thrown within 'liftIO' blocks), the monadic 'fail'
-- operation, and exceptions raised by 'throwI'.  It is not possible
-- to catch /asynchronous/ exceptions, such as lazily evaluated
-- divide-by-zero errors, the 'throw' function, or exceptions raised
-- by other threads using 'throwTo'.
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

-- | A version of 'throwI' with the arguments reversed, analogous to
-- 'handle' in the standard library.  (A more logical name for this
-- function might be 'handleI', but that name is used for the file
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
-- concatenated series.
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
chunkI = IterF $ \c@(Chunk t eof) -> return $
         if null t && not eof then chunkI else Done c (Chunk mempty eof)

-- | Wrap a function around an 'Iter' to transform its result.  The
-- 'Iter' will be fed data as usual, then fed to the function the
-- first time it enters a state other than 'IterF'.
wrapI :: (ChunkData t, Monad m) =>
         (Iter t m a -> Iter t m b)
      -> Iter t m a
      -> Iter t m b
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
-- return value is lifted into the invoking Iteratee monadic type.
runI :: (ChunkData t1, ChunkData t2, Monad m) =>
        Iter t1 m a
     -> Iter t2 m a
runI iter@(IterF _)  = lift (runIter iter chunkEOF) >>= runI
runI (Done a _)      = return a
runI (IterFail e)    = IterFail e
runI (EnumIFail e i) = EnumIFail e i
runI (EnumOFail e i) = runI i >>= EnumIFail e

-- | Pop an 'Iter' back out of an 'EnumI'.
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
-- eof function.  For instance, 'handleI' could be defined as:
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
-- as a list of lazy 'Bytestring's, where each element of the list
-- should be a separate datagram.
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

-- | An outermost enumerator that gets data from somewhere else and
-- feeds it to the iteratee, returning the iteratee when there is no
-- more input or the iteratee produces a result of type a.
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
-- side-effect of @a@.
cat :: (Monad m, ChunkData t) => EnumO t m a -> EnumO t m a -> EnumO t m a
cat a b iter = do
  iter' <- returnI $ a iter
  case iter' of
    IterF _ -> b iter'
    _       -> iter'
infixr 3 `cat`

-- | Run an outer enumerator on an iteratee.  Any errors in inner
-- enumerators that have been fused to the iteratee will be considered
-- interatee failures.
(|$) :: (ChunkData t, Monad m) => EnumO t m a -> Iter t m a -> m a
(|$) enum iter = run $ enum $ wrapI (>>= return) iter
-- The purpose of the wrapI (>>= return) is to convert any EnumIFail
-- (or, less likely, EnumOFail) errors thrown by iter to IterFail
-- errors, so that enumCatch statements only catch enumerator
-- failures.
infixr 2 |$

-- | An inner enumerator or transcoder.  Such a function accepts data
-- from some outer enumerator (acting like an Iteratee), then
-- transcodes the data and feeds it to another Iter (hence also
-- acting like an enumerator towards that inner Iter).  Note that
-- data is viewed as flowing inwards from the outermost enumerator to
-- the innermost iteratee.  Thus tOut, the \"outer type\", is actually
-- the type of input fed to an EnumI, while @tIn@ is what the @EnumI@
-- feeds to an iteratee.
type EnumI tOut tIn m a = Iter tIn m a -> Iter tOut m (Iter tIn m a)

-- | Concatenate two inner enumerators
catI :: (ChunkData tOut, ChunkData tIn, Monad m) =>
        EnumI tOut tIn m a
     -> EnumI tOut tIn m a
     -> EnumI tOut tIn m a
catI a b = a >=> b
infixr 3 `catI`

-- | Fuse an outer enumerator, producing chunks of some type @tOut@,
-- with an inner enumerator that transcodes @tOut@ to @tIn@, to
-- produce a new outer enumerator producing chunks of type @tIn@.
(|..) :: (ChunkData tOut, ChunkData tIn, Monad m) =>
         EnumO tOut m (Iter tIn m a)
      -> EnumI tOut tIn m a
      -> EnumO tIn m a
(|..) outer inner iter = joinI $ outer $ inner iter
infixl 4 |..

-- | Fuse two inner enumerators into one.
(..|..) :: (ChunkData tOut, ChunkData tMid, ChunkData tIn, Monad m) => 
           EnumI tOut tMid m (Iter tIn m a)
        -> EnumI tMid tIn m a
        -> EnumI tOut tIn m a
(..|..) outer inner iter = wrapI (return . joinI . joinI) $ outer $ inner iter
infixl 5 ..|..

-- | Fuse an inner enumerator that transcodes @tOut@ to @tIn@ with an
-- iteratee taking type @tIn@ to produce an iteratee taking type
-- @tOut@.
(..|) :: (ChunkData tOut, ChunkData tIn, Monad m
         , Show tOut, Show tIn, Show a) =>
         EnumI tOut tIn m a
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
    Left e  -> EnumOFail e iter
    Right b -> do iter' <- returnI $ enumO (input b) iter
                  ec <- tryI $ runI (after b)
                  case ec of
                    Left e | not $ isIterError iter' -> EnumOFail e iter'
                    _                                -> iter'

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

-- | Like 'catchI', but for 'EnumO's instead of 'Iter's.  Note that
-- this only catches exceptions that were thrown by enumerators.
-- ('catchI' catches both exceptions that are thrown by enumerators
-- and those thrown by iteratees.)  However, also note that when
-- applied to an 'EnumO', it will catch all errors thrown by any
-- 'EnumI's fused to the 'EnumO'.  This could lead to unexpected
-- results in cases like this:
--
-- >    cat (enumFile "file1" `enumCatch` handler1)
-- >        (enumFile "file2" `enumCatch` handler2)
-- >            |.. inum
-- >    |$ somethingI
--
-- Suppose @handler1@ calls 'resumeI' to continue executing after an
-- exception.  If @inum@ throws an exception while processing @file1@,
-- the exception will be caught by @handler1@, then execution will
-- continue on @file2@, at which point the exception will be re-thrown
-- and caught for a second time by @handler2@.  If that is not what
-- you wanted, then it may have been better to write:
--
-- >    cat (enumFile "file1" `enumCatch` handler1)
-- >        (enumFile "file2" `enumCatch` handler2)
-- >    |$ inum ..| somethingI
--
enumCatch :: (Exception e, ChunkData t, Monad m) =>
             EnumO t m a
          -- ^ 'Iter' that might throw an exception
          -> (e -> Iter t m a -> Iter t m a)
          -- ^ Exception handler
          -> EnumO t m a
enumCatch enum handler = wrapI check . enum
    where
      check iter'@(IterF _)    = catchI iter' handler
      check iter'@(Done _ _)   = iter'
      check iter'@(IterFail _) = iter'
      check err                = case fromException $ getIterError err of
                                   Just e  -> handler e err
                                   Nothing -> err

-- | 'enumCatch' with the argument order switched.
enumHandler :: (Exception e, ChunkData t, Monad m) =>
               (e -> Iter t m a -> Iter t m a)
            -- ^ Exception handler
            -> EnumO t m a
            -- ^ 'Iter' that might throw an exception
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
-- produced.  You can fuse this to another 'Iter' to produce an 'Iter'
-- that can safely be written from multiple threads.
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


