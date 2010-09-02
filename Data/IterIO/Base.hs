{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}


{- | This module contains the base Enumerator/Iteratee IO
     abstractions.  See the documentation in the "Data.IterIO" module
     for a high-level tutorial on these abstractions.

     An iteratee is a data sink that is fed chunks of data.  It may
     return a useful result, or its use may lie in monadic
     side-effects, such as storing received data to a file.  Iteratees
     are represented by the type @'Iter' t m a@.  @t@ is the type of
     the data chunks (which must be a 'ChunkData', such as 'String' or
     lazy 'L.ByteString').  @m@ is the 'Monad' in which the iteratee
     runs--for instance 'IO' (or an instance of 'MonadIO') for the
     iteratee to perform IO.  @a@ is the result type of the iteratee,
     for when it has consumed enough input to produce a result.

     An Enumerator is a data source that feeds data chunks to an
     iteratee.  There are two types of Enumerator.

       * An /outer enumerator/, represented by the type 'EnumO',
         generates data from some external source, such as IO (or
         potentially some internal state such as a pseudo-random
         generator).  Outer enumerators are generally constructed
         using 'enumO', which repeatedly runs a degenerate 'Codec'
         that outputs data.  (The 'Codec' is degenerate because its
         input type is @()@--meaning it converts nothing to some kind
         of output data.)  When the 'Codec' can produce no more data,
         it signals this by returning in the 'CodecE' state, which
         causes 'enumO' to return the iteratee so that it can
         potentially be passed to a different enumerator for more
         data.  (An enumerator should not feed 'EOF' to an
         iteratee--only the '|$' operator, 'run', and 'runI' functions
         do this.)  If the iteratee returns a result or fails, the
         enumerator also returns it immediately.

       * An /inner enumerator/, represented by the type 'EnumI', gets
         its data from another enumerator, then feeds some transformed
         version of the data to an iteratee.  Thus, an 'EnumI' behaves
         as an iteratee when interfacing to the outer enumerator, and
         behaves as an enumerator when feeding data to some \"inner\"
         iteratee.  Inner enumerators are built using the function
         'enumI', which is analogous to 'enumO' for outer enumerators,
         except that the 'Codec' actually processes meaningful input
         data.  An inner enumerator, when done, returns the inner
         iteratee's state, as well as its own Iteratee state.  An
         inner enumerator that receives EOF should /not/ feed the EOF
         to its iteratee, as the iteratee may subsequently be passed
         to another enumerator for more input.  (This convention is
         respected by the 'enumI' function.)

    IO is performed by applying an outer enumerator to an iteratee,
    using the '|$' (\"pipe apply\") binary operator.

    An important property of enumerators and iteratees is that they
    can be /fused/.  The '|..' operator fuses an outer enumerator with
    an inner enumerator, yielding an outer enumerator, e.g.,
    @enumo '|..' enumi@.  Similarly, the '..|' operator fuses an inner
    enumerator with an iteratee to yield another iteratee, e.g.,
    @enumi '..|' iter@.  Finally, two inner enumerators may be fused
    into one with the '..|..' operator.

    Enumerators can also be concatenated.  Two outer enumerators may
    be concatenated using the 'cat' function.  Thus, @enumO1 ``cat``
    enumO2@ produces an outer enumerator whose effect is to feed first
    @enumO1@'s data then @enumO2@'s data to an iteratee.  Inner
    enumerators may similarly be concatenated using the 'catI'
    function.
-}

module Data.IterIO.Base
    (-- * Base types
     ChunkData(..), Chunk(..)
    , Iter(..), EnumO, EnumI
    -- * Concatenation and fusing operators
    , (|$)
    , cat, catI
    , (|..), (..|..), (..|)
    -- * Enumerator construction functions
    , Codec, CodecR(..)
    , iterToCodec
    , enumCO, enumO, enumO', enumCObracket, enumObracket
    , enumCI, enumI, enumI'
    -- * Exception and error functions
    , IterNoParse(..), IterEOF(..), IterExpected(..), IterParseErr(..)
    , throwI, throwEOFI, expectedI
    , tryI, tryBI, catchI, catchBI, handlerI, handlerBI
    , enumCatch, enumHandler, inumCatch
    , resumeI, verboseResumeI, mapExceptionI
    , ifParse, ifNoParse, multiParse
    -- * Some basic Iteratees
    , nullI, dataI, chunkI, atEOFI
    , wrapI, runI, popI, joinI, returnI, resultI
    -- * Some basic Enumerators
    , enumPure
    , iterLoop
    , inumNop, inumRepeat, inumSplit
    -- * Control functions
    , CtlCmd, CtlReq(..), CtlHandler
    , ctlI, safeCtlI
    , noCtls, ctl, ctl', ctlHandler
    -- * Other functions
    , runIter, run, chunk, chunkEOF
    ) where

import Prelude hiding (null)
import qualified Prelude
import Control.Applicative (Applicative(..))
import Control.Concurrent.MVar
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
import qualified Data.ByteString.Lazy as L
import System.Environment
import System.IO
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
data Chunk t = Chunk !t !Bool deriving (Eq)

instance (Show t) => Show (Chunk t) where
    showsPrec _ (Chunk t eof) rest =
        "Chunk " ++ show t ++ if eof then "+EOF" ++ rest else rest

-- | Constructor function that builds a chunk containing data and a
-- 'False' EOF flag.
chunk :: t -> Chunk t
chunk t = Chunk t False

-- | An empty chunk with the EOF flag 'True'.
chunkEOF :: (Monoid t) => Chunk t
chunkEOF = Chunk mempty True

instance (ChunkData t) => Monoid (Chunk t) where
    mempty = Chunk mempty False
    mappend (Chunk a False) (Chunk b eof) = Chunk (mappend a b) eof
    -- We mostly want to avoid appending data to a Chunk that has the
    -- EOF bit set, but make an exception for appending a null chunk,
    -- so that code like the following will work:
    --
    --   (Done (Done "" (Chunk "" True)) (Chunk "" False)) >>= id
    --
    -- While the above code may seem arcane, something similar happens
    -- with, for instance:
    --
    -- do iter <- returnI $ runIter (return "") chunkEOF
    --    iter
    mappend a (Chunk b _) | null b        = a
    mappend _ _                           = error "mappend to EOF"

instance (ChunkData t) => ChunkData (Chunk t) where
    null (Chunk t False) = null t
    null (Chunk _ True)  = False


-- Note that the Ctl types were originally done without
-- MultiParamTypeClasses and FunctionalDependencies, but the result
-- was error prone in that valid control requests would just not be
-- caught if the return type expected didn't match.

-- | Class of control commands for enclosing enumerators.  The class
-- binds each control argument type to a unique result type.
class (Typeable carg, Typeable cres) => CtlCmd carg cres | carg -> cres

-- | A request for a control operation
data CtlReq t m a = forall carg cres. (CtlCmd carg cres) =>
                    CtlReq !carg !(Maybe cres -> Iter t m a)

-- | The basic Iteratee type is @Iter t m a@, where @t@ is the type of
-- input (in class 'ChunkData'), @m@ is a monad in which the iteratee
-- may execute actions (using the monad transformer 'lift' method),
-- and @a@ is the result type of the iteratee.
--
-- An @Iter@ is in one of several states:  it may require more input
-- ('IterF'), it may request some control action other than input data
-- from the enclosing enumerators ('IterC'), it may wish to execute
-- monadic actions in the transformed monad ('IterM'), it may have
-- produced a result ('Done'), or it may have failed.  Failure is
-- indicated by 'IterFail', 'EnumIFail', or 'EnumOFail', depending on
-- whether the failure occured in an iteratee, in an inner enumerator,
-- or in an outer enumerator.  (In the latter two cases, when an
-- enumerator failed, the result also includes the state of the inner
-- iteratee, which generally will not have failed.)
--
-- Note that @Iter t@ is a 'MonadTrans' and @Iter t m@ is a a 'Monad'
-- (as discussed in the documentation for module "Data.IterIO").
data Iter t m a = IterF !(Chunk t -> Iter t m a)
                -- ^ The iteratee requires more input.
                | IterM !(m (Iter t m a))
                -- ^ The iteratee must execute monadic bind in monad @m@
                | IterC !(CtlReq t m a)
                -- ^ A control request for enclosing enumerators
                | Done a (Chunk t)
                -- ^ Sufficient input was received; the 'Iter' is
                -- returning a result of type @a@.  In adition, the
                -- 'Iter' has a 'Chunk' containing any residual input
                -- that was not consumed to produce the result.
                | IterFail !SomeException
                -- ^ The iteratee failed.
                | EnumOFail !SomeException (Iter t m a)
                -- ^ An 'EnumO' failed; the result includes the status
                -- of the iteratee at the time the enumerator failed.
                | EnumIFail !SomeException a
                -- ^ An 'EnumI' failed; this result includes status of
                -- the Iteratee.  (The type @a@ will always be
                -- @'Iter' t m a\'@ for some @a'@ in the result of an
                -- 'EnumI'.)

instance (ChunkData t) => Show (Iter t m a) where
    showsPrec _ (IterF _) rest = "IterF _" ++ rest
    showsPrec _ (IterM _) rest = "IterM _" ++ rest
    showsPrec _ (Done _ (Chunk t eof)) rest =
        "Done _ (Chunk " ++ (if null t then "mempty " else "_ ")
                         ++ show eof ++ ")" ++ rest
    showsPrec _ (IterC _) rest = "IterC _ _" ++ rest
    showsPrec _ (IterFail e) rest = "IterFail " ++ show e ++ rest
    showsPrec _ (EnumOFail e i) rest =
        "EnumOFail " ++ show e ++ " (" ++ (shows i $ ")" ++ rest)
    showsPrec _ (EnumIFail e _) rest =
        "EnumIFail " ++ show e ++ " _" ++ rest

instance (ChunkData t, Monad m) => Functor (Iter t m) where
    fmap = liftM

instance (ChunkData t, Monad m) => Applicative (Iter t m) where
    pure   = return
    (<*>)  = ap
    (*>)   = (>>)
    a <* b = do r <- a; _ <- b; return r

instance (ChunkData t, Monad m) => Monad (Iter t m) where
    return a = Done a mempty

    m@(IterF _)           >>= k = IterF $ runIter m >=> k
    (IterM m)             >>= k = IterM $ liftM (>>= k) m
    (Done a c)            >>= k = runIter (k a) c
    (IterC (CtlReq a fr)) >>= k = iterC a $ fr >=> k
    err                   >>= _ = IterFail $ getIterError err

    fail msg = IterFail $ toException $ ErrorCall msg

instance (ChunkData t) => MonadTrans (Iter t) where
    lift m = IterM $ m >>= return . return

-- | Lift an IO operation into an 'Iter' monad, but if the IO
-- operation throws an error, catch the exception and return it as a
-- failure of the Iteratee.  An IO exception satisfying the
-- 'isEOFError' predicate is re-wrapped in an 'IterEOF' type so as to
-- be caught by handlers expecting 'IterNoParse'.
instance (ChunkData t, MonadIO m) => MonadIO (Iter t m) where
    liftIO m = do
      result <- lift $ liftIO $ try m
      case result of
        Right ok -> return ok
        Left err -> IterFail $ case fromException err of
                                 Just ioerr | isEOFError ioerr ->
                                                toException $ IterEOF ioerr
                                 _ -> err

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

{-
-- | This is a fixed point combinator for iteratees over monads that
-- have no side effects.  If you wish to use @mdo@ with such a monad,
-- you can define an instance of 'MonadFix' in which
-- @'mfix' = fixIterPure@.  However, be warned that this /only/ works
-- when computations in the monad have no side effects, as
-- @fixIterPure@ will repeatedly re-invoke the function passsed in
-- when more input is required (thereby also repeating side-effects).
-- For cases in which the monad may have side effects, if the monad is
-- in the 'MonadIO' class then there is already an 'mfix' instance
-- defined using 'fixMonadIO'.
fixIterPure :: (ChunkData t, MonadFix m) =>
               (a -> Iter t m a) -> Iter t m a
fixIterPure f = IterM $ mfix ff
    where
      ff ~(Done a _)  = check $ f a
      -- Warning: IterF case re-runs function, repeating side effects
      check (IterF i) = return $ IterF $ \c ->
                        fixIterPure $ \a -> runIter (f a) c
      check (IterM m) = m >>= check
      check iter      = return iter
-}


--
-- Internal utility functions
--

-- | @iterC carg fr = 'IterC' ('CtlReq' carg fr)@
iterC :: (CtlCmd carg cres) => carg -> (Maybe cres -> Iter t m a) -> Iter t m a
iterC carg fr = IterC (CtlReq carg fr)

getIterError                 :: Iter t m a -> SomeException
getIterError (IterFail e)    = e
getIterError (EnumOFail e _) = e
getIterError (EnumIFail e _) = e
getIterError (IterM _)       = error "getIterError: no error (in IterM state)"
getIterError (IterC _)       = error "getIterError: no error (in IterC state)"
getIterError _               = error "getIterError: no error to extract"

-- | True if an iteratee /or/ an enclosing enumerator has experienced
-- a failure.  (@isIterError@ is always 'True' when 'isEnumError' is
-- 'True', but the converse is not true.)
isIterError :: Iter t m a -> Bool
isIterError (IterFail _)    = True
isIterError (EnumOFail _ _) = True
isIterError (EnumIFail _ _) = True
isIterError _               = False

-- | True if an enumerator enclosing an iteratee has experienced a
-- failure (but not if the iteratee itself failed).
isEnumError :: Iter t m a -> Bool
isEnumError (EnumOFail _ _) = True
isEnumError (EnumIFail _ _) = True
isEnumError _               = False

-- | True if an iteratee is in an error state caused by an EOF exception.
isIterEOFError :: Iter t m a -> Bool
isIterEOFError (IterF _)   = False
isIterEOFError (IterM _)   = False
isIterEOFError (Done _ _)  = False
isIterEOFError (IterC _)   = False
isIterEOFError err         = case fromException $ getIterError err of
                               Just (IterEOF _) -> True
                               _                -> False

-- | True if an 'Iter' is requesting something from an
-- enumerator--i.e., the 'Iter' is not 'Done' and is not in one of the
-- error states.
isIterActive :: Iter t m a -> Bool
isIterActive (IterF _) = True
isIterActive (IterM _) = True
isIterActive (IterC _) = True
isIterActive _         = False

-- | Apply a function to the next state of an 'Iter' if it is still
-- active, or the current state if it is not.
apNext :: (Monad m) =>
         (Iter t m a -> Iter t m b)
      -> Iter t m a
      -> Iter t m b
apNext f (IterF iterf)            = IterF $ f . iterf
apNext f (IterM iterm)            = IterM $ iterm >>= return . f
apNext f (IterC (CtlReq carg fr)) = iterC carg $ f . fr
apNext f iter                     = f iter

-- | Like 'apNext', but feed an EOF to the 'Iter' if it is in the
-- 'IterF' state.  Thus it can be used within 'Iter's with different
-- input type from the one a function is being applied to.
apRun :: (Monad m, ChunkData t1) =>
         (Iter t1 m a -> Iter t2 m b)
      -> Iter t1 m a
      -> Iter t2 m b
apRun f iter@(IterF _)           = f $ unEOF $ runIter iter chunkEOF
apRun f (IterM iterm)            = IterM $ iterm >>= return . f
apRun f (IterC (CtlReq carg fr)) = iterC carg $ f . fr
apRun f iter                     = f iter

-- | Remove EOF bit from an Iter in the 'Done' state.
unEOF :: (Monad m, ChunkData t) => Iter t m a -> Iter t m a
unEOF = wrapI fixeof
    where
      fixeof (Done a (Chunk t _)) = Done a (Chunk t False)
      fixeof iter                 = iter

--
-- Core functions
--

-- | Runs an 'Iter' on a 'Chunk' of data.  When the 'Iter' is already
-- 'Done', or in some error condition, simulates the behavior
-- appropriately.
--
-- Note that this function asserts the following invariants on the
-- behavior of an 'Iter':
--
--     1. An 'Iter' may not return an 'IterF' (asking for more input)
--        if it received a 'Chunk' with the EOF bit 'True'.  (It is
--        okay to return IterF after issuing a successful 'IterC'
--        request.)
--
--     2. An 'Iter' returning 'Done' must not set the EOF bit if it
--        did not receive the EOF bit.
--
-- It /is/, however, okay for an 'Iter' to return 'Done' without the
-- EOF bit even if the EOF bit was set on its input chunk, as
-- @runIter@ will just propagate the EOF bit.  For instance, the
-- following code is valid:
--
-- @
--      runIter (return ()) 'chunkEOF'
-- @
--
-- Even though it is equivalent to:
--
-- @
--      runIter ('Done' () ('Chunk' 'mempty' True)) ('Chunk' 'mempty' False)
-- @
--
-- in which the first argument to @runIter@ appears to be discarding
-- the EOF bit from the input chunk.  @runIter@ will propagate the EOF
-- bit, making the above code equivalent to to @'Done' () 'chunkEOF'@.
--
-- On the other hand, the following code is illegal, as it violates
-- invariant 2 above:
--
-- @
--      runIter ('Done' () 'chunkEOF') $ 'Chunk' \"some data\" False -- Bad
-- @
runIter :: (ChunkData t, Monad m) =>
           Iter t m a
        -> Chunk t
        -> Iter t m a
runIter iter c | null c           = iter
runIter (IterF f) c@(Chunk _ eof) = (if eof then forceEOF else noEOF) $ f c
    where
      noEOF (Done _ (Chunk _ True)) = error "runIter: IterF returned bad EOF"
      noEOF iter                    = iter
      forceEOF (IterF _)             = error "runIter: IterF returned after EOF"
      forceEOF (IterM m)             = IterM $ forceEOF `liftM` m
      forceEOF (IterC (CtlReq a fr)) = iterC a $ \r -> 
                                       case r of Just _  -> fr r
                                                 Nothing -> forceEOF $ fr r
      forceEOF iter                  = iter
runIter (IterM m) c               = IterM $ flip runIter c `liftM` m
runIter (Done a c) c'             = Done a (mappend c c')
runIter (IterC (CtlReq a fr)) c   = iterC a $ flip runIter c . fr
runIter err _                     = err

unIterEOF :: SomeException -> SomeException
unIterEOF e = case fromException e of
                Just (IterEOF e') -> toException e'
                _                 -> e

-- | Return the result of an iteratee.  If it is still in the 'IterF'
-- state, feed it an EOF to extract a result.  Throws an exception if
-- there has been a failure.
run :: (ChunkData t, Monad m) => Iter t m a -> m a
run iter@(IterF _)        = run $ runIter iter chunkEOF
run (IterM m)             = m >>= run
run (Done a _)            = return a
run (IterC (CtlReq _ fr)) = run $ fr Nothing
run (IterFail e)          = throw $ unIterEOF e
run (EnumOFail e _)       = throw $ unIterEOF e
run (EnumIFail e _)       = throw $ unIterEOF e


--
-- Exceptions
--

-- | Generalized class of errors that occur when an Iteratee does not
-- receive expected input.  (Catches 'IterEOF', 'IterExpected', and
-- the miscellaneous 'IterParseErr'.)
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

-- | Iteratee expected particular input and did not receive it.
data IterExpected = IterExpected [String] deriving (Typeable)
instance Show IterExpected where
    showsPrec _ (IterExpected [token]) rest =
        "Iteratee expected " ++ token ++ rest
    showsPrec _ (IterExpected tokens) rest =
        "Iteratee expected one of ["
        ++ intercalate ", " tokens ++ "]" ++ rest
instance Exception IterExpected where
    toException = noParseToException
    fromException = noParseFromException

-- | Miscellaneous Iteratee parse error.
data IterParseErr = IterParseErr String deriving (Typeable)
instance Show IterParseErr where
    showsPrec _ (IterParseErr err) rest =
        "Iteratee parse error: " ++ err ++ rest
instance Exception IterParseErr where
    toException = noParseToException
    fromException = noParseFromException

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
      Just (IterExpected e1) -> mapExceptionI (combine e1) iter
      _                      -> iter
    where
      combine e1 (IterExpected e2) = IterExpected $ e1 ++ e2

-- | Try two Iteratees and return the result of executing the second
-- if the first one throws an 'IterNoParse' exception.  The statement
-- @multiParse a b@ is similar to @'ifParse' a return b@, but the
-- two functions operate differently.  Depending on the situation,
-- only one of the two formulations is correct.  Specifically:
-- 
--  * @'ifParse' a f b@ works by first executing @a@, saving a copy of
--    all input consumed by @a@.  If @a@ throws a parse error, the
--    saved input is used to backtrack and execute @b@ on the same
--    input that @a@ just rejected.  If @a@ suceeds, @b@ is never run;
--    @a@'s result is fed to @f@, and the resulting action is executed
--    without backtracking (so any error thrown within @f@ will not be
--    caught by this 'ifParse' expression).
--
--  * Istead of saving input, @multiParse a b@ executes both @a@ and
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
-- saving the input for backtracking.  Note that the second
-- argument to 'ifParse' ('return') is a continuation
-- for @a@ when @a@ succeeds.
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
-- must return all left-over input for the next monad (i.e., @next@ in
-- @total >>= next@).  Since @total@ has to look arbitrarily far into
-- the input to determine that @parseAndSumIntegerList@ fails, in
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
                        multiParse (runIter a c) (runIter b c)
    where
      -- If b is IterM, IterC, or Done, we will just accumulate all
      -- the input anyway inside 'runIter', so we might as well just
      -- do it efficiently with 'copyInput' (which is what 'ifParse'
      -- uses, indirectly, via 'tryBI').
      useIfParse (Done _ _) = True
      useIfParse (IterM _)  = True
      useIfParse (IterC _)  = True
      useIfParse _          = False
multiParse a b
    | isIterActive a = apNext (flip multiParse b) a
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
ifParse :: (ChunkData t, Monad m) =>
           Iter t m a
        -- ^ Iteratee @iter@ to run with backtracking
        -> (a -> Iter t m b)
        -- ^ @success@ function
        -> Iter t m b
        -- ^ @failure@ action
        -> Iter t m b
        -- ^ result
ifParse iter yes no = do
  ea <- tryBI iter
  case ea of
    Right a  -> yes a
    Left err -> combineExpected err no

-- | This function is just 'ifParse' with the second and third
-- arguments reversed.
ifNoParse :: (ChunkData t, Monad m) =>
             Iter t m a -> Iter t m b -> (a -> Iter t m b) -> Iter t m b
ifNoParse iter no yes = ifParse iter yes no

-- | Throw an exception from an Iteratee.  The exception will be
-- propagated properly through nested Iteratees, which will allow it
-- to be categorized properly and avoid situations in which, for
-- instance, functions holding 'MVar's are prematurely terminated.
-- (Most Iteratee code does not assume the Monad parameter @m@ is in
-- the 'MonadIO' class, and so cannot use 'catch' or 'onException' to
-- clean up after exceptions.)  Use 'throwI' in preference to 'throw'
-- whenever possible.
throwI :: (Exception e) => e -> Iter t m a
throwI e = IterFail $ toException e

-- | Throw an exception of type 'IterEOF'.  This will be interpreted
-- by 'enumO' and 'enumI' as an end of file chunk when thrown by the
-- generator/codec.  It will also be interpreted by 'ifParse' and
-- 'multiParse' as an exception of type 'IterNoParse'.  If not caught
-- within the 'Iter' monad, the exception will be rethrown by 'run'
-- (and hence '|$') as an 'IOError' of type EOF.
throwEOFI :: String -> Iter t m a
throwEOFI loc = throwI $ IterEOF $ mkIOError eofErrorType loc Nothing Nothing

-- | Throw an iteratee error that describes expected input not found.
expectedI :: String -> Iter t m a
expectedI target = throwI $ IterExpected [target]

-- | Internal function used by 'tryI' and 'backtrackI' when re-propagating
-- exceptions that don't match the requested exception type.  (To make
-- the overall types of those two funcitons work out, a 'Right'
-- constructor needs to be wrapped around the returned failing
-- iteratee.)
fixError :: (ChunkData t, Monad m) =>
            Iter t m a -> Iter t m (Either x a)
fixError (EnumIFail e i) = EnumIFail e $ Right i
fixError (EnumOFail e i) = EnumOFail e $ liftM Right i
fixError iter            = IterFail $ getIterError iter

-- | If an 'Iter' succeeds and returns @a@, returns @'Right' a@.  If
-- the 'Iter' throws an exception @e@, returns @'Left' (e, i)@ where
-- @i@ is the state of the failing 'Iter'.
tryI :: (ChunkData t, Monad m, Exception e) =>
        Iter t m a
     -> Iter t m (Either (e, Iter t m a) a)
tryI = wrapI errToEither
    where
      errToEither (Done a c) = Done (Right a) c
      errToEither iter       = case fromException $ getIterError iter of
                                 Just e  -> return $ Left (e, iter)
                                 Nothing -> fixError iter

-- | Runs an 'Iter' until it no longer requests input, keeping a copy
-- of all input that was fed to it (which might be longer than the
-- input that the 'Iter' actually consumed, because fed input includes
-- any residual data returned in the 'Done' state).
copyInput :: (ChunkData t, Monad m) =>
          Iter t m a
       -> Iter t m (Iter t m a, Chunk t)
copyInput iter1 = doit id iter1
    where
      -- It is usually faster to use mappend in a right associative
      -- way (i.e, mappend a1 (mappend a2 (mappand a3 a4)) will be
      -- faster than mappend (mappend (mappend a1 a2) a3) a4).  Thus,
      -- acc is a function of the rest of the input, rather than a
      -- simple prefix of ithe input.  This is the same technique used
      -- by 'ShowS' to optimize the use of (++) on srings.
      doit acc iter@(IterF _) =
          IterF $ \c -> doit (acc . mappend c) (runIter iter c)
      doit acc iter | isIterActive iter = apNext (doit acc) iter
      doit acc (Done a c)               = Done (return a, acc mempty) c
      doit acc iter                     = return (iter, acc mempty)

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

-- | Catch an exception thrown by an 'Iter'.  Returns the failed
-- 'Iter' state, which may contain more information than just the
-- exception.  For instance, if the exception occured in an
-- enumerator, the returned 'Iter' will also contain an inner 'Iter'
-- that has not failed.  To avoid discarding this extra information,
-- you should not re-throw exceptions with 'throwI'.  Rather, you
-- should re-throw an exception by re-executing the failed 'Iter'.
-- For example, you could define an @onExceptionI@ function analogous
-- to the standard library @'onException'@ as follows:
--
-- @
--  onExceptionI iter cleanup =
--      iter \`catchI\` \\('SomeException' _) iter' -> cleanup >> iter'
-- @
--
-- If you wish to continue processing the iteratee after a failure in
-- an enumerator, use the 'resumeI' function.  For example:
--
-- @
--  action \`catchI\` \\('SomeException' e) iter ->
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
-- wrapped around an inner enumerator.  Consider the following test,
-- in which an exception thrown by an inner enumerator is caught:
--
-- > inumBad :: (ChunkData t, Monad m) => EnumI t t m a
-- > inumBad = enumI' $ fail "inumBad"
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
-- before fusing them to an 'EnumO'.)
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
      check iter'@(Done _ _) = iter'
      check err              = case fromException $ getIterError err of
                                 Just e  -> handler e err
                                 Nothing -> err

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
        -> (e -> Iter t m a)
        -> Iter t m a
catchBI iter handler = copyInput iter >>= uncurry check
    where
      check iter'@(Done _ _) _ = iter'
      check err input          = case fromException $ getIterError err of
                                   Just e -> Done () input >> handler e
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

-- | 'catchBI' with the arguments reversed.
handlerBI :: (Exception e, ChunkData t, Monad m) =>
             (e -> Iter t m a)
          -- ^ Exception handler
          -> Iter t m a
          -- ^ 'Iter' that might throw an exception
          -> Iter t m a
handlerBI = flip catchBI

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
-- * 'enumCatch' only catches 'EnumO' failures, and should only be
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
-- 'EnumI's, see the `inumCatch` function.  For example, compare
-- @test1@ (which throws an exception) to @test2@ and @test3@ (which
-- do not):
--
-- >    inumBad :: (ChunkData t, Monad m) => EnumI t t m a
-- >    inumBad = enumI' $ fail "inumBad"
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

-- | Similar to the standard @'mapException'@ function in
-- "Control.Exception", but operates on exceptions propagated through
-- the 'Iter' monad, rather than language-level exceptions.
mapExceptionI :: (Exception e1, Exception e2, ChunkData t, Monad m) =>
                 (e1 -> e2) -> Iter t m a -> Iter t m a
mapExceptionI f iter1 = wrapI check iter1
    where
      check (IterFail e)    = IterFail (doMap e)
      check (EnumOFail e i) = EnumOFail (doMap e) i
      check (EnumIFail e a) = EnumIFail (doMap e) a
      check iter            = iter
      doMap e = case fromException e of
                  Just e' -> toException (f e')
                  Nothing -> e

--
-- Some super-basic Iteratees
--

-- | Sinks data like @\/dev\/null@, returning @()@ on EOF.
nullI :: (Monad m, Monoid t) => Iter t m ()
nullI = IterF $ check
    where
      check (Chunk _ True) = Done () chunkEOF
      check _              = nullI

-- | Returns any non-empty amount of input data, or throws an
-- exception if EOF is encountered and there is no data.
dataI :: (Monad m, ChunkData t) => Iter t m t
dataI = IterF nextChunk
    where
      nextChunk (Chunk d True) | null d = throwEOFI "dataI"
      nextChunk (Chunk d _)             = return d

-- | Returns a non-empty 'Chunk' or an EOF 'Chunk'.
chunkI :: (Monad m, ChunkData t) => Iter t m (Chunk t)
chunkI = IterF $ \c -> if null c then chunkI else return c

-- | Does not actually consume any input, but returns 'True' if there
-- is no more input data to be had.
atEOFI :: (Monad m, ChunkData t) => Iter t m Bool
atEOFI = IterF check
    where
      check c@(Chunk t eof) | not (null t) = Done False c
                            | eof          = Done True c
                            | otherwise    = atEOFI

-- | Wrap a function around an 'Iter' to transform its result.  The
-- 'Iter' will be fed 'Chunk's as usual for as long as it remains in
-- the 'IterF' or 'IterM' states.  When the 'Iter' enters a state
-- other than 'IterF' or 'IterM', @wrapI@ passes it through the
-- tranformation function.
wrapI :: (ChunkData t, Monad m) =>
         (Iter t m a -> Iter t m b) -- ^ Transformation function
      -> Iter t m a                 -- ^ Original 'Iter'
      -> Iter t m b                 -- ^ Returns an 'Iter' whose
                                    -- result will be transformed by
                                    -- the transformation function
wrapI f = next
    where next iter | isIterActive iter = apNext next iter
                    | otherwise         = f iter


-- XXX the following is no longer true of runI, but should it be true?
-- In the event that the failure is an enumerator failure (either
-- 'EnumIFail' or 'EnumOFail'), @runI@ returns an 'EnumIFail' failure
-- and includes the state of the iteratee.

-- | Runs an Iteratee from within another iteratee (feeding it EOF if
-- it is in the 'IterF' state) so as to extract a return value.  The
-- return value is lifted into the invoking Iteratee monadic type.  If
-- the iteratee being run fails, then @runI@ will propagate the
-- failure by also failing.
runI :: (ChunkData t1, ChunkData t2, Monad m) =>
        Iter t1 m a
     -> Iter t2 m a
runI (Done a _)            = return a
runI (IterFail e)          = IterFail e
runI (EnumIFail e i)       = EnumIFail e i
runI (EnumOFail e i)       = EnumOFail e $ runI i
-- runI (EnumOFail e i) = runI i >>= EnumIFail e
runI iter                  = apRun runI iter

-- | Pop an 'Iter' back out of an 'EnumI', propagating any failure,
-- Any enumerator failure ('EnumIFail' or 'EnumOFail') will be
-- translated to an 'EnumOFail' state.
popI :: (ChunkData tIn, ChunkData tOut, Monad m) =>
          Iter tOut m (Iter tIn m a)
       -> Iter tIn m a
popI (Done i _)            = i
popI (IterFail e)          = IterFail e
popI (EnumIFail e i)       = EnumOFail e i
popI (EnumOFail e i)       = EnumOFail e $ popI i
popI iter                  = apRun popI iter

-- | Join the result of an inner enumerator, so that the result of the
-- inner 'Iter' becomes the result in the outer 'Iter' Monad.  This is
-- similar to 'popI', but where 'popI' returns an 'Iter' taking the
-- inner input type, @joinI@ returns an 'Iter' of the outer input
-- type.  Note that the behavior of @joinI@ is very similar to what
-- one would obtain by defining @joinI iter = iter >>= 'runI'@, with
-- one big difference:  In @iter >>= 'runI'@, the @>>=@ operator will
-- translate any enumerator ('EnumOFail' or 'EnumIFail') failure into
-- an 'IterFail' failure.  By contrast, @joinI@ converts both types of
-- enumerator failure into an 'EnumOFail', thereby preserving the
-- state of the iteratee for possible resumption by 'resumeI'.
joinI :: (ChunkData tIn, ChunkData tOut, Monad m) =>
        Iter tOut m (Iter tIn m a)
     -> Iter tOut m a
joinI (Done i c)      = runI i >>= flip Done c
joinI (IterFail e)    = IterFail e
joinI (EnumIFail e i) = EnumOFail e $ runI i
joinI (EnumOFail e i) = EnumOFail e $ joinI i
joinI iter            = apRun joinI iter

-- | Allows you to look at the state of an 'Iter' by returning it into
-- an 'Iter' monad.  This is just like the monadic 'return' method,
-- except that so long as the 'Iter' is in the 'IterM' or 'IterC'
-- state, then the monadic action or control request is executed.
-- Thus, 'Iter's that do not require input, such as
-- @returnI $ liftIO $ ...@, can execute and return a result (possibly
-- reflecting exceptions) immediately.  Moreover, code looking at an
-- 'Iter' produced with @iter <- returnI someOtherIter@ does not need
-- to worry about the 'IterM' or 'IterC' cases, since the returned
-- 'Iter' is guaranteed not to be in one of those states.
returnI :: (ChunkData tOut, ChunkData tIn, Monad m) =>
           Iter tIn m a
        -> Iter tOut m (Iter tIn m a)
returnI iter@(IterM _) = apRun returnI iter
returnI iter@(IterC _) = apRun returnI iter
returnI iter           = return iter

-- | @resultI@ is like a version of 'returnI' that additionally
-- ensures the returned 'Iter' is not in the 'IterF' state.  Moreover,
-- if the returned 'Iter' is in the 'Done' state, then the left over
-- data will be pulled up to the enclosing 'Iter', so that the
-- returned 'Iter' always has 'mempty' data.  (The EOF flag in the
-- left-over 'Chunk' is preserved, however.)
resultI :: (ChunkData t, Monad m) =>
           Iter t m a -> Iter t m (Iter t m a)
resultI = wrapI fixdone
    where
      fixdone (Done a c@(Chunk _ eof)) = Done (Done a (Chunk mempty eof)) c
      fixdone iter                     = return iter

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
-- @EnumO@s should generally be constructed using the 'enumO'
-- function, which takes care of most of the error-handling details.
type EnumO t m a = Iter t m a -> Iter t m a

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
-- In fact, given an @EnumI@ object @inum@, it is possible to
-- construct a function of type @EnumI'@ as @(enumI '..|')@.  But
-- sometimes one might like to concatenate @EnumI@s.  For instance,
-- consider a network protocol that changes encryption or compression
-- modes midstream.  Transcoding is done by @EnumI@s.  To change
-- transcoding methods after applying an @EnumI@ to an iteratee
-- requires the ability to \"pop\" the iteratee back out of the
-- @EnumI@ so as to be able to hand it to another @EnumI@.  The 'popI'
-- function provides this function in its most general form, though,
-- if one only needs 'EnumI' concatenation, the simpler 'catI'
-- function serves this purpose.
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
  if isIterError iter' then iter' else b iter'
infixr 3 `cat`

-- | Concatenate two inner enumerators.  Has fixity:
--
-- > infixr 3 `catI`
catI :: (ChunkData tOut, ChunkData tIn, Monad m) =>
        EnumI tOut tIn m a      -- ^
     -> EnumI tOut tIn m a
     -> EnumI tOut tIn m a
catI a b iter = do
  iter' <- resultI $ a iter
  if isIterError iter' then iter' else b (popI iter')
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
(|..) outer inner iter = popI $ outer $ inner iter
infixl 4 |..

-- | Fuse two inner enumerators into one.  Has fixity:
--
-- > infixl 5 ..|..
(..|..) :: (ChunkData tOut, ChunkData tMid, ChunkData tIn, Monad m) => 
           EnumI tOut tMid m (Iter tIn m a) -- ^
        -> EnumI tMid tIn m a
        -> EnumI tOut tIn m a
(..|..) outer inner iter = wrapI joinI (outer $ inner iter)
-- wrapI joinI $ outer $ wrapI popI $ inner iter
-- = wrapI (return . joinI . joinI) $ outer $ inner iter
infixl 5 ..|..

-- | Fuse an inner enumerator that transcodes @tOut@ to @tIn@ with an
-- iteratee taking type @tIn@ to produce an iteratee taking type
-- @tOut@.  Has fixity:
--
-- > infixr 4 ..|
(..|) :: (ChunkData tOut, ChunkData tIn, Monad m) =>
         EnumI tOut tIn m a     -- ^
      -> Iter tIn m a
      -> Iter tOut m a
(..|) inner iter = wrapI joinI (inner iter)
-- 
-- Some alternate but less good implementations might be:
-- 
--  * inner ..| iter = inner iter >>= runI
--      But the >>= operation would convert an EnumIFail into an
--      IterFail, discarding information.
--
--  * inner ..| iter = wrapI (runI . popI) $ inner iter
--      But this would discard left-over input if the inner enumerator
--      just returns an unfinished iter upon hitting some input
--      boundary.
-- 
infixr 4 ..|

-- | A @Codec@ is an 'Iter' that tranlates data from some input type
-- @tArg@ to an output type @tRes@ and returns the result in a
-- 'CodecR'.  If the @Codec@ is capable of repeatedly being invoked to
-- translate more input, it returns a 'CodecR' in the 'CodecF' state.
-- This convention allows @Codec@s to maintain state from one
-- invocation to the next by currying the state into the codec
-- function the next time it is invoked.  A @Codec@ that cannot
-- process more input returns a 'CodecR' in the 'CodecE' state,
-- possibly including some final output.
type Codec tArg m tRes = Iter tArg m (CodecR tArg m tRes)

-- | The result type of a 'Codec' that translates from type @tArg@ to
-- @tRes@ in monad @m@.  The result potentially includes a new 'Codec'
-- for translating subsequent input.
data CodecR tArg m tRes = CodecF { unCodecF :: !(Codec tArg m tRes)
                                 , unCodecR :: !tRes }
                          -- ^ This is the normal 'Codec' result,
                          -- which includes another 'Codec' (often the
                          -- same as one that was just called) for
                          -- processing further input.
                        | CodecE { unCodecR :: !tRes }
                          -- ^ This constructor is used if the 'Codec'
                          -- is ending--i.e., returning for the last
                          -- time--and thus cannot provide another
                          -- 'Codec' to process further input.

-- | Transform an ordinary 'Iter' into a stateless 'Codec'.
iterToCodec :: (ChunkData t, Monad m) => Iter t m a -> Codec t m a
iterToCodec iter = codec
    where codec = iter >>= return . CodecF codec
-- iter >>= return . CodecF (iterToCodec iter)

-- | Construct an outer enumerator given a 'Codec' that generates data
-- of type @t@ and a 'CtlHandler' to handle control requests.
enumCO :: (Monad m, ChunkData t) =>
          CtlHandler t m a
       -- ^ Function for handling control requests
       -> Codec () m t
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
enumCO cf codec0 (IterM m)      = IterM $ m >>= return . enumCO cf codec0
enumCO cf codec0 iter@(IterF _) = resultI (runI codec0) >>= nextCodec
    where
      nextCodec codec@(Done _ _) = codec >>= check
      nextCodec codec
          | isIterEOFError codec = iter
          | otherwise            = EnumOFail (getIterError codec) iter 
      check (CodecE t) | null t    = iter
                       | otherwise = runIter iter (chunk t)
      check (CodecF codec t)       = enumCO cf codec $ runIter iter (chunk t)
enumCO cf codec0 (IterC req)    = 
-- Note if when we execute the 'CtlReq' with the handler we get a
-- 'CtlReq' back (likely because the handler doesn't know about that
-- type), we pass it back out to 'run', which will just feed in
-- Nothing.  However, this allows one to augment 'EnumO's with new
-- control operations by simply wrapping them with a new function.
    case cf req of
      IterC (CtlReq carg fr) -> iterC carg $ enumCO cf codec0 . fr
      iter                   -> enumCO cf codec0 iter
enumCO _ _ iter                 = iter

-- | Construct an outer enumerator given a 'Codec' that generates data
-- of type @t@.
enumO :: (Monad m, ChunkData t) =>
         Codec () m t
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
enumO codec0 = enumCO noCtls codec0

-- | Like 'enumO', but the input function returns raw data, not
-- 'Chunk's.  The only way to signal EOF is therefore to raise an EOF
-- exception (either from within 'liftIO', or with 'throwEOFI').
enumO' :: (Monad m, ChunkData t) =>
          Iter () m t
       -> EnumO t m a
enumO' = enumO . iterToCodec

-- | A variant of 'enumObracket' that also takes a 'CtlHandler' (as a
-- function of the input).
enumCObracket :: (Monad m, ChunkData t) =>
                 (Iter () m b)
              -- ^ Before action
              -> (b -> Iter () m c)
              -- ^ After action, as a function of before action result
              -> (b -> CtlHandler t m a)
              -- ^ Control request handler, as funciton of before action result
              -> (b -> (Codec () m t))
              -- ^ Input 'Codec', as a funciton of before aciton result
              -> EnumO t m a
enumCObracket before after cf codec iter0 = tryI (runI before) >>= checkBefore
    where
      checkBefore (Left (e,_)) = EnumOFail e iter0
      checkBefore (Right b)    = resultI (enumCO (cf b) (codec b) iter0)
                                 >>= checkMain b
      checkMain b iter = tryI (runI $ after b) >>= checkAfter iter
      checkAfter iter (Left (e,_)) | not (isIterError iter) = EnumOFail e iter
      checkAfter iter _                                     = iter 

-- | Build an 'EnumO' from a @before@ action, an @after@ function, and
-- an @input@ 'Codec' in a manner analogous to the IO @'bracket'@
-- function.  For instance, you could implement @`enumFile'`@ as
-- follows:
--
-- >   enumFile' :: (MonadIO m) => FilePath -> EnumO L.ByteString m a
-- >   enumFile' path = enumObracket (liftIO $ openBinaryFile path ReadMode)
-- >                                 (liftIO . hClose) doGet
-- >       where
-- >         doGet h = do
-- >           buf <- liftIO $ hWaitForInput h (-1) >> L.hGetNonBlocking h 8192
-- >           return $ if null buf then CodecE L.empty
-- >                                else CodecF (doGet h) buf
enumObracket :: (Monad m, ChunkData t) =>
                (Iter () m b)
             -- ^ Before action
             -> (b -> Iter () m c)
             -- ^ After action, as a function of before action result
             -> (b -> (Codec () m t))
             -- ^ Input 'Codec', as a funciton of before aciton result
             -> EnumO t m a
enumObracket before after codec iter0 = tryI (runI before) >>= checkBefore
    where
      checkBefore (Left (e,_)) = EnumOFail e iter0
      checkBefore (Right b)    = resultI (enumO (codec b) iter0) >>= checkMain b
      checkMain b iter = tryI (runI $ after b) >>= checkAfter iter
      checkAfter iter (Left (e,_)) | not (isIterError iter) = EnumOFail e iter
      checkAfter iter _                                     = iter 

-- | Build an inner enumerator given a 'Codec' that returns chunks of
-- the appropriate type.  Makes an effort to send an EOF to the codec
-- if the inner 'Iter' fails, so as to facilitate cleanup.  However,
-- if a containing 'EnumO' or 'EnumI' fails, code handling that
-- failure will have to send an EOF or the codec will not be able to
-- clean up.
enumCI :: (Monad m, ChunkData tOut, ChunkData tIn) =>
          CtlHandler tIn m a
       -- ^ Control request handler
       -> Codec tOut m tIn
       -- ^ Codec to be invoked to produce transcoded chunks.
       -> EnumI tOut tIn m a
enumCI cf codec0 iter@(IterF _) = resultI codec0 >>= nextCodec
    where
      nextCodec (Done codecr (Chunk _ eof))  = nextIter codecr eof
      nextCodec codec | isIterEOFError codec = return iter
                      | otherwise         = EnumIFail (getIterError codec) iter
      nextIter (CodecF codec dat) False = enumCI cf codec $ feed dat
      nextIter (CodecE t) _ | null t    = return iter
      nextIter codecr _                 = return $ feed $ unCodecR codecr
      feed dat = runIter iter (chunk dat)
enumCI cf codec0 (IterC req) = case cf req of
                                 IterC (CtlReq carg fr) ->
                                     iterC carg $ enumCI cf codec0 . fr
                                 iter -> enumCI cf codec0 iter
enumCI cf codec0 iter
    | isIterActive iter = apRun (enumCI cf codec0) iter
    -- If iter finished, still must feed EOF to codec before returning
    | otherwise         = resultI (unEOF $ runIter codec0 chunkEOF) >>= check
    where
      check codec | isIterEOFError codec = return iter
                  | isIterError codec    = EnumIFail (getIterError codec) iter
                  | otherwise            = return iter

-- | Build an inner enumerator given a 'Codec' that returns chunks of
-- the appropriate type.  Makes an effort to send an EOF to the codec
-- if the inner 'Iter' fails, so as to facilitate cleanup.  However,
-- if a containing 'EnumO' or 'EnumI' fails, code handling that
-- failure will have to send an EOF or the codec will not be able to
-- clean up.
enumI :: (Monad m, ChunkData tOut, ChunkData tIn) =>
         Codec tOut m tIn
      -- ^ Codec to be invoked to produce transcoded chunks.
      -> EnumI tOut tIn m a
enumI = enumCI IterC

-- | Transcode (until codec throws an EOF error, or until after it has
-- received EOF).
enumI' :: (Monad m, ChunkData tOut, ChunkData tIn) =>
          Iter tOut m tIn
       -- ^ This Iteratee will be executed repeatedly to produce
       -- transcoded chunks.
       -> EnumI tOut tIn m a
enumI' fn iter = enumI (iterToCodec fn) iter

--
-- Support for control operations
--

-- | A control request handler maps control requests to 'Iter's.
type CtlHandler t m a = CtlReq t m a -> Iter t m a

-- | A version of 'ctlI' that uses 'Maybe' instead of throwing an
-- exception to indicate failure.
safeCtlI :: (CtlCmd carg cres, ChunkData t, Monad m) =>
            carg -> Iter t m (Maybe cres)
safeCtlI carg = iterC carg return

-- | Issue a control request, and return the result.  Throws an
-- exception if the operation did not succeed.
ctlI :: (CtlCmd carg cres, ChunkData t, Monad m) =>
        carg -> Iter t m cres
ctlI carg = safeCtlI carg >>= returnit
    where
      returnit (Just res) = return res
      returnit Nothing    = fail $ "Unsupported CtlCmd " ++ show (typeOf carg)

{-
wrapCtl :: (ChunkData t, Monad m) =>
           (CtlReq t m a -> Iter t m a)
        -> Iter t m a -> Iter t m a
wrapCtl f = next
    where next (IterC req)              = apNext next (f req)
          next iter | isIterActive iter = apNext next iter
                    | otherwise         = iter
-}

-- | A control request handler that ignores the request argument and
-- always fails immediately (thereby not passing the control request
-- up further to other enclosing enumerators).
--
-- One use of this is for 'EnumI's that change the data in such a way
-- that control requests would not makes sense to outer enumerators.
-- Suppose @gunzipCodec@ is a codec that uncompresses a file in gzip
-- format.  The corresponding inner enumerator should probably be
-- defined as:
--
-- > inumGunzip :: (Monad m) => EnumI ByteString ByteString m a
-- > inumGunzip = enumCI noCtls gunzipCodec
--
-- The alternate definition @inumGunzip = enumI gunzipCodec@ would
-- likely wreak havoc in the event of any seek requests, as the outer
-- enumerator might seek around in the file, confusing the
-- decompression codec.
noCtls :: CtlHandler t m a
noCtls (CtlReq _ fr) = fr Nothing

-- | Wrap a control command for requests of type @carg@ into a
-- function of type @'CtlReq' t m a -> Maybe 'Iter' t m a@, which is
-- not parameterized by @carg@ and therefore can be grouped in a list
-- with control functions for other types.  The intent is then to
-- combine a list of such functions into a 'CtlHandler' with
-- 'tryCtls'.
--
-- As an example, the following funciton produces a 'CtlHandler'
-- (suitable to be passed to 'enumCO' or 'enumCObracket') that
-- implements control operations for three types:
--
-- @
--  fileCtl :: ('ChunkData' t, 'MonadIO' m) => 'Handle' -> 'CtlHandler' t m a
--  fileCtl h = 'ctlHandler'
--              [ ctl $ \\('SeekC' mode pos) -> 'liftIO' ('hSeek' h mode pos)
--              , ctl $ \\'TellC' -> 'liftIO' ('hTell' h)
--              , ctl $ \\'SizeC' -> 'liftIO' ('hFileSize' h)
--              ]
-- @
ctl :: (CtlCmd carg cres, ChunkData t, Monad m) =>
       (carg -> Iter t m cres)
    -> CtlReq t m a
    -> Maybe (Iter t m a)
ctl f (CtlReq carg fr) = case cast carg of
                           Nothing    -> Nothing
                           Just carg' -> Just $ f carg' >>= fr . cast

-- | A variant of 'ctl' that, makes the control operation fail if it
-- throws any kind of exception (as opposed to re-propagating the
-- exception as an 'EnumOFail', which is what would end up happening
-- with 'ctl').
ctl' :: (CtlCmd carg cres, ChunkData t, Monad m) =>
        (carg -> Iter t m cres)
     -> CtlReq t m a
     -> Maybe (Iter t m a)
ctl' f (CtlReq carg fr) = case cast carg of
                           Nothing    -> Nothing
                           Just carg' -> Just $ doit carg'
    where
      doit carg' = do
        er <- tryI $ f carg'
        case er of
          Right r                   -> fr $ cast r
          Left (SomeException _, _) -> fr Nothing

-- | Create a 'CtlHandler' from a list of functions created with 'ctl'
-- that try each tries one argument type.  See the example given for
-- 'ctl'.
ctlHandler :: (ChunkData t, Monad m) =>
              [CtlReq t m a -> Maybe (Iter t m a)]
           -> CtlHandler t m a
ctlHandler ctls req = case res of
                     Nothing           -> IterC req
                     Just iter         -> iter
    where
      res = foldr ff Nothing ctls
      ff a b = case a req of
                 Nothing  -> b
                 a'       -> a'

{-
-- | Wrap a handler for a particular kind of 'CtlCmd' request around
-- an 'Iter'.
enumCtl :: (ChunkData t, Monad m, CtlCmd carg cres) =>
           (carg -> Iter t m cres) -> EnumO t m a
enumCtl f = wrapCtl handler
    where
      handler (CtlReq carg fr) =
          case cast carg of
            Nothing -> fr Nothing
            Just ca -> do mr <- resultI $ f ca
                          if isIterError mr
                            then EnumOFail (getIterError mr) (iterC carg fr)
                            else mr >>= fr . cast

-- | Block all handlers 'CtlCmd' request issued by an 'Iter'.
filterCtl :: (ChunkData t, Monad m) => EnumO t m a
filterCtl = wrapCtl $ \(CtlReq _ fr) -> fr Nothing
-}

--
-- Basic outer enumerators
--

-- | An 'EnumO' that will feed pure data to 'Iter's.
enumPure :: (Monad m, ChunkData t) => t -> EnumO t m a
enumPure t = enumO $ return $ CodecE t

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
  -- MVar to avoid races when appending to the stored chunk.
  pipe <- liftIO $ newEmptyMVar >>= newMVar
  return (IterF $ iterf pipe, enum pipe)
    where
      iterf pipe c@(Chunk _ eof) = do
             liftIO $ withMVar pipe $ \p ->
                 do mp <- tryTakeMVar p
                    putMVar p $ case mp of
                                  Nothing -> c
                                  Just c' -> mappend c' c
             if eof then Done () chunkEOF else IterF $ iterf pipe

      enum pipe = let codec = do
                        p <- liftIO $ readMVar pipe
                        Chunk c eof <- liftIO $ takeMVar p
                        return $ if eof then CodecE c else CodecF codec c
                  in enumO codec

--
-- Basic inner enumerators
--

-- | The null 'EnumI', which passes data through to another iteratee
-- unmodified.
inumNop :: (ChunkData t, Monad m) => EnumI t t m a
inumNop = enumI' dataI

-- | Repeat an 'EnumI' until an end of file is received or a failure
-- occurs.
inumRepeat :: (ChunkData tOut, MonadIO m, Show tIn) =>
              (EnumI tOut tIn m a) -> (EnumI tOut tIn m a)
inumRepeat inum iter0 = do
  eof <- atEOFI
  if eof then return iter0 else resultI (inum iter0) >>= check
    where
      check (Done iter (Chunk _ False)) = inumRepeat inum iter
      check res                         = res

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
        rnew <- returnI $ runIter rold $ chunk t
        liftIO $ putMVar mv rnew
        case rnew of
          IterF _ | not eof -> IterF $ iterf mv
          _                 -> return rnew


