
-- | This module contains functions to help parsing input from within
-- iteratees.  Many of the operators are imported from or inspired by
-- "Data.Applicative" and "Text.Parsec".  Some operators, notably
-- '<|>', are re-defined here to have different precedence and
-- associativity.  (The @'Alternative'@ class's @\<|\>@ operator is
-- left associative, which would be very inefficient with iteratees.)

module Data.IterIO.Parse (ifParse, ifNoParse, char, string
                         , (<?>), (<|>), many, many1
                         , (<$>), (<$), Applicative(..), (<**>)
                         ) where

import Control.Applicative (Applicative(..), (<**>))
import Control.Exception (SomeException, Exception(..))
import Control.Monad
import Data.Functor ((<$>), (<$))
import qualified Data.ListLike as LL
import Data.Maybe
import Data.Typeable
import System.IO.Error (isEOFError)

import Data.IterIO

-- | Infix synonym for 'mplus'.  Note that 'mplus' for the 'Iter'
-- 'MonadPlus' uses @'backtrackI'@, which keeps a copy of all input
-- data around until the iteratee finishes.  Thus, @\<|\>@ should not
-- be used with iteratees that consume lots of input.
--
-- Note also that 'mplus' for the 'Iter' 'MonadPlus' only considers
-- EOF failures and failures caused by the 'fail' method of the
-- 'Monad'.  Other types of errors (such as IO errors) will not be
-- caught by 'mplus'.
--
-- @\<|\>@ has precedence:
--
-- > infixr 1 <|>
--
-- Note that we specifically don't use the 'Alternative' class because
-- the @\<|\>@ method of 'Alternative' has left fixity instead of
-- right fixity (which would be very expensive).
(<|>) :: (MonadPlus m) => m a -> m a -> m a
(<|>) = mplus
infixr 1 <|>

-- | @iter \<?\> token@ replaces any kind of parse failure in @iter@
-- with an exception equivalent to calling @'expectedI' token@.
(<?>) :: (ChunkData t, Monad m) => Iter t m a -> String -> Iter t m a
iter <?> expected =
    mapExceptionI (\(IterNoParse _) -> IterExpected [expected]) iter
infix 0 <?>

-- | @ifParse iter success failure@ runs @iter@, but saves a copy of
-- all input consumed.  (This means @iter@ must not consume unbounded
-- amounts of input!)  If @iter@ suceeds, its result is passed to the
-- function @success@.  If @iter@ throws an exception of class
-- 'IterNoParse', then @failure@ is executed with the input re-wound
-- (so that @failure@ is fed the same input that @iter@ was).
ifParse :: (ChunkData t, Monad m) =>
           Iter t m a           -- ^ @iter@ to run with backtracking
        -> (a -> Iter t m b)    -- ^ @success@ function
        -> Iter t m b           -- ^ @failure@ action
        -> Iter t m b
ifParse iter yes no = do
  ea <- backtrackI iter
  case ea of
    Right a -> yes a
    Left (IterNoParse err, _) -> 
        case cast err of
          Just (IterExpected exp) -> mapExceptionI (combine exp) no
          _ -> no
    where
      combine e1 (IterExpected e2) = IterExpected (e1 ++ e2)

-- | This function is just 'ifParse' with the second and third
-- arguments reversed.
ifNoParse :: (ChunkData t, Monad m) =>
             Iter t m a -> Iter t m b -> (a -> Iter t m b) -> Iter t m b
ifNoParse iter no yes = ifParse iter yes no

-- | Super inefficient--Yucko
foldrMany :: (ChunkData t, Monad m) =>
             (a -> b -> b) -> b -> Iter t m a -> Iter t m b
foldrMany f z iter =
    let foldNext = ifNoParse iter (return z) $ \a -> f a <$> foldNext
    in foldNext

-- | Run an iteratee zero or more times (until it fails) and return a
-- list-like container of the results.
many :: (LL.ListLike f a, MonadPlus m) => m a -> m f
many v = many1 v <|> return LL.empty

-- | Run an iteratee one or more times (until it fails) and return a
-- list-like container of the results.
many1 :: (LL.ListLike f a, MonadPlus m) => m a -> m f
many1 v = liftM LL.cons v `ap` many v

-- | Read the next input element if it satisfies some predicate.
satisfy :: (ChunkData t, LL.ListLike t e, Monad m) =>
           (e -> Bool) -> Iter t m e
satisfy test = headLikeI >>= check
    where
      check e | test e    = return e
              | otherwise = fail "satisfy failed"

-- | Read input that exactly matches a character, or else fail.
char :: (ChunkData t, LL.ListLike t e, Eq e, Enum e, Monad m) =>
        Char -> Iter t m e
char target = headLikeI >>= check
    where
      t = toEnum (fromEnum target)
      check c | c == t    = return c
              | otherwise = fail $ "expected '" ++ target:"'"

-- | Read input that exactly matches a string, or else fail.
string :: (ChunkData t, LL.ListLike t e, LL.StringLike t, Eq e, Monad m) =>
          String -> Iter t m t
string fulltarget = doMatch ft
    where
      ft = LL.fromString fulltarget
      doMatch target | LL.null target = return ft
                     | otherwise      = do
        m <- stringMaxI $ LL.length target
        if not (LL.null m) && LL.isPrefixOf m target
          then doMatch $ LL.drop (LL.length m) target
          else fail $ "expected " ++ show fulltarget

