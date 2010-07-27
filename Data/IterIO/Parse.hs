
-- | This module contains functions to help parsing input from within
-- iteratees.  Many of the operators are imported from or inspired by
-- "Data.Applicative" and "Text.Parsec".  Some operators, notably
-- '\<|\>', are re-defined here to have different precedence and
-- associativity.  (The @'Alternative'@ class's @\<|\>@ operator is
-- left associative, which would be very inefficient with iteratees.)

module Data.IterIO.Parse (char, string
                         , (<?>), (<|>), (<$>), (<$), Applicative(..), (<**>)
                         ) where

import Control.Applicative (Applicative(..), (<**>))
import Control.Exception (Exception(..))
import Control.Monad
import Data.Functor ((<$>), (<$))
import qualified Data.ListLike as LL
import Data.Maybe
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
iter <?> expected = mapExceptionI change iter
    where
      change e = if (maybe False isEOFError (fromException e)
                     || isJust (fromException e :: Maybe IterError))
                 then toException (IterExpected [expected])
                 else e
infix 0 <?>

-- | Run an iteratee zero or more times (until it fails) and return a
-- list-like container of the results.
many :: (LL.ListLike f a, MonadPlus m) => m a -> m f
many v = some v <|> return LL.empty

-- | Run an iteratee one or more times (until it fails) and return a
-- list-like container of the results.
some :: (LL.ListLike f a, MonadPlus m) => m a -> m f
some v = liftM LL.cons v `ap` many v

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

