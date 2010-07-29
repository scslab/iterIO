
-- | This module contains functions to help parsing input from within
-- iteratees.  Many of the operators are either imported from
-- "Data.Applicative" and inspired by "Text.Parsec".

module Data.IterIO.Parse (-- * Iteratee combinators
                          (\/), orEmpty, (<?>)
                         , foldrI, foldr1I, many, many1
                         -- * Applicative combinators
                         , (<$>), (<$), Applicative(..), (<**>)
                         -- * Parsing Iteratees
                         , satisfy, char, string
                         ) where

import Control.Applicative (Applicative(..), (<**>))
import Control.Exception (SomeException, Exception(..))
import Control.Monad
import Data.Functor ((<$>), (<$))
import qualified Data.ListLike as LL
import Data.Maybe
import Data.Monoid
import Data.Typeable

import Data.IterIO

{-
(<|>) :: (ChunkData t, Monad m) => Iter t m a -> Iter t m a -> Iter t m a
a@(IterF _) <|> b = IterF $ \c -> do runIter a c >>= check (null c)

      case a1 of
        IterF _ | null c        -> return (a1 <|> b)
        _ | isIterParseError a1 -> runIter b c
        _                       -> return a1
a <|> b = if isIterParseError a then b else a
-}

-- | An infix synonym for 'ifNoParse'.  The code:
--
-- >     iter \/ failIter $ \res ->
-- >     doSomethingWith res
--
-- Executes @iter@ (saving the input with 'backtrackI'), and if @iter@
-- throws an exception of class 'IterNoParse', executes @failIter@ on
-- the same input.  On the other hand, if @iter@ succeeds and returns
-- @res@, then the expression will go on to @doSomethingWith res@.
--
-- For example, to build up a list of results of executing @iter@, one
-- could implement a type-restricted version of 'many' as follows:
--
-- @
--   myMany :: (ChunkData t, Monad m) => Iter t m a -> Iter t m [a]
--   myMany iter = iter \\/ return [] $ \\res -> (res :) '<$>' myMany iter
-- @
--
-- Has fixity:
--
-- > infix 2 \/
--
(\/) :: (ChunkData t, Monad m) => 
        Iter t m a -> Iter t m b -> (a -> Iter t m b) -> Iter t m b
(\/) = ifNoParse
infix 2 \/

-- | Defined as @orEmpty = ('\\/ return 'mempty')@, and useful when
-- parse failures should just return an empty 'Monoid'.  For example,
-- a type-restricted 'many' can be implemented as:
--
-- @
--   myMany :: (ChunkData t, Monad m) => Iter t m a -> Iter t m [a]
--   myMany iter = iter ``orEmpty`` (\\res -> (res :) '<$>' myMany iter)
-- @
--
orEmpty :: (ChunkData t, Monad m, Monoid b) =>
           Iter t m a -> (a -> Iter t m b) -> Iter t m b
orEmpty = (\/ return mempty)

-- | @iter \<?\> token@ replaces any kind of parse failure in @iter@
-- with an exception equivalent to calling @'expectedI' token@.
--
-- Has fixity:
--
-- > infixr 0 <?>
--
(<?>) :: (ChunkData t, Monad m) => Iter t m a -> String -> Iter t m a
iter <?> expected =
    mapExceptionI (\(IterNoParse _) -> IterExpected [expected]) iter
infixr 0 <?>

-- | Repeatedly invoke an Iteratee, and right fold a function over the
-- results.
foldrI :: (ChunkData t, Monad m) =>
          (a -> b -> b) -> b -> Iter t m a -> Iter t m b
foldrI f z iter = iter \/ return z $ \a -> f a <$> foldrI f z iter

-- | Repeatedly invoke an Iteratee, and right fold a function over the
-- results.  Requires the Iteratee to succeed at least once.
foldr1I :: (ChunkData t, Monad m) =>
          (a -> b -> b) -> b -> Iter t m a -> Iter t m b
foldr1I f z iter = f <$> iter <*> foldr1I f z iter

-- | Run an iteratee zero or more times (until it fails) and return a
-- list-like container of the results.
many :: (ChunkData t, LL.ListLike f a, Monad m) => Iter t m a -> Iter t m f
many = foldrI LL.cons LL.empty

skipMany :: (ChunkData t, Monad m) => Iter t m a -> Iter t m ()
skipMany = foldrI (\_ _ -> ()) ()

sepBy :: (ChunkData t, LL.ListLike f a, Monad m) =>
         Iter t m a -> Iter t m b -> Iter t m f
sepBy item sep = item `orEmpty` \i1 ->
                 LL.cons i1 <$> foldr1I LL.cons LL.empty (sep *> item)

endBy :: (ChunkData t, LL.ListLike f a, Monad m) =>
         Iter t m a -> Iter t m b -> Iter t m f
endBy item sep = foldrI LL.cons LL.empty (item <* sep)

sepEndBy :: (ChunkData t, LL.ListLike f a, Monad m) =>
            Iter t m a -> Iter t m b -> Iter t m f
sepEndBy item sep = nextSep undefined
    where nextItem i = LL.cons i <$> (sep `orEmpty` nextSep)
          nextSep _  = item `orEmpty` nextItem

-- | Run an iteratee one or more times (until it fails) and return a
-- list-like container of the results.
many1 :: (ChunkData t, LL.ListLike f a, Monad m) => Iter t m a -> Iter t m f
many1 = foldr1I LL.cons LL.empty

skipMany1 :: (ChunkData t, Monad m) => Iter t m a -> Iter t m ()
skipMany1 = foldr1I (\_ _ -> ()) ()

sepBy1 :: (ChunkData t, LL.ListLike f a, Monad m) =>
          Iter t m a -> Iter t m b -> Iter t m f
sepBy1 item sep = item >>= \i1 ->
                  LL.cons i1 <$> foldr1I LL.cons LL.empty (sep *> item)

endBy1 :: (ChunkData t, LL.ListLike f a, Monad m) =>
          Iter t m a -> Iter t m b -> Iter t m f
endBy1 item sep = foldr1I LL.cons LL.empty (item <* sep)

sepEndBy1 :: (ChunkData t, LL.ListLike f a, Monad m) =>
             Iter t m a -> Iter t m b -> Iter t m f
sepEndBy1 item sep = item >>= nextItem
    where nextItem i = LL.cons i <$> (sep `orEmpty` nextSep)
          nextSep _  = item `orEmpty` nextItem

                 
-- | Read the next input element if it satisfies some predicate.
satisfy :: (ChunkData t, LL.ListLike t e, Monad m) =>
           (e -> Bool) -> Iter t m e
satisfy test = do
  e <- headLikeI
  if test e then return e else expectedI "satify predicate"

-- | Read input that exactly matches a character.
char :: (ChunkData t, LL.ListLike t e, Eq e, Enum e, Monad m) =>
        Char -> Iter t m e
char target = satisfy (toEnum (fromEnum target) ==) <?> [target]

-- | Read input that exactly matches a string.
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
          else expectedI fulltarget
