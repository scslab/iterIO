
-- | This module contains functions to help parsing input from within
-- iteratees.  Many of the operators are either imported from
-- "Data.Applicative" or inspired by "Text.Parsec".

module Data.IterIO.Parse (-- * Iteratee combinators
                          (<|>), (<&>), (\/), orI, orEmpty, (<?>)
                         , foldrI, foldr1I, foldlI, foldl1I
                         , peekI, skipI, ensureI
                         , skipWhileI, skipWhile1I, whileI, while1I
                         , readI
                         -- * Applicative combinators
                         , (<$>), (<$), Applicative(..), (<**>)
                         , (>$>), (<++>)
                         -- * Parsing Iteratees
                         -- $Parseclike
                         , many, skipMany, sepBy, endBy, sepEndBy
                         , many1, skipMany1, sepBy1, endBy1, sepEndBy1
                         , satisfy, char, string
                         -- * Internal use functions
                         , fastLL1, paranoidLL1
                         ) where

import Prelude hiding (null)
import Control.Applicative (Applicative(..), (<**>), liftA2)
import Control.Exception (Exception(..))
import Data.Char
import Data.Functor ((<$>), (<$))
import qualified Data.ListLike as LL
import Data.Monoid

import Data.IterIO.Base
import Data.IterIO.ListLike

fastLL1 :: (ChunkData t, Monad m) => Iter t m a -> Iter t m a -> Iter t m a
fastLL1 a@(IterF _) b = IterF $ \c -> runIter a c >>= check c
    where
      check _ a1@(Done _ _) = return a1
      check c a1@(IterF _) | not (null c) = return a1
                           | otherwise    = return $ fastLL1 a1 b
      check c a1 = runIter (fastLL1 a1 b) c
fastLL1 a b = a `catchI` \err _ -> combineExpected err b


-- | To catch bugs that only get triggered on certain input
-- boundaries, this version of @<|>@ always just feeds the first
-- character to Iteratees.
paranoidLL1 :: (ChunkData t, LL.ListLike t e, Monad m) =>
               Iter t m a -> Iter t m a -> Iter t m a
paranoidLL1 a@(IterF _) b = IterF dorun
    where
      dorun c@(Chunk t eof)
          | LL.null t || LL.null (LL.tail t) = runIter a c >>= check c
          | otherwise                        = do
        let ch = Chunk (LL.singleton $ LL.head t) False
            ct = Chunk (LL.tail t) eof
        a2 <- runIter a ch >>= check ch
        runIter a2 ct
      check _ a1@(Done _ _) = return a1
      check c a1@(IterF _) | not (null c) = return a1
                           | otherwise    = return $ paranoidLL1 a1 b
      check c a1 = runIter (paranoidLL1 a1 b) c
paranoidLL1 a b = a `catchI` \err _ -> combineExpected err b

-- | LL(1) parser alternative.  @a \<|\> b@ starts by executing @a@.
-- If @a@ throws an exception of class 'IterNoParse' /and/ @a@ has not
-- consumed any input, then @b@ is executed.  (@a@ has consumed input
-- if it returns in the 'IterF' state after being fed a non-empty
-- 'Chunk'.)
--
-- Use of this combinator is somewhat error-prone, because it may be
-- difficult to tell whether Iteratee @a@ will ever consume input
-- before failing.  For this reason, it is safer to use 'orI', the
-- '\/' operator, or the '<&>' operator, all of which support
-- unlimited lookahead (LL(*) parsing) in different ways.
--
-- @\<|\>@ has fixity:
--
-- > infixr 3 <|>

(<|>) :: (ChunkData t, LL.ListLike t e, Monad m) =>
         Iter t m a -> Iter t m a -> Iter t m a
(<|>) = paranoidLL1
infixr 3 <|>

-- | An alternative to the LL(1) function '<|>' and the LL(*)
-- functions '\/', 'orI', and 'ifParse' (which copy input for
-- backtracking and hence pose problems for unbounded input).
-- @a \<&>  b@ is an LL(*) parser that succeeds if either @a@ or @b@
-- does, but avoids keeping a copy of all input data around for
-- backtracking by running @a@ and @b@ in parallel on input as it
-- arrives.  More specifically, @a \<&> b@ behaves as follows:
--
--  * If @a@ throws a parse error (exception of type 'IterNoParse'),
--    then returns the result of having executed @b@ concurrently on
--    the same input.
--
--  * Otherwise, if @a@ succeeds or throws any other kind of error,
--    returns (or throws) the result of executing @a@.  However, in
--    this case @b@ may have partially executed on some portion of the
--    input.
--
-- Because the second iteratee (@b@) may partially execute, it is a
-- bad idea to use @\<&>@ if @b@ has any monadic side effects.  If @b@
-- has side effects, you may want to use '\/' to execute the two
-- Iteratees in strict sequence (but with '\/' you will have to be
-- careful to avoid storing unbounded inputs in memory).
(<&>) :: (ChunkData t, Monad m) =>
         Iter t m a -> Iter t m a -> Iter t m a
a@(IterF _) <&> b =
    IterF $ \c -> do
      a1 <- runIter a c
      case (a1, b) of
        (Done _ _, _)      -> return a1
        (IterF _, IterF _) -> runIter b c >>= return . (a1 <&>)
        _ -> case fromException $ getIterError a1 of
               Just e  -> runIter b c >>= return . combineExpected e
               Nothing -> return a1
a <&> b = a `catchI` \err _ -> combineExpected err b


-- | @(f >$> a) t@ is equivelent to @f t '<$>' a@.  Particularly
-- useful with infix combinators such as '\/' and ``orEmpty`` for
-- chaining a bunch of parse actions.  (See the example at 'orEmpty'.)
--
-- Has fixity:
--
-- > infixl 3 >$>
--
(>$>) :: (Functor f) => (t -> a -> b) -> f a -> t -> f b
(>$>) f a t = f t <$> a
infixr 3 >$>

-- | An infix synonym for 'ifNoParse' that allows LL(*) parsing of
-- alternatives, while keeping a copy of input data consumed by an
-- Iteratee that might require backtracking.  The code:
--
-- >     iter1 \/ iter2 $ \iter1Result -> doSomethingWith iter1Result
--
-- Executes @iter1@ (saving a copy of the input for backtracking).  If
-- @iter1@ fails with an exception of class 'IterNoParse', then the
-- input is re-wound and fed to @iter2@.  On the other hand, if
-- @iter1@ succeeds and returns @iter1Result@, then the saved input is
-- discarded (as @iter2@ will not need to be run) and the result of
-- @iter1@ is fed to function @doSomethingWith@.
--
-- For example, to build up a list of results of executing @iter@, one
-- could implement a type-restricted version of 'many' as follows:
--
-- @
--   myMany :: (ChunkData t, Monad m) => Iter t m a -> Iter t m [a]
--   myMany iter = iter \\/ return [] '$' (:) '>$>' myMany iter
-- @
--
-- In other words, @myMany@ tries running @iter@.  If @iter@ fails,
-- then @myMany@ returns the empty list.  If @iter@ succeeds, its
-- result is fed to @(:) '>$>' myMany iter@, which is equivalent to:
--
-- @
--     \\iterResult -> (iterResult :) ``fmap`` myMany iter
-- @
--
-- @\\/@ has fixity:
--
-- > infix 2 \/
--
(\/) :: (ChunkData t, Monad m) => 
        Iter t m a -> Iter t m b -> (a -> Iter t m b) -> Iter t m b
(\/) = ifNoParse
infix 2 \/

-- | @orI@ is a version of '<|>' with infinite backtracking, allowing
-- LL(*) instead of LL(1) parsing.  @orI a b@ executes @a@, keeping a
-- copy of all input consumed.  If @a@ throws an exception of class
-- 'IterNoParse' (e.g., by calling 'expectedI' or 'throwEOFI'), then
-- @b@ is executed on the same input.
--
-- Because @orI@ must keep a copy of all input fed to @a@, @a@ must
-- not read unbounded input.  If @a@ is a compound Iteratee such as @a
-- = ma >>= k@ and backtracking is only required on the first part
-- (@ma@), then it is preferable to use '\/', as in:
--
-- @
--   ma '\/' b '$' k
-- @
--
-- Has fixity:
--
-- > infixr 3 `orI`
--
orI :: (ChunkData t, Monad m) => Iter t m a -> Iter t m a -> Iter t m a
orI a b = ifParse a return b
infixr 3 `orI`

-- | Defined as @orEmpty = ('\/' return 'mempty')@, and useful when
-- parse failures should just return an empty 'Monoid'.  For example,
-- a type-restricted 'many' can be implemented as:
--
-- @
--   myMany :: (ChunkData t, Monad m) => Iter t m a -> Iter t m [a]
--   myMany iter = iter ``orEmpty`` (:) '>$>' myMany iter
-- @
--
-- Has fixity:
--
-- > infixr 3 `orEmpty`
--
orEmpty :: (ChunkData t, Monad m, Monoid b) =>
           Iter t m a -> (a -> Iter t m b) -> Iter t m b
orEmpty = (\/ return mempty)
infixr 3 `orEmpty`

-- | @iter \<?\> token@ replaces any kind of parse failure in @iter@
-- with an exception equivalent to calling @'expectedI' token@.
--
-- Has fixity:
--
-- > infix 0 <?>
--
(<?>) :: (ChunkData t, Monad m) => Iter t m a -> String -> Iter t m a
iter <?> expected =
    mapExceptionI (\(IterNoParse _) -> IterExpected [expected]) iter
infix 0 <?>

-- | Repeatedly invoke an Iteratee, and right fold a function over the
-- results.
foldrI :: (ChunkData t, Monad m) =>
          (a -> b -> b) -> b -> Iter t m a -> Iter t m b
foldrI f z iter = iter \/ return z $ f >$> foldrI f z iter

-- | Repeatedly invoke an Iteratee, and right fold a function over the
-- results.  Requires the Iteratee to succeed at least once.
foldr1I :: (ChunkData t, Monad m) =>
          (a -> b -> b) -> b -> Iter t m a -> Iter t m b
foldr1I f z iter = f <$> iter <*> foldrI f z iter

-- | Strict left fold over an iteratee (until it throws an
-- 'IterNoParse' exception).  @foldlI f z iter@ is sort of equivalent
-- to:
--
-- > ... (f <$> (f <$> (f z <$> iter) <*> iter) <*> iter) ...
foldlI :: (ChunkData t, Monad m) =>
          (b -> a -> b) -> b -> Iter t m a -> Iter t m b
foldlI f z0 iter = foldNext z0
    where foldNext z = z `seq` iter \/ return z $ \a -> foldNext (f z a)

-- | A version of 'foldlI' that fails if the Iteratee argument does
-- not succeed at least once.
foldl1I :: (ChunkData t, Monad m) =>
           (b -> a -> b) -> b -> Iter t m a -> Iter t m b
foldl1I f z iter = iter >>= \a -> foldlI f (f z a) iter

-- | Discard the result of executing an Iteratee once.  Throws an
-- error if the Iteratee fails.  (Like @skip x = x >> return ()@.)
skipI :: Applicative f => f a -> f ()
skipI = (() <$)

-- | Peeks at the next input element without consuming it.  Throws an
-- 'IterEOF' exception if an end of file is encountered.
peekI :: (LL.ListLike t e, Monad m) => Iter t m e
peekI = IterF $ \c@(Chunk t eof) -> return $
        if LL.null t
        then if eof
             then throwEOFI "peekI"
             else peekI
        else Done (LL.head t) c

-- | Ensures the next input element satisfies a predicate or throws a
-- parse error.  Does not consume any input.
ensureI :: (ChunkData t, LL.ListLike t e, Monad m) =>
           (e -> Bool) -> Iter t m ()
ensureI test = do
  e <- peekI
  if test e then return () else expectedI "ensureI predicate"

-- | Skip all input elements encountered until an element is found
-- that does not match the specified predicate.
skipWhileI :: (LL.ListLike t e, Monad m) => (e -> Bool) -> Iter t m ()
skipWhileI test = IterF $ \(Chunk t eof) ->
                  return $ case LL.dropWhile test t of
                             t1 | LL.null t1 && not eof -> skipWhileI test
                             t1 -> Done () $ Chunk t1 eof

-- | Like 'skipWhileI', but fails if at least one element does not
-- satisfy the predicate.  Safe for use with '<|>'.
skipWhile1I :: (ChunkData t, LL.ListLike t e, Monad m) =>
               (e -> Bool) -> Iter t m ()
skipWhile1I test = ensureI test >> skipWhileI test <?> "skipWhile1I"

-- | Return all input elements up to the first one that does not match
-- the specified predicate.
whileI :: (Show t, LL.ListLike t e, Monad m) => (e -> Bool) -> Iter t m t
whileI test = more LL.empty
    where
      more t0 = IterF $ \(Chunk t eof) ->
                return $ case LL.span test t of
                         (t1, t2) | not (LL.null t2) || eof ->
                                      Done (LL.append t0 t1) $ Chunk t2 eof
                         (t1, _) -> more (LL.append t0 t1)

-- | Like 'whileI', but fails if at least one element does not satisfy
-- the predicate.  Safe for use with '<|>'.
while1I :: (Show t, ChunkData t, LL.ListLike t e, Monad m) =>
           (e -> Bool) -> Iter t m t
while1I test = ensureI test >> whileI test <?> "while1I"

-- | This iteratee parses a 'LL.StringLike' input.  It does not
-- consume any Iteratee input.  The only reason it is an Iteratee is
-- so that it can throw an Iteratee parse error if it fails to parse
-- the input (or if the input has an ambiguous parse).
readI :: (ChunkData t, LL.StringLike s, Monad m, Read a) => 
         s -> Iter t m a
readI s' = let s = LL.toString s'
           in case [a | (a,"") <- reads s] of
                [a] -> return a
                []  -> throwI $ IterParseErr $ "readI can't parse: " ++ s
                _   -> throwI $ IterParseErr $ "readI ambiguous: " ++ s
                               
-- | Concatenate the result of two 'Applicative' types returning
-- 'LL.ListLike' types (@\<++> = 'liftA2' 'LL.append'@).  Has the same
-- fixity as '++', namely:
--
-- > infixr 5 <++>
(<++>) :: (Applicative f, LL.ListLike t e) => f t -> f t -> f t
(<++>) = liftA2 LL.append
infixr 5 <++>

-- $Parseclike
--
-- These functions are intended to be similar to those supplied by
-- "Text.Parsec".

-- | Run an iteratee zero or more times (until it fails) and return a
-- list-like container of the results.
many :: (ChunkData t, LL.ListLike f a, Monad m) => Iter t m a -> Iter t m f
many = foldrI LL.cons LL.empty

skipMany :: (ChunkData t, Monad m) => Iter t m a -> Iter t m ()
skipMany = foldlI (\_ _ -> ()) ()

sepBy :: (ChunkData t, LL.ListLike f a, Monad m) =>
         Iter t m a -> Iter t m b -> Iter t m f
sepBy item sep =
    item `orEmpty` LL.cons >$> foldr1I LL.cons LL.empty (sep *> item)

endBy :: (ChunkData t, LL.ListLike f a, Monad m) =>
         Iter t m a -> Iter t m b -> Iter t m f
endBy item sep = foldrI LL.cons LL.empty (item <* sep)

sepEndBy :: (ChunkData t, LL.ListLike f a, Monad m) =>
            Iter t m a -> Iter t m b -> Iter t m f
sepEndBy item sep =
    item `orEmpty` LL.cons >$> sep `orEmpty` (\_ -> sepEndBy item sep)


-- | Run an iteratee one or more times (until it fails) and return a
-- list-like container of the results.
many1 :: (ChunkData t, LL.ListLike f a, Monad m) => Iter t m a -> Iter t m f
many1 = foldr1I LL.cons LL.empty

skipMany1 :: (ChunkData t, Monad m) => Iter t m a -> Iter t m ()
skipMany1 = foldl1I (\_ _ -> ()) ()

sepBy1 :: (ChunkData t, LL.ListLike f a, Monad m) =>
          Iter t m a -> Iter t m b -> Iter t m f
sepBy1 item sep = item >>= LL.cons >$> foldr1I LL.cons LL.empty (sep *> item)

endBy1 :: (ChunkData t, LL.ListLike f a, Monad m) =>
          Iter t m a -> Iter t m b -> Iter t m f
endBy1 item sep = foldr1I LL.cons LL.empty (item <* sep)

sepEndBy1 :: (ChunkData t, LL.ListLike f a, Monad m) =>
             Iter t m a -> Iter t m b -> Iter t m f
sepEndBy1 item sep =
    item >>= LL.cons >$> sep `orEmpty` (\_ -> sepEndBy item sep)

                 
-- | Read the next input element if it satisfies some predicate.
satisfy :: (ChunkData t, LL.ListLike t e, Monad m) =>
           (e -> Bool) -> Iter t m e
satisfy test = do
  e <- headLikeI
  if test e then return e else expectedI "satify predicate"

-- | Read input that exactly matches a character.
char :: (ChunkData t, LL.ListLike t e, Eq e, Enum e, Monad m) =>
        Char -> Iter t m e
char target = satisfy (toEnum (ord target) ==) <?> [target]

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
