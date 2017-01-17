{-# LANGUAGE CPP #-}
#if defined(__GLASGOW_HASKELL__) && (__GLASGOW_HASKELL__ >= 702)
{-# LANGUAGE Safe #-}
#endif
-- | This module contains functions to help parsing input from within
-- 'Iter's.  Many of the operators are either imported from
-- "Data.Applicative" or inspired by "Text.Parsec".

module Data.IterIO.Parse (-- * Iteratee combinators
                          (<|>), (\/), orEmpty, (<?>), expectedI
                         , foldrI, foldr1I, foldrMinMaxI
                         , foldlI, foldl1I, foldMI, foldM1I
                         , skipI, optionalI, ensureI
                         , eord
                         , skipWhileI, skipWhile1I
                         , whileI, while1I, whileMaxI, whileMinMaxI
                         , concatI, concat1I, concatMinMaxI
                         , readI, eofI
                         -- * Applicative combinators
                         , (<$>), (<$), ($>), (>$>), Applicative(..), (<**>)
                         , (<++>), (<:>), nil
                         -- * Parsing Iteratees
                         -- $Parseclike
                         , many, skipMany, sepBy, endBy, sepEndBy
                         , many1, skipMany1, sepBy1, endBy1, sepEndBy1
                         , satisfy, char, match, string, stringCase
                         ) where

import Prelude hiding (null)
import Control.Applicative (Applicative(..), (<**>), liftA2)
import Control.Monad
import Data.Char
import Data.Functor ((<$>), (<$))
import qualified Data.ListLike as LL
import Data.Monoid

import Data.IterIO.Iter
import Data.IterIO.Inum
import Data.IterIO.ListLike

-- | An infix synonym for 'multiParse' that allows LL(*) parsing of
-- alternatives by executing both Iteratees on input chunks as they
-- arrive.  This is similar to the @\<|>@ method of the
-- @'Alternative'@ class in "Control.Applicative", but the
-- @'Alternative'@ operator has left fixity, while for efficiency this
-- one has:
--
-- > infixr 3 <|>
(<|>) :: (ChunkData t, Monad m) =>
         Iter t m a -> Iter t m a -> Iter t m a
{-# INLINE (<|>) #-}
(<|>) = multiParse
infixr 3 <|>

-- | An infix synonym for 'ifNoParse' that allows LL(*) parsing of
-- alternatives by keeping a copy of input data consumed by the first
-- Iteratee so as to backtrack and execute the second Iteratee if the
-- first one fails.  Returns a function that takes a continuation for
-- the first 'Iter', should it succeed.  The code:
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
--   myMany iter = iter \\/ return [] '$' \\r -> 'fmap' ((:) r) (myMany iter)
-- @
--
-- In other words, @myMany@ tries running @iter@.  If @iter@ fails,
-- then @myMany@ returns the empty list.  If @iter@ succeeds, its
-- result @r@ is added to the head of the list returned by calling
-- @myMany@ recursively.  This idiom of partially applying a binary
-- funciton to a result and then applying the resulting function to an
-- 'Iter' via 'fmap' is so common that there is an infix operator for
-- it, @'>$>'@.  Thus, the above code can be written:
--
-- @
--   myMany iter = iter \\/ return [] '$' (:) '>$>' myMany iter
-- @
--
-- Of course, using 'fmap' is not the most efficient way to implement
-- @myMany@.  If you are going to use this pattern for something
-- performance critical, you should use an accumulator rather than
-- build up long chains of 'fmap's.  A faster implementation would be:
--
-- @
--   myMany iter = loop id
--       where loop ac = iter \\/ return (acc []) '$' \a -> loop (acc . (a :))
-- @
--
-- @\\/@ has fixity:
--
-- > infix 2 \/
--
(\/) :: (ChunkData t, Monad m) => 
        Iter t m a -> Iter t m b -> (a -> Iter t m b) -> Iter t m b
{-# INLINE (\/) #-}
(\/) = ifNoParse
infix 2 \/

-- | @(f >$> a) t@ is equivalent to @f t '<$>' a@ (where '<$>' is an
-- infix alias for 'fmap').  Particularly useful with infix
-- combinators such as '\/' and ``orEmpty`` when chaining parse
-- actions.  See examples at '\/' and 'orEmpty'.  Note 'fmap' is not
-- always the most efficient solution (see an example in the
-- description of '\/').
--
-- Has fixity:
--
-- > infixl 3 >$>
--
(>$>) :: (Functor f) => (t -> a -> b) -> f a -> t -> f b
{-# INLINE (>$>) #-}
(>$>) f a = \t -> f t <$> a
infixr 3 >$>

-- | @fa $> b = b <$ fa@ -- replaces the output value of a functor
-- with some pure value.  Has the same fixity as '<$>' and '<$',
-- namely:
--
-- > infixl 4 $>
($>) :: (Functor f) => f a -> b -> f b
{-# INLINE ($>) #-}
a $> b = b <$ a
infixl 4 $>

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
{-# INLINE orEmpty #-}
orEmpty = (\/ nil)
infixr 3 `orEmpty`

-- | @iter \<?\> token@ replaces any kind of parse failure in @iter@
-- with an exception equivalent to calling @'expectedI' prefix token@
-- where @prefix@ is a prefix of the input that was fed to @iter@ and
-- caused it to fail.
--
-- Has fixity:
--
-- > infix 0 <?>
--
(<?>) :: (ChunkData t, Monad m) => Iter t m a -> String -> Iter t m a
{-# INLINE (<?>) #-}
(<?>) iter expected =
    Iter $ \c -> case runIter iter c of
      r@(Done _ _)   -> r
      r@(Fail e _ _) -> case e of
                          IterException _ -> r
                          _ -> Fail (IterExpected [(show c, expected)])
                                    Nothing Nothing
      r              -> slowPath (show c) expected r
    where
      {-# NOINLINE slowPath #-}
      slowPath saw exp1 = onDoneR $ \r0 ->
        case r0 of
          r@(Fail e _ _) -> case e of
                              IterException _ -> r
                              _ -> Fail (IterExpected [(saw, exp1)])
                                   Nothing Nothing
          r -> r
infix 0 <?>
  
-- | Throw an 'Iter' exception that describes expected input not
-- found.
expectedI :: (ChunkData t) =>
             String             -- ^ Input actually received
          -> String             -- ^ Description of input that was wanted
          -> Iter t m a
expectedI saw target =
    Iter $ \_ -> Fail (IterExpected [(saw, target)]) Nothing Nothing

-- | Repeatedly invoke an 'Iter' and right-fold a function over the
-- results.
foldrI :: (ChunkData t, Monad m) =>
          (a -> b -> b) -> b -> Iter t m a -> Iter t m b
foldrI = innerFoldrI id

innerFoldrI :: (ChunkData t, Monad m) =>
               (b -> b) -> (a -> b -> b) -> b -> Iter t m a -> Iter t m b
innerFoldrI acc0 f z iter = loop acc0
    where loop acc = iter \/ return (acc z) $ \a -> loop (acc . f a)

-- | A variant of 'foldrI' that requires the 'Iter' to succeed at
-- least once.
foldr1I :: (ChunkData t, Monad m) =>
           (a -> b -> b) -> b -> Iter t m a -> Iter t m b
foldr1I f z iter = iter >>= \a -> innerFoldrI (f a) f z iter

-- | A variant of 'foldrI' that requires the 'Iter' to succeed at
-- least a minimum number of items and stops parsing after executing
-- the 'Iter' some maximum number of times.
foldrMinMaxI :: (ChunkData t, Monad m) =>
                Int             -- ^ Minimum number to parse
             -> Int             -- ^ Maximum number to parse
             -> (a -> b -> b)   -- ^ Folding function
             -> b               -- ^ Rightmost value
             -> Iter t m a      -- ^ Iteratee generating items to fold
             -> Iter t m b
foldrMinMaxI nmin0 nmax0 f z iter
    | nmin0 > nmax0 = throwParseI "foldrMinMaxI: min > max"
    | nmax0 < 0     = throwParseI "foldrMinMaxI: negative max"
    | otherwise = loop id nmin0 nmax0
    where
      loop acc nmin nmax
          | nmax == 0 = return $ acc z
          | nmin > 0  = iter >>= \a -> loop (acc . f a) (nmin - 1) (nmax - 1)
          | otherwise = iter \/ return (acc z) $ \a ->
                        loop (acc . f a) 0 (nmax - 1)

-- | Strict left fold over an 'Iter' (until it throws an 'IterNoParse'
-- exception).  @foldlI f z iter@ is sort of equivalent to:
--
-- > ... (f <$> (f <$> (f z <$> iter) <*> iter) <*> iter) ...
foldlI :: (ChunkData t, Monad m) =>
          (b -> a -> b) -> b -> Iter t m a -> Iter t m b
foldlI f z0 iter = foldNext z0
    where foldNext z = z `seq` iter \/ return z $ \a -> foldNext (f z a)

-- | A version of 'foldlI' that fails if the 'Iter' argument does not
-- succeed at least once.
foldl1I :: (ChunkData t, Monad m) =>
           (b -> a -> b) -> b -> Iter t m a -> Iter t m b
foldl1I f z iter = iter >>= \a -> foldlI f (f z a) iter

-- | @foldMI@ is a left fold in which the folding function can execute
-- monadic actions.  Essentially @foldMI@ is to 'foldlI' as 'foldM' is
-- to @`foldl'`@ in the standard libraries.
foldMI :: (ChunkData t, Monad m) =>
          (b -> a -> Iter t m b) -> b -> Iter t m a -> Iter t m b
foldMI f z0 iter = foldNext z0
    where foldNext z = iter \/ return z $ f z >=> foldNext

-- | A variant of 'foldMI' that requires the 'Iter' to succeed at
-- least once.
foldM1I :: (ChunkData t, Monad m) =>
           (b -> a -> Iter t m b) -> b -> Iter t m a -> Iter t m b
foldM1I f z0 iter = iter >>= f z0 >>= \z -> foldMI f z iter


-- | Discard the result of executing an Iteratee once.  Throws an
-- error if the Iteratee fails.  (Like @skip x = x >> return ()@.)
skipI :: Applicative f => f a -> f ()
skipI = (() <$)

-- | Execute an iteratee.  Discard the result if it succeeds.  Rewind
-- the input and suppress the error if it fails.
optionalI :: (ChunkData t, Monad m) => Iter t m a -> Iter t m ()
optionalI iter = ifParse iter (const $ return ()) (return ())

-- | Ensures the next input element satisfies a predicate or throws a
-- parse error.  Does not consume any input.
ensureI :: (ChunkData t, LL.ListLike t e, Monad m) =>
           (e -> Bool) -> Iter t m ()
ensureI test =
    Iter $ \c@(Chunk t eof) ->
        if LL.null t
           then (if eof then eofFail else IterF (ensureI test))
           else (if test (LL.head t) then Done () c else testFail)
    where testFail = Fail (IterParseErr "ensureI test failed") Nothing Nothing
          eofFail  = Fail (mkIterEOF "ensureI EOF") Nothing Nothing

-- | A variant of the standard library 'ord' function, but that
-- translates a 'Char' into any 'Enum' type, not just 'Int'.
-- Particularly useful for 'Iter's that must work with both 'String's
-- (which consist of 'Char's) and ASCII @'ByteString'@s (which consist
-- of @'Word8'@s).  For example, to skip one or more space or TAB
-- characters, you can use:
--
-- @
--   skipSpace :: ('LL.ListLike' t e, ChunkData t, 'Eq' e, 'Enum' e, Monad m) =>
--                'Iter' t m ()
--   skipSpace = 'skipWhile1I' (\\c -> c == eord ' ' || c == eord '\t')
-- @
eord :: (Enum e) => Char -> e
{-# INLINE eord #-}
eord = toEnum . ord

-- | Skip all input elements encountered until an element is found
-- that does not match the specified predicate.
skipWhileI :: (ChunkData t, LL.ListLike t e, Monad m) =>
              (e -> Bool) -> Iter t m ()
skipWhileI test = loop
    where loop = Iter $ \(Chunk t eof) ->
                 case LL.dropWhile test t of
                   t1 | LL.null t1 && not eof -> IterF loop
                   t1 -> Done () $ Chunk t1 eof

-- | Like 'skipWhileI', but fails if at least one element does not
-- satisfy the predicate.
skipWhile1I :: (ChunkData t, LL.ListLike t e, Monad m) =>
               (e -> Bool) -> Iter t m ()
skipWhile1I test = ensureI test >> skipWhileI test <?> "skipWhile1I"

-- | Return all input elements up to the first one that does not match
-- the specified predicate.
whileI :: (ChunkData t, LL.ListLike t e, Monad m)
          => (e -> Bool) -> Iter t m t
whileI test = more id
    where
      more acc = Iter $ \(Chunk t eof) ->
                 case LL.span test t of
                   (t1, t2) | not (LL.null t2) || eof ->
                                     Done (acc t1) $ Chunk t2 eof
                   (t1, _) -> IterF $ more (acc . LL.append t1)

-- | Like 'whileI', but fails if at least one element does not satisfy
-- the predicate.
while1I :: (ChunkData t, LL.ListLike t e, Monad m) =>
           (e -> Bool) -> Iter t m t
while1I test = ensureI test >> whileI test

-- | A variant of 'whileI' with a maximum number matches.
whileMaxI :: (ChunkData t, LL.ListLike t e, Monad m) =>
             Int                  -- ^ Maximum number to match
          -> (e -> Bool)          -- ^ Predicate test
          -> Iter t m t
whileMaxI nmax test = inumMax nmax .| whileI test

-- | A variant of 'whileI' with a minimum and maximum number matches.
whileMinMaxI :: (ChunkData t, LL.ListLike t e, Monad m) =>
                Int                  -- ^ Minumum number
             -> Int                  -- ^ Maximum number
             -> (e -> Bool)          -- ^ Predicate test
             -> Iter t m t
whileMinMaxI nmin nmax test = do
  result <- whileMaxI nmax test
  if LL.length result >= nmin
    then return result
    else expectedI "too few" "whileMinMaxI minimum"

-- | Repeatedly execute an 'Iter' returning a 'Monoid' and 'mappend'
-- all the results in a right fold.
concatI :: (ChunkData t, Monoid s, Monad m) =>
           Iter t m s -> Iter t m s
concatI iter = foldrI mappend mempty iter

-- | Like 'concatI', but fails if the 'Iter' doesn't return at least
-- once.
concat1I :: (ChunkData t, Monoid s, Monad m) =>
           Iter t m s -> Iter t m s
concat1I iter = foldr1I mappend mempty iter

-- | A version of 'concatI' that takes a minimum and maximum number of
-- items to parse.
concatMinMaxI :: (ChunkData t, Monoid s, Monad m) =>
                 Int            -- ^ Minimum number to parse
              -> Int            -- ^ Maximum number to parse
              -> Iter t m s     -- ^ 'Iter' whose results to concatenate
              -> Iter t m s
concatMinMaxI nmin nmax iter = foldrMinMaxI nmin nmax mappend mempty iter
                               
                               
-- | This 'Iter' parses a 'LL.StringLike' argument.  It does not
-- consume any Iteratee input.  The only reason it is an Iteratee is
-- so that it can throw an Iteratee parse error should it fail to
-- parse the argument string (or should the argument yield an
-- ambiguous parse).
readI :: (ChunkData t, Monad m, LL.StringLike s, Read a) => 
         s -> Iter t m a
readI s' = let s = LL.toString s'
           in case [a | (a,"") <- reads s] of
                [a] -> return a
                []  -> throwParseI $ "readI can't parse: " ++ s
                _   -> throwParseI $ "readI ambiguous: " ++ s

-- | Ensures the input is at the end-of-file marker, or else throws an
-- exception.
eofI :: (ChunkData t, Monad m, Show t) => Iter t m ()
eofI = do
  Chunk t eof <- iterF $ \c -> Done c c
  if eof && null t
    then return ()
    else expectedI (chunkShow t) "EOF"

-- | 'mappend' the result of two 'Applicative' types returning
-- 'Monoid' types (@\<++> = 'liftA2' 'mappend'@).  Has the same fixity
-- as '++', namely:
--
-- > infixr 5 <++>
(<++>) :: (Applicative f, Monoid t) => f t -> f t -> f t
(<++>) = liftA2 mappend
infixr 5 <++>

-- | 'LL.cons' an 'Applicative' type onto an an 'Applicative'
-- 'LL.ListLike' type (@\<:> = 'liftA2' 'LL.cons'@).  Has the same
-- fixity as @:@, namely:
--
-- > infixr 5 <:>
(<:>) :: (LL.ListLike t e, Applicative f) => f e -> f t -> f t
{-# INLINE (<:>) #-}
(<:>) = liftA2 LL.cons
infixr 5 <:>

-- | @nil = 'pure' 'mempty'@--An empty 'Monoid' injected into an
-- 'Applicative' type.
nil :: (Applicative f, Monoid t) => f t
{-# INLINE nil #-}
nil = pure mempty

-- $Parseclike
--
-- These functions are intended to be similar to those supplied by
-- "Text.Parsec".

-- | Run an 'Iter' zero or more times (until it fails) and return a
-- list-like container of the results.
many :: (ChunkData t, LL.ListLike f a, Monad m) => Iter t m a -> Iter t m f
{-# INLINE many #-}
many = foldrI LL.cons LL.empty

-- | Repeatedly run an 'Iter' until it fails and discard all the
-- results.
skipMany :: (ChunkData t, Monad m) => Iter t m a -> Iter t m ()
skipMany = foldlI (\_ _ -> ()) ()

-- | Parses a sequence of the form
-- /Item1 Separator Item2 Separator ... Separator ItemN/
-- and returns the list @[@/Item1/@,@ /Item2/@,@ ...@,@ /ItemN/@]@
-- or a 'LL.ListLike' equivalent.
sepBy :: (ChunkData t, LL.ListLike f a, Monad m) =>
         Iter t m a             -- ^ Item to parse
      -> Iter t m b             -- ^ Separator between items
      -> Iter t m f             -- ^ Returns 'LL.ListLike' list of items
sepBy item sep = item `orEmpty` \a ->
                 innerFoldrI (LL.cons a) LL.cons LL.empty (sep *> item)

-- | Like 'sepBy', but expects a separator after the final item.  In
-- other words, parses a sequence of the form
-- /Item1 Separator Item2 Separator ... Separator ItemN Separator/
-- and returns the list @[@/Item1/@,@ /Item2/@,@ ...@,@ /ItemN/@]@ or
-- a 'LL.ListLike' equivalent.
endBy :: (ChunkData t, LL.ListLike f a, Monad m) =>
         Iter t m a             -- ^ Item to parse
      -> Iter t m b             -- ^ Separator that must follow each item
      -> Iter t m f             -- ^ Returns 'LL.ListLike' list of items
endBy item sep = foldrI LL.cons LL.empty (item <* sep)

-- | Accepts items that would be parsed by either 'sepBy' or 'endBy'.
-- Essentially a version of 'endBy' in which the final separator is
-- optional.
sepEndBy :: (ChunkData t, LL.ListLike f a, Monad m) =>
            Iter t m a -> Iter t m b -> Iter t m f
sepEndBy item sep = sepBy item sep <* optionalI sep


-- | Run an 'Iter' one or more times (until it fails) and return a
-- list-like container of the results.
many1 :: (ChunkData t, LL.ListLike f a, Monad m) => Iter t m a -> Iter t m f
many1 = foldr1I LL.cons LL.empty

-- | A variant of 'skipMany' that throws a parse error if the 'Iter'
-- does not succeed at least once.
skipMany1 :: (ChunkData t, Monad m) => Iter t m a -> Iter t m ()
skipMany1 = foldl1I (\_ _ -> ()) ()

-- | A variant of 'sepBy' that throws a parse error if it cannot
-- return at least one item.
sepBy1 :: (ChunkData t, LL.ListLike f a, Monad m) =>
          Iter t m a -> Iter t m b -> Iter t m f
sepBy1 item sep = item >>= \a ->
                  innerFoldrI (LL.cons a) LL.cons LL.empty (sep *> item)

-- | A variant of 'endBy' that throws a parse error if it cannot
-- return at least one item.
endBy1 :: (ChunkData t, LL.ListLike f a, Monad m) =>
          Iter t m a -> Iter t m b -> Iter t m f
endBy1 item sep = foldr1I LL.cons LL.empty (item <* sep)

-- | A variant of 'sepEndBy' that throws a parse error if it cannot
-- return at least one item.
sepEndBy1 :: (ChunkData t, LL.ListLike f a, Monad m) =>
             Iter t m a -> Iter t m b -> Iter t m f
sepEndBy1 item sep = sepBy1 item sep <* optionalI sep

                 
-- | Read the next input element if it satisfies some predicate.
-- Otherwise throw an error.
satisfy :: (ChunkData t, LL.ListLike t e, Enum e, Monad m) =>
           (e -> Bool) -> Iter t m e
satisfy test =
    Iter $ \c@(Chunk t eof) ->
        if LL.null t
           then (if eof then eofFail else IterF (satisfy test))
           else case LL.head t of
                  h | test h -> 
                        Done h (Chunk (LL.tail t) eof)
                    | otherwise ->
                        Fail (IterExpected [(show $ chr $ fromEnum h
                                           , "satisfy predicate")])
                        Nothing (Just c)
    where eofFail  = Fail (mkIterEOF "satisfy: EOF") Nothing Nothing

-- | Read input that exactly matches a character.
char :: (ChunkData t, LL.ListLike t e, Eq e, Enum e, Monad m) =>
        Char -> Iter t m e
{-# INLINE char #-}
char target = satisfy (eord target ==) <?> show target

-- | Read input that exactly matches some target.
match :: (ChunkData t, LL.ListLike t e, Eq e, Monad m) =>
         t -> Iter t m t
match ft = doMatch ft
    where doMatch target | LL.null target = return ft
                         | otherwise      = do
            m <- data0MaxI $ LL.length target
            if not (LL.null m) && LL.isPrefixOf m target
              then doMatch $ LL.drop (LL.length m) target
              else expectedI (chunkShow m) $ chunkShow target

-- | Read input that exactly matches a string.
string :: (ChunkData t, LL.ListLike t e, LL.StringLike t, Eq e, Monad m) =>
          String -> Iter t m t
{-# INLINE string #-}
string = match . LL.fromString

-- | Read input that matches a string up to case.
stringCase :: (ChunkData t, LL.ListLike t e, Enum e, Eq e, Monad m) =>
              String -> Iter t m t
stringCase ft = doMatch LL.empty $ ft
    where
      prefix a b | LL.null a = True
                 | otherwise =
                     if toLower (chr $ fromEnum $ LL.head a) /= toLower (head b)
                     then False else LL.tail a `prefix` LL.tail b
      doMatch acc target | LL.null target = return acc
                         | otherwise      = do
        m <- data0MaxI $ LL.length target
        if not (LL.null m) && m `prefix` target
          then doMatch (LL.append acc m) $ LL.drop (LL.length m) target
          else expectedI (chunkShow m) $ chunkShow target
