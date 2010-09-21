
-- | This module contains functions to help parsing input from within
-- 'Iter's.  Many of the operators are either imported from
-- "Data.Applicative" or inspired by "Text.Parsec".

module Data.IterIO.Parse (-- * Iteratee combinators
                          (<|>), (\/), orEmpty, (<?>), expectedI
                         , someI, foldrI, foldr1I, foldrMinMaxI
                         , foldlI, foldl1I, foldMI, foldM1I
                         , skipI, ensureI
                         , skipWhileI, skipWhile1I
                         , whileI, while1I, whileMaxI, whileMinMaxI
                         , whileStateI
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

import Data.IterIO.Base
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
-- iter via 'fmap' is so common that there is an infix operator for
-- it, @'>$>'@.  Thus, the above code can be written:
--
-- @
--   myMany iter = iter \\/ return [] '$' (:) '>$>' myMany iter
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

-- | @(f >$> a) t@ is equivalent to @f t '<$>' a@.  Particularly
-- useful with infix combinators such as '\/' and ``orEmpty`` when
-- chaining parse actions.  See examples at '\/' and 'orEmpty'.
--
-- Has fixity:
--
-- > infixl 3 >$>
--
(>$>) :: (Functor f) => (t -> a -> b) -> f a -> t -> f b
(>$>) f a t = f t <$> a
infixr 3 >$>

-- | @fa $> b = b <$ fa@ -- replaces the output value of a functor
-- with some pure value.  Has the same fixity as '<$>' and '<$',
-- namely:
--
-- > infixl 4 $>
($>) :: (Functor f) => f a -> b -> f b
($>) = flip (<$)
infixl 4 $>


{-
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
-}

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
-- with an exception equivalent to calling @'expectedI' prefix token@
-- where @prefix@ is a prefix of the input that was fed to @iter@ and
-- caused it to fail.
--
-- Has fixity:
--
-- > infix 0 <?>
--
(<?>) :: (ChunkData t, Monad m) => Iter t m a -> String -> Iter t m a
(<?>) iter@(IterF _) expected = do
  saw <- IterF $ \c -> Done c c
  flip mapExceptionI iter $ \(IterNoParse _) ->
      IterExpected (take 50 (show saw) ++ "...") [expected]
(<?>) iter expected
      | isIterActive iter = inumMC passCtl iter >>= (<?> expected)
      | otherwise         = flip mapExceptionI iter $ \(IterNoParse _) ->
                            IterExpected "no input" [expected]
infix 0 <?>
  
-- | Throw an 'Iter' exception that describes expected input not
-- found.
expectedI :: String             -- ^ Input actually received
          -> String             -- ^ Description of input that was wanted
          -> Iter t m a
expectedI saw target = throwI $ IterExpected saw [target]

-- | Takes an 'Iter' returning a 'LL.ListLike' type, executes the
-- 'Iter' once, and throws a parse error if the returned value is
-- 'LL.null'.  (Note that this is quite different from the @'some'@
-- method of the @'Alternative'@ class in "Control.Applicative", which
-- executes a computation one /or more/ times.  This library does not
-- use @'Alternative'@ because @`Alternative`@'s @\<|\>@ operator has
-- left instead of right fixity.)
someI :: (ChunkData t, Monad m, LL.ListLike a e) => Iter t m a -> Iter t m a
someI iter = flip (<?>) "someI" $ do
  a <- iter
  if LL.null a then throwI $ IterMiscParseErr "someI" else return a

-- | Repeatedly invoke an 'Iter' and right-fold a function over the
-- results.
foldrI :: (ChunkData t, Monad m) =>
          (a -> b -> b) -> b -> Iter t m a -> Iter t m b
foldrI f z iter = iter \/ return z $ f >$> foldrI f z iter

-- | A variant of 'foldrI' that requires the 'Iter' to succeed at
-- least once.
foldr1I :: (ChunkData t, Monad m) =>
          (a -> b -> b) -> b -> Iter t m a -> Iter t m b
foldr1I f z iter = f <$> iter <*> foldrI f z iter

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
foldrMinMaxI nmin nmax f z iter
    | nmin > nmax = throwI $ IterMiscParseErr "foldrMinMaxI: min > max"
    | nmin > 0    = f <$> iter <*> foldrMinMaxI (nmin - 1) (nmax - 1) f z iter
    | nmax == 0   = return z
    | nmax < 0    = throwI $ IterMiscParseErr "foldrMinMaxI: negative max"
    | otherwise   = iter \/ return z $ f >$> foldrMinMaxI 0 (nmax - 1) f z iter

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

-- | Ensures the next input element satisfies a predicate or throws a
-- parse error.  Does not consume any input.
ensureI :: (ChunkData t, LL.ListLike t e, Monad m) =>
           (e -> Bool) -> Iter t m ()
ensureI test = do
  me <- peekI safeHeadI
  case me of
    Just e | test e -> return ()
    Just e          -> do t <- showt $ LL.singleton e
                          expectedI t "ensureI predicate"
    Nothing         -> expectedI "EOF" "ensureI predicate"
    where
      showt :: (Monad m, ChunkData t) => t -> Iter t m String
      showt = return . chunkShow

-- | Skip all input elements encountered until an element is found
-- that does not match the specified predicate.
skipWhileI :: (ChunkData t, LL.ListLike t e, Monad m) =>
              (e -> Bool) -> Iter t m ()
skipWhileI test = iterF $ \(Chunk t eof) ->
                  case LL.dropWhile test t of
                    t1 | LL.null t1 && not eof -> skipWhileI test
                    t1 -> Done () $ Chunk t1 eof

-- | Like 'skipWhileI', but fails if at least one element does not
-- satisfy the predicate.
skipWhile1I :: (ChunkData t, LL.ListLike t e, Monad m) =>
               (e -> Bool) -> Iter t m ()
skipWhile1I test = ensureI test >> skipWhileI test <?> "skipWhile1I"

-- | A variant of 'whileI' in which the predicate function can keep
-- state.  The predicate function returns @'Right' state@ while it
-- should accept elements, and @'Left' state@ when it hits the first
-- character that should not be included in the returned string.
whileStateI :: (LL.ListLike t e, Monad m, ChunkData t) =>
               (a -> e -> Either a a)
            -- ^ Preidcate function
            -> a 
            -- ^ Initial state
            -> Iter t m (t, a)
            -- ^ (accepted input, modified state)
whileStateI f z0 = dochunk id z0
    where
      dochunk acc z = do
        (Chunk t eof) <- chunkI
        let (end, (z', len)) = LL.foldr ff ((,) True) t (z, 0)
            (a, b)           = if end then (t, LL.empty) else LL.splitAt len t
            t'               = acc . mappend a
        if LL.null b && not eof
          then dochunk t' z'
          else Done (t' LL.empty, z') (Chunk b eof)
      ff e dorest = \(z, n) -> z `seq` n `seq`
                  case f z e of
                    Left z'  -> (False, (z', n))
                    Right z' -> dorest (z', n + 1)

{-
data InputPred e = IPred { ipredF :: e -> Bool }
                 | IPredMin { ipredMin :: Int
                            , ipredF :: e -> Bool }
                 | IPredMinMax { ipredMin :: Int
                               , ipredMax :: Int
                               , ipredF :: e -> Bool }

whilePredsI :: (LL.ListLike t e, Monad m, ChunkData t) =>
               [InputPred e] -> Iter t m t
whilePredsI preds = do
  (t, preds') <- whileStateI dopred preds
  if null preds' then return t else expectedI "whilePredsI predicates"
      where
        dec n = if n > 0 then n - 1 else 0

        step (IPredMin n f : ps)      = IPredMin (dec n) f : ps
        step (IPredMinMax n x f : ps) = IPredMinMax (dec n) (dec x) f : ps
        step ps                       = ps

        minok (IPredMin n _) | n > 0      = False
        minok (IPredMinMax n _ _) | n > 0 = False
        minok _                           = True

        maxok (IPredMinMax _ n _) | n <= 0 = False
        maxok _                            = True

        dopred [] e                                  = Left []
        dopred p@(p1:ps) e | maxok p1 && ipredF p1 e = Right $ step p
                           | minok p1                = dopred ps e
                           | otherwise               = Left p
-}


-- | Return all input elements up to the first one that does not match
-- the specified predicate.
whileI :: (ChunkData t, LL.ListLike t e, Monad m)
          => (e -> Bool) -> Iter t m t
whileI test = more LL.empty
    where
      more t0 = iterF $ \(Chunk t eof) ->
                case LL.span test t of
                  (t1, t2) | not (LL.null t2) || eof ->
                               Done (LL.append t0 t1) $ Chunk t2 eof
                  (t1, _) -> more (LL.append t0 t1)

-- | Like 'whileI', but fails if at least one element does not satisfy
-- the predicate.
while1I :: (ChunkData t, LL.ListLike t e, Monad m) =>
           (e -> Bool) -> Iter t m t
while1I test = ensureI test >> whileI test <?> "while1I"

-- | A variant of 'whileI' with a maximum number matches.
whileMaxI :: (ChunkData t, LL.ListLike t e, Monad m) =>
             Int                  -- ^ Maximum number to match
          -> (e -> Bool)          -- ^ Predicate test
          -> Iter t m t
whileMaxI nmax test = iterF $ \(Chunk t eof) ->
        let s = LL.takeWhile test $ LL.take nmax t
            slen = LL.length s
            rest = LL.drop slen t
        in if slen >= nmax || not (LL.null rest) || eof
           then Done s $ Chunk rest eof
           else LL.append s <$> whileMaxI (nmax - slen) test

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
                []  -> throwI $ IterMiscParseErr $ "readI can't parse: " ++ s
                _   -> throwI $ IterMiscParseErr $ "readI ambiguous: " ++ s

-- | Ensures the input is at the end-of-file marker, or else throws an
-- exception.
eofI :: (ChunkData t, Monad m, Show t) => Iter t m ()
eofI = do
  Chunk t eof <- peekI chunkI
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
(<:>) = liftA2 LL.cons
infixr 5 <:>

-- | @nil = 'pure' 'mempty'@ -- An empty 'Monoid' injected into an
-- 'Applicative' type.
nil :: (Applicative f, Monoid t) => f t
nil = pure mempty

-- $Parseclike
--
-- These functions are intended to be similar to those supplied by
-- "Text.Parsec".

-- | Run an 'Iter' zero or more times (until it fails) and return a
-- list-like container of the results.
many :: (ChunkData t, LL.ListLike f a, Monad m) => Iter t m a -> Iter t m f
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
sepBy item sep =
    item `orEmpty` (LL.cons >$> foldrI LL.cons LL.empty (sep *> item))

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
sepEndBy item sep =
    item `orEmpty` LL.cons >$> sep `orEmpty` (\_ -> sepEndBy item sep)


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
sepBy1 item sep = item >>= LL.cons >$> foldrI LL.cons LL.empty (sep *> item)

-- | A variant of 'endBy' that throws a parse error if it cannot
-- return at least one item.
endBy1 :: (ChunkData t, LL.ListLike f a, Monad m) =>
          Iter t m a -> Iter t m b -> Iter t m f
endBy1 item sep = foldr1I LL.cons LL.empty (item <* sep)

-- | A variant of 'sepEndBy' that throws a parse error if it cannot
-- return at least one item.
sepEndBy1 :: (ChunkData t, LL.ListLike f a, Monad m) =>
             Iter t m a -> Iter t m b -> Iter t m f
sepEndBy1 item sep =
    item >>= LL.cons >$> sep `orEmpty` (\_ -> sepEndBy item sep)

                 
-- | Read the next input element if it satisfies some predicate.
satisfy :: (ChunkData t, LL.ListLike t e, Enum e, Monad m) =>
           (e -> Bool) -> Iter t m e
satisfy test = do
  e <- headI
  if test e
    then return e
    else expectedI (show $ chr $ fromEnum e) "satify predicate"

-- | Read input that exactly matches a character.
char :: (ChunkData t, LL.ListLike t e, Eq e, Enum e, Monad m) =>
        Char -> Iter t m e
char target = satisfy (toEnum (ord target) ==) <?> show target

-- | Read input that exactly matches some target.
match :: (ChunkData t, LL.ListLike t e, Eq e, Monad m) =>
         t -> Iter t m t
match ft = doMatch ft
    where
      doMatch target | LL.null target = return ft
                     | otherwise      = do
        m <- takeI $ LL.length target
        if not (LL.null m) && LL.isPrefixOf m target
          then doMatch $ LL.drop (LL.length m) target
          else expectedI (chunkShow m) $ chunkShow target

-- | Read input that exactly matches a string.
string :: (ChunkData t, LL.ListLike t e, LL.StringLike t, Eq e, Monad m) =>
          String -> Iter t m t
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
        m <- takeI $ LL.length target
        if not (LL.null m) && m `prefix` target
          then doMatch (LL.append acc m) $ LL.drop (LL.length m) target
          else expectedI (chunkShow m) $ chunkShow target

