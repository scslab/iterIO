
-- | This module contains functions to help parsing input from within
-- iteratees.  Many of the operators are imported from or inspired by
-- "Data.Applicative" and "Text.Parsec".  Some operators, notably
-- '\<|\>', are re-defined here to have different precedence and
-- associativity.  (The @'Alternative'@ class's @\<|\>@ operator is
-- left associative, which would be very inefficient with iteratees.)

module Data.IterIO.Parse (matchI
                         , (<|>), (<$>), (<$), Applicative(..), (<**>)
                         ) where

import Control.Applicative (Applicative(..), (<**>))
import Control.Monad
import Data.Functor ((<$>), (<$))
import qualified Data.ListLike as LL

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

-- | Read input that exactly matches a string, or else fail.
matchI :: (ChunkData t, LL.ListLike t e, LL.StringLike t, Eq e, Monad m) =>
          String -> Iter t m ()
matchI fulltarget = doMatch $ LL.fromString fulltarget
    where
      doMatch target | LL.null target = return ()
                     | otherwise      = do
        m <- stringMaxI $ LL.length target
        if not (LL.null m) && LL.isPrefixOf m target
          then doMatch $ LL.drop (LL.length m) target
          else fail $ "matchI expected " ++ show fulltarget

