{-# LANGUAGE DeriveDataTypeable #-}

-- | This module contains an adapter function to run attoparsec
-- 'Parser's from within the 'Iter' monad.
module Data.IterIO.Atto where

import Control.Exception
import Data.Attoparsec as A
import Data.Typeable
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as S

import Data.IterIO

-- | Class of types whose 'Iter's can be converted to strict
-- 'S.ByteString's.  Basically just strict 'S.ByteString's and lazy
-- 'L.ByteString's.  This class mostly exists so that the 'atto'
-- function can work with either type of ByteString.
class (ChunkData t) => IterStrictByteString t where
    fromIterStrictByteString :: (Monad m) => Iter S.ByteString m a -> Iter t m a

instance IterStrictByteString S.ByteString where
    {-# INLINE fromIterStrictByteString #-}
    fromIterStrictByteString = id

instance IterStrictByteString L.ByteString where
    {-# INLINE fromIterStrictByteString #-}
    fromIterStrictByteString = (inumLtoS .|)

-- | Run an attoparsec parser in an 'Iter' monad.  Throws an
-- 'IterFail' exception with constructor 'IterParseErr' if the parse
-- fails.  (This exception can be handled with 'multiParse' and
-- 'ifParse'.)
atto :: (IterStrictByteString t, Monad m) =>
        A.Parser a -> Iter t m a
atto parser = fromIterStrictByteString $
              data0I >>= A.parseWith data0I parser >>= check
    where check (A.Done t a)   = ungetI t >> return a
          check (A.Fail t _ e) = ungetI t >> throwParseI e
          check _              = error $ "atto: Partial"

-- | Try running an attoparsec parser.  Returns @'Right' a@ if the
-- parser succeeds with result @a@.  Returns @'Left' err@ where @err@
-- is an error message otherwise.  Note that the input stream will be
-- in an indeterminate state should the parser fail.  (If you need to
-- keep parsing input from some known state, it may be better to use
-- 'atto' in conjunction with 'multiParse'.)
tryAtto :: (IterStrictByteString t, Monad m) =>
           A.Parser a -> Iter t m (Either String a)
tryAtto parser = either check Right `fmap` tryFI (atto parser)
    where check (IterParseErr e) = Left e
          check _                = error "tryAtto"
