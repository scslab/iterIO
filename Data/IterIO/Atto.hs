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

-- | An attoparsec parsing error occured
data IterAttoParsecErr = IterAttoParsecErr [String] String
    deriving (Show, Typeable)

instance Exception IterAttoParsecErr where
    toException = toException . IterNoParse
    fromException s = do IterNoParse e <- fromException s; cast e

-- | Class of types whose 'Iter's can be converted to strict
-- 'S.ByteString's.  Basically just strict 'S.ByteString's and lazy
-- 'L.ByteString's.  This class mostly exists so that the 'atto'
-- function can work with either type of ByteString.
class IterStrictByteString t where
    fromIterStrictByteString :: (Monad m) => Iter S.ByteString m a -> Iter t m a

instance IterStrictByteString S.ByteString where
    fromIterStrictByteString = id

instance IterStrictByteString L.ByteString where
    fromIterStrictByteString = (inumLtoS .|)

-- | Run an attoparsec parser in an 'Iter' monad.
atto :: (IterStrictByteString t, Monad m) =>
        A.Parser a -> Iter t m a
atto parser = fromIterStrictByteString $
              data0I >>= A.parseWith data0I parser >>= check
    where check (A.Done t a)    = ungetI t >> return a
          check (A.Fail t cl e) = ungetI t >> throwI (IterAttoParsecErr cl e)
          check _               = error $ "atto: Partial"
