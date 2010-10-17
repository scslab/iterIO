{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DeriveDataTypeable #-}

-- | This module provides 'CtlStream', a data type that is a wrapper
-- around any 'ChunkData' type that allows control requests to be
-- interspersed with data.  It is useful to construct 'Iter's and
-- 'Inum's on 'CtlStream' types if upstream enumerators need to send
-- control requests such as flush indicators (or maybe sendfile) to
-- downstream iteratees.
module Data.IterIO.CtlStream
    (CtlStream (..), csData, csCtl
    , inumToCS, inumFromCS
    , cschunkI
    ) where

import Prelude hiding (null)
import Data.Typeable
import Data.Monoid

import Data.IterIO.Base
import Data.IterIO.Inum

-- | A @CtlStream@ is a stream of data of type @t@ interspersed with
-- control requests of type @d@.  Each non-empty @CtlStream@ consists
-- of data or control requests followed by another @CtlStream@.
data CtlStream t d = CSData !t (CtlStream t d)
                   -- ^ Data in the stream.  Invariants:  @t@ must
                   -- always be non-null and the next @CtlStream@ must
                   -- never be of type @CSData@.
                   | CSCtl !d (CtlStream t d)
                   -- ^ A control request.
                   | CSEmpty
                   -- ^ An empty stream with no data or control requests.
                     deriving (Show, Typeable)

-- | Construct a 'CtlStream' given some data.
csData :: (ChunkData t) => t -> CtlStream t d
csData t | null t    = CSEmpty
         | otherwise = CSData t CSEmpty

-- | Construct a 'CtlStream' given a control request.
csCtl :: d -> CtlStream t d
csCtl d = CSCtl d CSEmpty

csAppend :: (Monoid t) => CtlStream t d -> CtlStream t d -> CtlStream t d
csAppend a CSEmpty                          = a
csAppend CSEmpty b                          = b
csAppend (CSData ah CSEmpty) (CSData bh bt) = CSData (mappend ah bh) bt
csAppend (CSData ah at) b                   = CSData ah $ csAppend at b
csAppend (CSCtl ah at) b                    = CSCtl ah $ csAppend at b

instance (Monoid t) => Monoid (CtlStream t d) where
    mempty = CSEmpty
    mappend = csAppend

instance (ChunkData t, Show d) => ChunkData (CtlStream t d) where
    null CSEmpty = True
    null _       = False
    chunkShow CSEmpty         = "CSEmpty"
    chunkShow (CSData t rest) = chunkShow t ++ "+" ++ chunkShow rest
    chunkShow (CSCtl d rest)  = show d ++ "+" ++ chunkShow rest

-- | Adapt an ordinary data stream into a 'CtlStream' by wrapping all
-- data in the 'CSData' constructor.
inumToCS :: (ChunkData t, Show d, Monad m) => Inum t (CtlStream t d) m a
inumToCS = mkInumM $ withCleanup pullup $ irepeat $ dataI >>= ifeed . csData
    where pullup = ipopresid >>= ungetI . flatten
          flatten (CSData h t) = mappend h $ flatten t
          flatten (CSCtl _ t)  = flatten t
          flatten CSEmpty      = mempty

-- | Return the next chunk of data or next control request in a
-- 'CtlStream'.  On end of file, returns 'CSEmpty'.  On any other
-- condition, returns a 'CtlStream' in which the tail is always
-- 'CSEmpty' and thus may be ignored.
cschunkI :: (ChunkData t, Show d, Monad m) =>
            Iter (CtlStream t d) m (CtlStream t d)
cschunkI = chunkI >>= check
    where check (Chunk (CSData h t) eof) = Done (csData h) (Chunk t eof)
          check (Chunk (CSCtl h t) eof)  = Done (csCtl h) (Chunk t eof)
          check (Chunk CSEmpty True)     = Done CSEmpty (Chunk mempty True)
          check _                        = cschunkI

-- | Strip control requests out of an input stream of type
-- 'CtlStream', to produce a stream of ordinary data.
inumFromCS :: (ChunkData t, Show d, Monad m) => Inum (CtlStream t d) t m a
inumFromCS = mkInumM $ withCleanup pullup $ irepeat $ cschunkI >>= feed
    where pullup = ipopresid >>= ungetI . csData
          feed (CSData t _) = ifeed t
          feed (CSCtl _ _)  = return False
          feed CSEmpty      = idone
