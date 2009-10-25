
-- | This is the main module to import for the IterIO package.  It
-- exports several other internal modules.  The documentation in this
-- gives a high-level overview of Iteratee-based IO.  See the
-- "Data.IterIO.Base" and "Data.IterIO.ListLike" modules for
-- documetation of individual functions.
module Data.IterIO
    (module Data.IterIO.Base
    , module Data.IterIO.ListLike
    -- * Overview
    -- $Overview
    ) where

import Data.IterIO.Base hiding (ChunkData(null))
import Data.IterIO.ListLike

{- $Overview

This library provides enumerator/iteratee-based IO, alternative to
lazy IO that offers referential transparency and convenient
composition of protocol layers or parsers.

An enumerator, represented by the type 'EnumO', is a source of data.

It may return
a useful result, or its use may be in the side-effects it has, such as
storing the data to a file.  Iteratees are represented by the type
@'Iter' t m a@.  @t@ is the type of the data chunks (which must be a
'ChunkData', such as 'String' or lazy 'ByteString').  @m@ is the
'Monad' in which the iteratee runs--for instance 'IO' (or an instance
of 'MonadIO') for the iteratee to perform IO.  'a' is the result type
of the iteratee, for when it has consumed enough input to produce a
result.


-}
