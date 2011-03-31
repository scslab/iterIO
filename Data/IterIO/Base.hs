{- | This module contains the base Enumerator/Iteratee IO
     abstractions.  See the documentation in the "Data.IterIO" module
     for a high-level tutorial on these abstractions.

     An iteratee is a data sink that is fed chunks of data.  It may
     return a useful result, or its utility may lie in monadic
     side-effects, such as storing received data to a file.  Iteratees
     are represented by the type @'Iter' t m a@.  @t@ is the type of
     the data chunks the iteratee receives as input.  (@t@ must be an
     instance of 'ChunkData', such as 'String' or lazy
     'L.ByteString'.)  @m@ is the 'Monad' in which the iteratee
     runs--for instance 'IO' (or an instance of 'MonadIO') for the
     iteratee to perform IO.  @a@ is the result type of the iteratee,
     for when it has consumed enough input to produce a result.

     An enumerator is a data source that feeds data chunks to an
     iteratee.  In this library, all enumerators are also iteratees.
     We use the type 'Inum' to represent these /iteratee-enumerators/.
     As an iteratee, an 'Inum' sinks data of some input type,
     generally designated @tIn@.  As an enumerator, the 'Inum' feeds
     data of a potentially different type, @tOut@, to another
     iteratee.  Thus, the 'Inum' can be viewed as transcoding data
     from type @tIn@ to type @tOut@ for consumption by another
     iteratee.

     'Inum's are generally constructed using the functions @'mkInum'@
     and @'mkInumM'@ in module "Data.IterIO.Inum".  The first function
     uses a simple @'Iter' tIn m tOut@ to translate between input type
     @tIn@ and output type @tOut@.  The second function, @'mkInumM'@,
     allows construction of more complex 'Inum's.

     An important special kind of 'Inum' is an /outer enumerator/,
     which is just an 'Inum' with the void input type @()@.  Outer
     enumerators are sources of data.  Rather than transcode input
     data, they produce data from monadic actions (or from pure data
     in the case of 'enumPure').  The type 'Onum' represents outer
     enumerators and is a synonym for 'Inum' with an input type of
     @()@.

     To execute iteratee-based IO, you must apply an 'Onum' to an
     'Iter' with the '|$' (\"pipe apply\") binary operator.

     An important property of enumerators and iteratees is that they
     can be /fused/.  The '|.' operator fuses two 'Inum's together
     (provided the output type of the first is the input type of the
     second), yielding a new 'Inum' that transcodes from the input
     type of the first to the output type of the second.  Similarly,
     the '.|' operator fuses an 'Inum' to an 'Iter', yielding a new
     'Iter' with a potentially different input type.

     Enumerators of the same type can also be /concatenated/, using
     the 'cat' function.  @enum1 ``cat`` enum2@ produces an enumerator
     whose effect is to feed first @enum1@'s data then @enum2@'s data
     to an 'Iter'.

 -}

module Data.IterIO.Base where

import Data.IterIO.Iter
import Data.IterIO.Inum
