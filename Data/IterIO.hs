
-- | This is the main module to import for the IterIO package.  It
-- exports several other internal modules.  The documentation in this
-- gives a high-level overview of Iteratee-based IO; it is intended
-- more as an introduction than a reference.  See the
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

This library performs IO by hooking /enumerators/ (which are sources
of data) to /iteratees/ (which are data sinks) in a manner reminiscent
of Unix command-line pipes.  Compared to lazy IO, the
enumerator/iteratee paradigm provides better error handing,
referential transparency (which should, after all, be one of the big
advantages of Haskell), and equally convenient composition of protocol
layers and parsers without having to think about where IO chunk
boundaries lie.

Enumerators, represented by the type 'EnumO', are sources of data.  An
'EnumO' enumerates all data elements (e.g., bytes or packets) in some
source such as a file or socket.

Iteratees, represented by the type 'Iter', are data sinks.  When
executing a pipeline, the library iterates over data elements using
the iteratee to produce a result.

Consider the following simple example:

@
    -- Return first line of file
    headFile :: FilePath -> IO String
    headFile path = 'enumFile' path '|$' 'lineI'
@

'enumFile' enumerates the contents of a file.  'lineI' returns a line
of input (discarding the newline).  '|$' is the /pipe apply/ operator
that applies an 'EnumO' to an 'Iter', returning the result of the
'Iter', which in this case is the first line of the file @path@.

An `Iter`'s main purpose may lie not in its result type, but in its
side effects.  For example, 'handleI' writes data to a file handle.
Thus, the following function copies the contents of a file handle to
standard output:

@
    -- Copy file to standard output
    catFile :: FilePath -> IO ()
    catFile path = 'enumFile'' path '|$' 'handleI' stdout
@

'enumFile'' is like 'enumFile' above, but always returns data using
the lazy 'ByteString' type, which is more efficient than plain
'String's.  ('enumFile' supports multiple types, but in this example
there is not enough enformation for Haskell to choose one of them, so
we must use 'enumfile'' or use a cast.)  Once again, '|$' is used to
execute the IO actions, but this time, the return value is just @()@;
the interesting action lies in the side effects of writing to standard
output.

The real power of the iteratee abstraction lies in the fact that
'Iter's are monads.  One 'Iter' may invoke another to make use of its
results.  Here is an example of a function that returns the first two
lines of a file:

@
    -- | Return first two lines of file
    head2File :: FilePath -> IO (String, String)
    head2File path = 'enumFile' path |$ lines2I
@

@
    -- | Iter that returns next two lines as a pair
    lines2I :: (Monad m) => 'Iter' String m (String, String)
    lines2I = do
      line1 <- 'lineI'
      line2 <- 'lineI'
      return (line1, line2)
@

This example illustrates several points.  First, consider the type of
the @lines2I@ function:  @'Iter' String m (String, String)@.  The
'Iter' type constructor takes three type arguments.  The first,
'String' in this case, specifies the type of input expected by the
iteratee.  The last type, @(String, String)@ in this case, specifies
the result type of the iteratee.  Finally, the middle type, @m@, is a
monad, which illustrates another point:  @'Iter' t@ (for any input
type @t@) is a monad transformer.  In this case, when @head2File@
invokes @lines2I@, @m@ will be @IO@, because @head2File@ is returning
a result in the @IO@ monad.  However, @lines2I@ would work equally
well with any other monad.


-}
