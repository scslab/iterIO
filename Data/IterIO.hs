
-- | This is the main module to import for the IterIO package.  It
-- exports several other internal modules.  The module's documentation
-- gives a high-level overview of Iteratee-based that is intended more
-- as an introduction than as a reference.  See the "Data.IterIO.Base"
-- and "Data.IterIO.ListLike" modules for documetation of individual
-- functions.
module Data.IterIO
    (module Data.IterIO.Base
    , module Data.IterIO.ListLike
    -- * Overview
    -- $Overview
    ) where

import Data.IterIO.Base hiding (ChunkData(null))
import Data.IterIO.ListLike

{- $Overview

This library performs IO by hooking up sources of data, called
/enumerators/, to data sinks, called /iteratees/, in a manner
reminiscent of Unix command pipelines.  Compared to lazy IO, the
enumerator/iteratee paradigm provides better error handing,
referential transparency (which should, after all, be one of the big
advantages of Haskell), and equally convenient composition of protocol
layers and parsers without worrying about IO chunk boundaries.

Enumerators, implemented by the type 'EnumO', are so called because
they enumerate all data elements (e.g., bytes or packets) in some
source such as a file or socket.  Hence, an enumerator should be
viewed as a /source/ outputting data.

Conversely, iteratees, implemented by the type 'Iter', should be
viewed as /sinks/ consuming data.  When executing IO, the library
/iterates/ over all data elements output by the source, using an
iteratee to produce a result.

Here is a simple example:

@
    -- Return the first line of file
    headFile :: FilePath -> IO String
    headFile path = 'enumFile' path '|$' 'lineI'
@

'enumFile' enumerates the contents of a file.  'lineI' returns a line
of input (discarding the newline).  '|$' is the /pipe apply/ operator
that applies an 'EnumO' to an 'Iter', returning the result of the
'Iter'--in this case is the first line of the file @path@.

An `Iter`'s main purpose may not be to produce a result.  Some 'Iter's
are primarily useful for their side effects.  For example, 'handleI'
writes data to a file handle.  Thus, the following function copies the
contents of a file to standard output:

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
the interesting action lies in the side effects of writing data to
standard output while iterating over the input with 'handleI'.

Now the real power of the iteratee abstraction lies in the fact that
'Iter's are monads.  One 'Iter' may invoke another to make use of the
first one's results.  Here is an example of a function that returns
the first two lines of a file:

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
monad, because @'Iter' t@ (for any input type @t@) is a monad
transformer.  In this case, when @head2File@ invokes @lines2I@, @m@
will be @IO@, because @head2File@ is returning a result in the @IO@
monad.  However, @lines2I@ would work equally well with any other
monad.

Next notice how @'Iter' String m@ works as a monad.  The type of
'lineI' in the above example is @'Iter' String m String@.  The
@lines2I@ function executes 'lineI' twice using monadic @do@ syntax,
then injects a pair of strings into the @'Iter' String m@ monad with
@return@.

So that explains the 'Iter' type.  'EnumO' has the same three type
arguments.  Thus, the type of 'enumFile' is @'enumFile' :: 'EnumO'
String IO a@.  Most 'EnumO' types are polymorphic in the last
argument, so as to be able to return whatever type the iteratee is
returning.  (In fact, 'EnumO' is also polymorphic in the first two
arguments, so as to work with multiple String-like types as well as
any 'MonadIO' monad.)

Here is an example of an 'Iter' with side effects:

@
    liftIOexampleI :: (MonadIO m) => 'Iter' String m ()
    liftIOexampleI = do
      line <- 'lineI'
      liftIO $ putStrLn $ "First line is: " ++ line
      next <- 'stringExactI' 40
      liftIO $ putStrLn $ "And the next 40 bytes are: " ++ next
@

Unlike @lines2I@, @liftIOexampleI@ does not return any interesting
result, but it uses the 'liftIO' monad transformer method to output
the first line of the file, followed by the next 40 bytes.  The
'stringExactI' returns a 'String' (or 'ByteString') with exactly the
requested number of bytes, unless an EOF (end-of-file) is encountered.

Of course, the real power of command pipelines is that you can hook
multiple commands together.  For instance, say you want to know how
many words in the system dictionary files contain a double k and start
with a lower-case letter.  You would run a command like this:

>    cat /usr/share/dict/words /usr/share/dict/extra.words \
>        | grep kk | grep '^[a-z]' | wc -l

Let's see how to do something equivalent with iteratees, starting with
the @wc -l@ command, which counts lines.  Here is an equivalent iteratee:

@
    lineCountI :: (Monad m) => 'Iter' String m Int
    lineCountI = count 0
        where
          count n = do
            line <- 'safeLineI'
            case line of
              Just _  -> count (n+1)
              Nothing -> return n
@

The 'safeLineI' function is like 'lineI', but returns a @'Maybe'
'String'@ (or @'Maybe' 'ByteString'@) which is 'Nothing' upon an EOF
condition.

What about the @grep@ command?  @grep@ sits in the middle of a
pipeline, so it acts as both a data sink, and a data source.  In the
iteratee world, we call such pipeline stage an /inner enumerator/, or
'EnumI'.  Before defining our @grep@ equivalent, since multiple
pipeline stages are going to be considering the file one line at a
time, let's first build an 'EnumI' to separate input into lines:

@
    inumToLines :: (Monad m) => 'EnumI' S.ByteString [S.ByteString] m a
    inumToLines = 'enumI'' $ do
                    line <- 'lineI'
                    return [line]
@

'EnumI' takes four type arguments, compared to only three for 'EnumO'.
That's because an 'EnumI' is acting as both an iteratee and an
enumerator, and it needn't be processing the same type of data in both
roles.  In the above example, when acting as an iteratee,
@inumToLines@ sinks data of type @S.ByteString@ (the first type
argument), accepting one long stream of unstructured bytes.  However,
as an enumerator, @inumToLines@ produces output of type
@[S.ByteString]@ (the second type argument)--a /list/ of strings, one
per line of the file.

(Note that data is often viewed as flowing inwards from an outer
enumerator, through inner enumerators, to iteratees.  Thus, inner
enumerators often have types with names like @tOut@ and @tIn@, where
@tOut@ is the outer data type, i.e., the input type of the inner
enumerator.  Generally you should read @tOut@ and @tIn@ as "outer
type" and "inner type", which is unfortunately the opposite of "output
type" and "input type".)


Inner-enumerators are generally constructed using either 'enumI' or
`enumI'`, and by convention most 'EnumI' functions have names starting
@inum@.  'enumI'' takes an argument of type @Iter t1 m t2@ that
transcodes type @t1@ to type @t2@.  (In this case, @t1@ is
@S.ByteString@ and @t2@ is @[S.ByteString]@).  'enumI' is like
`enumI'`, but returns data 'Chunk's, the library's internal
representation for data that allows finer-grained control of EOF
conditions.  In this example, we just to let 'lineI' throw an
exception on EOF and `enumI'` will do the right thing.

We similarly define an 'EnumI' to filter out lines not matching a
regular expression (using the "Text.Regex.Posix.ByteString" library),
and a simple 'EnumI' to count list elements (since @lineCountI@ has
the wrong type after we break files into lines with @inumToLines@):

@
    inumGrep :: (Monad m) => String -> 'EnumI' [S.ByteString] [S.ByteString] m a
    inumGrep re = `enumI'` $ do
      line <- 'headI'
      return $ if line =~ packedRe then [line] else []
        where
          packedRe = S8.pack re
@

@
    lengthI :: (Monad m) => 'Iter' [t] m Int
    lengthI = count 0
        where
          count n = do
            line <- 'safeHeadI'
            case line of
              Just _  -> count (n+1)
              Nothing -> return n
@

Now we are almost ready to assemble all of the pieces.  But recall
that the '|$' operator applies one 'EnumO' to one 'Iter', yet now we
have two 'EnumO's (because we want to look through two files), and
three 'EnumI's that we want to compose into a pipeline.  For this the
library has two types of composition functions:  /concatenation/
functions, and /fusing/ operators.

Two 'EnumO's of the same type can be /concatenated/ with the 'cat'
function, producing a new data source that enumerates first all of the
data in the first 'EnumO', then all of the data in the second.  (There
is a similar 'catI' function for inner enumerators, though it is less
frequently used.)

There are three /fusing/ operators.  The '|..' operator fuses an
'EnumO' to an 'EnumI', producing a new 'EnumO'.  (Mnemonic: it
produces a pipeline that is open on the right hand side, as it still
needs to be applied to an iteratee with '|$'.)  The '..|' operator
fuses an 'EnumI' to an 'Iter', producing a new 'Iter'.  Finally, there
is a '..|..' operator that fuses two 'EnumI's into a single new
'EnumI' that composes their effects.

The concatenation operators bind more tightly than the fusing
operators, which in turn bind more tightly than '|$.  Thus, putting it
all together, here is our Haskell equivalent to the above Unix
pipeline:

@
    grepCount :: IO Int
    grepCount = 'enumFile' \"\/usr\/share\/dict\/words\"
                         ``cat`` 'enumFile' \"\/usr\/share\/dict\/extra.words\"
                    '|..' inumToLines
                '|$' inumGrep \"kk\"
                    '..|' inumGrep \"^[a-z]\"
                    '..|' lengthI
@

One often has a choice as to whether to fuse an 'EnumI' to the
'EnumO', or to the 'Iter'.  For example, @grepCount@ could also have
been implemented almost equivalently as:

@
    grepCount :: IO Int
    grepCount = 'enumFile' \"\/usr\/share\/dict\/words\"
                         ``cat`` 'enumFile' \"\/usr\/share\/dict\/extra.words\"
                    '|..' inumToLines
                    '|..' inumGrep \"kk\"
                    '|..' inumGrep \"^[a-z]\"
                '|$' lengthI
@

The difference lies in the error handling.  The library distinguishes
between /enumerator failures/ and /iteratee failures/, as applications
generally need to handle input errors differently from output errors.
Often output errors are more serious than input errors.  For instance,
in the above example, if 'enumFile' fails because one of the files
does not exist, you might want to continue processing lines from the
next file.  In fact, 'EnumO' failures preserve the 'Iter' state so as
to allow it to be passed off to another 'EnumO'.  Conversely, if the
iteratee @lengthI@ fails, there is not much point in continuing the
program.

Generally you should fuse an 'EnumI' to an 'EnumO' if you want to be
able to recover from the `EnumI`'s failure.  In the @grepCount@
example, if, for instance, a regular expression fails to compile, this
is catastrophic for the program, which is why the first version fused
@inumGrep@ to the @Iter@.


-}
