
-- | This is the main module to import for the IterIO package.  It
-- exports several other internal modules.  This module's
-- documentation gives a high-level overview of the iteratee model,
-- intended more as an introduction than as a reference.  See the
-- "Data.IterIO.Base" and "Data.IterIO.ListLike" modules for more
-- detailed documentation of data structures and functions.
module Data.IterIO
    (module Data.IterIO.Base
    , module Data.IterIO.ListLike
    -- * Overview
    -- $Overview
    ) where

-- import Prelude hiding (catch)
import Data.IterIO.Base hiding (null, run -- names that might collide
                               )
import Data.IterIO.ListLike

{- $Overview

This library performs IO by hooking up sources of data, called
/enumerators/, to data sinks, called /iteratees/, in a manner
reminiscent of Unix command pipelines.  Compared to lazy IO, the
enumerator/iteratee paradigm provides better error handing,
referential transparency (which should, after all, be one of the big
advantages of Haskell), and equally convenient composition of protocol
layers and parsers without worrying about IO chunk boundaries.

Enumerators, implemented by the type 'Onum' (short for
/outer enumerator/, for reasons that will become clear below), are so
called because they enumerate all data elements (e.g., bytes or
packets) in some source such as a file or socket.  Hence, an
enumerator should be viewed as a /source/ outputting chunks of data
whose type is a @'Monoid'@.

Conversely, iteratees, implemented by the type 'Iter', should be
viewed as /sinks/ consuming data.  When executing IO, the library
/iterates/ over all data elements output by the source, using an
iteratee to produce a result.  The source may output data in chunks
whose boundaries do not coincide with logical message units; iteratees
handle this transparently, simplifying programming.

Here is a simple example:

@
    -- Return the first line of a file
    headFile :: FilePath -> IO String
    headFile path = 'enumFile' path '|$' 'lineI'
@

'enumFile' enumerates the contents of a file.  'lineI' returns a line
of input (discarding the newline).  '|$' is the /pipe apply/ operator
that applies an 'Onum' to an 'Iter', returning the result of the
'Iter'--in this case the first line of the file named @path@.

An `Iter`'s main purpose may not be to produce a result.  Some 'Iter's
are primarily useful for their side effects.  For example, 'handleI'
writes data to a file handle.  Thus, the following function copies the
contents of a file to standard output:

@
    -- Copy file to standard output
    catFile :: FilePath -> IO ()
    catFile path = 'enumFile'' path '|$' 'handleI' stdout
@

'enumFile'' is like 'enumFile' above, but type restricted to data in
the lazy 'ByteString' format, which is more efficient than plain
'String's.  ('enumFile' supports multiple types, but in this example
there is not enough enformation for Haskell to choose one of them, so
we must use 'enumfile'' or use @::@ to specify a type explicitly.)
Once again, '|$' is used to execute the IO actions, but, this time,
the return value is just @()@; the interesting action lies in the side
effects of writing data to standard output while iterating over the
input with 'handleI'.

The real power of the iteratee abstraction lies in the fact that
'Iter's are monad computations.  One 'Iter' may invoke another to make use of the
first one's results.  Here is an example of a function that returns
the first two lines of a file:

@
    -- | Return first two lines of file
    head2File :: FilePath -> IO (String, String)
    head2File path = 'enumFile' path '|$' lines2I
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
monad, because @'Iter' t@ (for a given input type @t@) is a monad
transformer.  In this case, when @head2File@ invokes @lines2I@, @m@
will be @IO@, because @head2File@ is returning a result in the @IO@
monad.  However, @lines2I@ would work equally well with any other
monad.

Next, notice the functioning of @'Iter' String m@ as a monad.  The
type of 'lineI' in the above example is @'Iter' String m String@.  The
@lines2I@ function executes 'lineI' twice using monadic @do@ syntax to
bind the results to @line1@ and @line2@.  The monadic bind operator
hides the details of IO chunk boundaries.  If, for instance, 'lineI'
needs more input because a newline character has not yet been
read, 'lineI' returns to the containing enumerator asking for more
data.  If the first 'lineI' receives more than a line of input, it
simply passes the unused input on to the next invocation of 'lineI'.
Both of these actions are hidden by the syntax, making most code much
easier to read and write.

That explains the iteratee type 'Iter'.  The enumerator type, 'Onum',
has the same three type arguments.  Thus, the type of 'enumFile' as
instantiated in the above examples is
@'enumFile' :: 'Onum' String IO a@.  Most 'Onum' types are
polymorphic in the last argument, so as to be able to return whatever
type the iteratee is returning.  (In fact, 'enumFile' is polymorphic
in the first two arguments, too, so as to work with multiple
String-like types and any monad in the @'MonadIO'@ class.)

Here is an example of an 'Iter' with side effects:

@
    liftIOexampleI :: (MonadIO m) => 'Iter' String m ()
    liftIOexampleI = do
      line <- 'lineI'
      liftIO $ putStrLn $ \"First line is: \" ++ line
      next <- 'stringExactI' 40
      liftIO $ putStrLn $ \"And the next 40 bytes are: \" ++ next
@

Unlike @lines2I@, @liftIOexampleI@ does not return any interesting
result, but it uses the @'liftIO'@ monad transformer method to output
the first line of the file, followed by the next 40 bytes.  The
'stringExactI' iteratee returns a 'String' (or 'ByteString') with exactly the
requested number of bytes, unless an EOF (end-of-file) is encountered.

Of course, the real power of command pipelines is that you can hook
multiple commands together.  For instance, say you want to know how
many words in the system dictionary files contain a double k and start
with a lower-case letter.  You could run a command like this:

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
condition.  ('lineI' throws an exception on EOF.)

What about the @grep@ command?  @grep@ sits in the middle of a
pipeline, so it acts both as a data sink and as a data source.  In the
iteratee world, we call such a pipeline stage an /inner enumerator/, or
'Inum'.  Before defining our @grep@ equivalent, since multiple
pipeline stages are going to be considering the file one line at a
time, let's first build an 'Inum' to separate input into lines:

@
    import Data.ByteString as S
    import Data.ByteString.Char8 as S8
@

@
    -- | Break input into lines of type S.ByteString, as this type
    -- works most conveniently with regular expressions.  (Otherwise,
    -- we would prefer lazy ByteStrings.)
    inumToLines :: (Monad m) => 'Inum' S.ByteString [S.ByteString] m a
    inumToLines = 'enumI'' $ do
                    line <- 'lineI'
                    return [line]
@

'Inum' takes four type arguments, compared to only three for 'Onum'.
That's because an 'Inum' is acting as both an iteratee and an
enumerator, and it needn't be processing the same type of data in both
roles.  In the above example, when acting as an iteratee,
@inumToLines@ sinks data of type @S.ByteString@ (the first type
argument), accepting one long stream of unstructured bytes.  However,
as an enumerator, @inumToLines@ produces output of type
@[S.ByteString]@ (the second type argument)--a /list/ of strings, one
per line of the file.

(Note that data is often viewed as flowing inwards from an outer
enumerator, through inner enumerators, to iteratees.  Thus, inner
enumerators often have types like @'Inum' tOut tIn m a@, where @tOut@
is the outer data type, i.e., the input type of the inner enumerator.
You should read @tOut@ and @tIn@ as \"outer type\" and \"inner type\",
which is unfortunately the opposite of \"output type\" and \"input
type\".)


Inner-enumerators are generally constructed using either 'enumI' or
`enumI'`, and by convention most 'Inum' functions have names starting
\"@inum@...\".  'enumI'' takes an argument of type @Iter t1 m t2@ that
transcodes type @t1@ to type @t2@.  (For @inumToLines@, @t1@ is
@S.ByteString@ and @t2@ is @[S.ByteString]@).  'enumI' is like
`enumI'`, but the transcoding function is of type @'CodeC' t1 m t2@,
which is designed to allow the transcoder to keep state from one
invocation to the next (unlike @'Iter' t1 m t2@), as well as to signal
EOF explicitly.  In @inumToLines@, we do not need to keep state and
are happy just to let 'lineI' throw an exception on EOF.  `enumI'`
will catch the EOF exception and do the right thing.

We similarly define an 'Inum' to filter out lines not matching a
regular expression (using the "Text.Regex.Posix.ByteString" library),
and a simple 'Inum' to count list elements (since @lineCountI ::
'Iter' String m Int@ has input data type @String@, while after
@inumToLines@ we need an 'Iter' with input data type
@[S.ByteString]@).

@
    inumGrep :: (Monad m) => String -> 'Inum' [S.ByteString] [S.ByteString] m a
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

Now we are almost ready to assemble all the pieces.  But recall that
the '|$' operator applies one 'Onum' to one 'Iter', yet now we have
two 'Onum's (because we want to look through two files), and three
'Inum's that we want to compose into a pipeline.  The library
supports two types of composition for pipeline stages:
/concatenation/ and /fusing/.

Two 'Onum's of the same type can be /concatenated/ with the 'cat'
function, producing a new data source that enumerates all of the
data in the first 'Onum' followed by all of the data in the second.  (There
is a similar 'catI' function for inner enumerators, though it is less
frequently used.)

There are three /fusing/ operators.  The '|..' operator fuses an
'Onum' to an 'Inum', producing a new 'Onum'.  (Mnemonic: it
produces a pipeline that is open on the right hand side, as it still
needs to be applied to an iteratee with '|$'.)  The '..|' operator
fuses an 'Inum' to an 'Iter', producing a new 'Iter'.  Finally, there
is a '..|..' operator that fuses two 'Inum's into a single, new
'Inum', composing their effects.

The fusing operators bind more tightly than the infix concatenation
functions, which in turn bind more tightly than '|$'.  (Concatenation
operators can also be used through prefix function application, which
binds most tightly.)  Hence, putting it all together, we produce the
following Haskell equivalent to the above Unix pipeline:

@
    grepCount :: IO Int
    grepCount = 'enumFile' \"\/usr\/share\/dict\/words\" '|..' inumToLines
                    ``cat`` 'enumFile' \"\/usr\/share\/dict\/extra.words\" '|..' inumToLines
                '|$' inumGrep \"kk\"
                        '..|' inumGrep \"^[a-z]\"
                        '..|' lengthI
@

One often has a choice as to whether to fuse an 'Inum' to the
'Onum', or to the 'Iter'.  For example, @grepCount@ could
alternatively have been implemented as:

@
    grepCount' :: IO Int
    grepCount' = 'cat' ('enumFile' \"\/usr\/share\/dict\/words\" '|..' inumToLines)
                         ('enumFile' \"\/usr\/share\/dict\/extra.words\" '|..' inumToLines)
                    '|..' inumGrep \"kk\"
                    '|..' inumGrep \"^[a-z]\"
                 '|$' lengthI
@

The difference lies in the error handling.  The library distinguishes
between /enumerator failures/ and /iteratee failures/, as applications
need to handle input errors differently from output errors.  Often
output errors are more serious than input errors.  For instance, in
the above example, if 'enumFile' fails because one of the files does
not exist, you might want to continue processing lines from the next
file.  In fact, 'Onum' failures preserve the 'Iter' state so as to
allow it to be passed off to another 'Onum'.  Conversely, if the
iteratee @lengthI@ fails, there is not much point in continuing the
program.

As a rule of thumb, you should fuse an 'Inum' to an 'Onum' when you
want to be able to recover from the `Inum`'s failure.  In the
@grepCount@ example, if, for instance, a regular expression fails to
compile, this is catastrophic for the program, which is why the first
version fused @inumGrep@ to the @Iter@.

Another alternative would have been to swap the order of concatenation
and fusing:

@
    grepCount'' :: IO Int
    grepCount'' = 'cat' ('enumFile' \"\/usr\/share\/dict\/words\")
                           ('enumFile' \"\/usr\/share\/dict\/extra.words\")
                      '|..' inumToLines
                  '|$' inumGrep \"kk\"
                      '..|' inumGrep \"^[a-z]\"
                      '..|' lengthI
@

This last version changes the semantics of the counting slightly.
With @grepCount''@, if the first file has an incomplete last line,
this line will be merged with the first line of the second file, which
is probably not what you want.  (For instance, if the incomplete last
line of the first file starts with a capital letter, then the first
line of the second file will not be counted even if it starts with a
lower-case letter and contains two \"k\"s.)

One limitation of all the @grepCount@ variants shown so far is that if
the first file does not exist, the whole operation aborts.  This
might or might not be reasonable when counting lines, but in other
contexts we may want to resume after failure.  Suppose we want to
implement a function like the Unix @grep@ command that searches for a
string in a bunch of files and prints all matching lines.  If opening
or reading a file produces an error, the function should print the
error message and continue on with the next file.

Error handling is provided by the 'catchI', 'enumCatch' and 'throwI'
functions, which are roughly equivalent to the standard library
@'catch'@ and @'throwIO'@ functions.  Because @'catch'@ only works in
the IO monad, 'catchI' and 'enumCatch' work by propagating synchronous
exceptions through the 'Iter' monad.  @'liftIO'@ transforms IO errors
into such synchronous exceptions.  Unfortunately, there is no way to
handle asynchronous exceptions such as those that arise in lazily
evaluated pure code (e.g., divide by zero) or those thrown by another
thread using @'throwTo'@.  Fortunately, for our @grep@ example, we
only need to catch IO errors.

Here is the @grep@ code.  We will analyze it below.

@
    grep :: String -> [FilePath] -> IO ()
    grep re files
        | null files = 'enumHandle' stdin '|..' inumToLines '|$' inumGrep re '..|' linesOutI
        | otherwise  = foldr1 'cat' (map enumLines files) '|$' inumGrep re '..|' linesOutI
        where
          enumLines file = 'enumCatch' ('enumFile' file '|..' inumToLines) handler
          handler :: 'IOError' -> 'Iter' [S.ByteString] IO a -> 'Iter' [S.ByteString] IO a
          handler e iter = do
            liftIO (hPutStrLn stderr $ show e)
            'resumeI' iter
          linesOutI = do
            mline <- 'safeHeadI'
            case mline of
              Just line -> do liftIO $ S.putStrLn line
                              linesOutI
              Nothing -> return ()
@

There are two cases.  If the list of files to search is null, @grep@
simply reads from standard input, in which case there is only one
input stream and we do not care about resuming.  In the second case,
we use @'foldr1' cat@ to concatenate a list of 'Onum's, a fairly
common idiom.  Each 'Onum' is generated by the function @enumLines@,
which fuses 'enumFile' to our previously defined @inumToLines@, but
also wraps the exception handler function @handler@ around the
enumerator using the 'enumCatch' function.

Note that unlike @catch@, 'enumCatch' expects an exception handler to
have /two/ arguments.  The first argument, @e@ in this example, is the
exception itself.  As with @catch@, the type of @e@ determines which
exceptions are caught, which is why we must either specify an explicit
type signature for @handler@ or somewhere specify @e@'s type
explicitly, for instance with:

>            liftIO (hPutStrLn stderr $ show (e :: IOError))

The second argument to @handler@, @iter@, is the failing state, which
contains more information than just the exception.  In the case of an
enumerator failure, it contains the state of the iteratee (which has
not failed).  The function 'resumeI' extracts this state and returns
it, so that the next enumerator in a concatenated series can continue
feeding input to the iteratee.  If, instead of resuming, you want to
re-throw the error, it suffices to re-execute the failing iteratee to
propagate the error.  For instance, suppose we want to continue
executing @grep@ when a named file does not exist, but if some other
error happens, we want to re-throw the exception to abort the whole
program.  This could be achieved as follows:

>          handler e iter = do
>            if isDoesNotExistError e
>              then do liftIO (hPutStrLn stderr $ show e)
>                      resumeI iter
>              else iter

Because printing an exception is so common, there is a function
'verboseResumeI' that prints exceptions before resuming (also
prefixing the program name).  So we can simplify the above function
to:

>          handler e iter = if isDoesNotExistError e
>                             then verboseResumeI iter
>                             else iter

This final version also gets rid of the need for an explicit type
signature, because the function @isDoesNotExistError@ has argument
type 'IOError', constraining the type of @e@ to the type of exceptions
we want to catch.

-}
