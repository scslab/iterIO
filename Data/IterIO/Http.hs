{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Data.IterIO.Http where

import Data.Array.Unboxed
import Data.Bits
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.ByteString.Internal (w2c)
import Data.Bits
import Data.Char
import Data.Int
import Data.List
import Data.Word
import Text.Printf

import Data.IterIO
import Data.IterIO.Parse
-- import Data.IterIO.Search

import System.IO

-- import Debug.Trace

type S = L8.ByteString

pack :: String -> S
pack = L8.pack

unpack :: S -> String
unpack = L8.unpack

eord :: (Enum e) => Char -> e
eord = toEnum . ord

optional :: (ChunkData t, Monad m) => Iter t m a -> Iter t m ()
optional iter = skipI iter <|> return ()

-- | Secton 19.3 of RFC2616: "The line terminator for message-header
-- fields is the sequence CRLF.  However, we recommend that
-- applications, when parsing such headers, recognize a single LF as a
-- line terminator and ignore the leading CR."
crlf :: (Monad m) => Iter S m Word8
crlf = char '\r' *> char '\n' <|> char '\n'

spaces :: (Monad m) => Iter S m ()
spaces = skipWhile1I (\c -> c == eord ' ' || c == eord '\t')
         <?> "spaces"

lws :: (Monad m) => Iter S m S
lws = optional crlf >> L8.singleton ' ' <$ spaces <?> "linear white space"

olws :: (Monad m) => Iter S m ()
olws = lws \/ return () $ const $ return ()

noctl :: (Monad m) => Iter S m S
noctl = while1I (\c -> c >= 0x20 && c < 0x7f) <?> "non-control characters"

text :: (Monad m) => Iter S m S
text = concat1I (noctl <|> lws) <?> "text (Data.IterIO.Http)"

text_except :: (Monad m) => String -> Iter S m S
text_except except = concat1I (while1I ok <|> lws)
    where
      ok c = c >= 0x20 && c < 0x7f && not (w2c c `elem` except)

hexTab :: UArray Word8 Int8
hexTab = listArray (0,127) $ fmap digit ['\0'..'\177']
    where
      digit c | isHexDigit c = toEnum $ digitToInt c
              | otherwise    = -1

-- | Return one hex digit
hex :: (Monad m) => Iter S m Int
hex = headLikeI >>= digit <?> "hex digit"
    where
      digit c | c > 127   = expectedI "hex digit"
              | otherwise = case hexTab ! c of
                              -1 -> expectedI "hex digit"
                              n  -> return $ fromIntegral n

-- | Parse a raw hexadecimal number (no "0x..." prefix).
hexInt :: (Monad m) => Iter S m Int
hexInt = foldM1I digit 0 hex
    where
      maxok = maxBound `shiftR` 4
      digit n d | n > maxok = throwI (IterParseErr "hex integer too large")
                | otherwise = return $ (n `shiftL` 4) .|. d

-- | Percent-decode input for as long as the non percent-escaped
-- characters match some predicate.
percent_decode :: (Monad m) => (Word8 -> Bool) -> Iter S m S
percent_decode pred = foldrI L.cons' L.empty getc
    where
      getc = do
        c <- headLikeI
        case c :: Word8 of
          _ | c == eord '%' -> getval
          _ | pred c        -> return c
          _                 -> expectedI "percent_decode predicate"
      getval = do hi <- hex; lo <- hex; return $ toEnum $ 16 * hi + lo

tokenTab :: UArray Word8 Bool
tokenTab = listArray (0,127) $ fmap isTokenChar [0..127]
    where
      isTokenChar c = c > 0x20 && c < 0x7f && not (elem (chr c) separators)
      separators = "()<>@,;:\\\"/[]?={} \t\177"

token :: (Monad m) => Iter S m S
token = while1I (\c -> c < 127 && tokenTab ! c) <?> "token"

quoted_pair :: (Monad m) => Iter S m S
quoted_pair = char '\\' <:> headLikeI <:> nil

comment :: (Monad m) => Iter S m S
comment = char '('
          <:> concatI (text_except "()" <|> quoted_pair <|> comment)
          <++> string ")"
          <?> "comment"

quoted_string :: (Monad m) => Iter S m S
quoted_string = do char '"'
                   ret <- concatI (text_except "\"" <|> quoted_pair)
                   char '"'
                   return ret

inumToChunks :: (Monad m) => EnumI S S m a
inumToChunks = enumI $ iterToCodec doChunk
    where
      doChunk = do
        Chunk s eof <- chunkI
        let len       = L8.length s
            chunksize = pack $ printf "%x\r\n" len
            trailer   = if eof && len > 0
                        then pack "\r\n0\r\n\r\n"
                        else pack "\r\n"
        return $ L8.append chunksize $ L8.append s trailer

inumFromChunks :: (Monad m) => EnumI S S m a
inumFromChunks = enumI getsize
    where
      osp = skipWhileI $ \c -> c == eord ' ' || c == eord '\t'
      chunk_ext_val = do char '"'; osp; token <|> quoted_string; osp
      chunk_ext = do char ';'; osp; token; osp; optional chunk_ext_val

      getsize = do
        size <- hexInt
        osp
        skipMany chunk_ext
        crlf
        if size > 0 then getdata size else gettrailer

      getdata n = do
        s <- stringMaxI n
        let n' = n - fromIntegral (L8.length s)
        return $ CodecF (if n' > 0 then getdata n' else crlf >> getsize) s

      gettrailer = do
        skipMany (noctl >> crlf)
        skipI crlf
        return $ CodecE L8.empty

-- | Mime boundary characters
bcharTab :: UArray Word8 Bool
bcharTab = listArray (0,127) $ fmap isBChar ['\0'..'\177']
    where
      isBChar c = isAlphaNum c || elem c otherBChars
      otherBChars = "'()/+_,-./:=? "


hTTPvers :: (Monad m) => Iter S m (Int, Int)
hTTPvers = do
  string "HTTP/"
  major <- whileI (isDigit . w2c) >>= readI
  char '.'
  minor <- whileI (isDigit . w2c) >>= readI
  return (major, minor)



-- | RFC3986 syntax classes unreserved characters
rfc3986_unreserved :: Word8
rfc3986_unreserved = 0x1

rfc3986_gen_delims :: Word8
rfc3986_gen_delims = 0x2

rfc3986_sub_delims :: Word8
rfc3986_sub_delims = 0x4

rfc3986_schemechars :: Word8
rfc3986_schemechars = 0x8

rfc3986_addrchars :: Word8
rfc3986_addrchars = 0x10

rfc3986_syntax :: UArray Word8 Word8
rfc3986_syntax = listArray (0, 255) $ fmap bits ['\0'..'\377']
    where
      bits c = foldl' (.|.) 0 [
                 if isAlphaNum c || c `elem` "-._~"
                 then rfc3986_unreserved else 0
               , if c `elem` ":/?#[]@" then rfc3986_gen_delims else 0
               , if c `elem` "!$&'()*+,;=" then rfc3986_sub_delims else 0
               , if isAlphaNum c || c `elem` "+-."
                 then rfc3986_schemechars else 0
               , if isAlphaNum c || c `elem` "-._~:!$&'()*+,;="
                 then rfc3986_addrchars else 0
               ]

rfc3986_test :: Word8 -> Word8 -> Bool
rfc3986_test bit c = rfc3986_syntax ! c .&. bit /= 0

isUnreserved :: Word8 -> Bool
isUnreserved c = rfc3986_syntax ! c .&. rfc3986_unreserved /= 0

host :: (Monad m) => Iter S m S
host = (bracketed <|> percent_decode regnamechar)
       <++> (char ':' <:> whileI (isDigit . w2c) <|> nil)
    where
      regnamechar c = 0 /= rfc3986_syntax ! c
                      .&. (rfc3986_unreserved .|. rfc3986_sub_delims)
      addrchar c = 0 /= rfc3986_syntax ! c .&. rfc3986_addrchars
      bracketed = char '[' <:> percent_decode addrchar <++> char ']' <:> nil
 
{-
absUri :: Iter S m (String, String, String)
absUri = do
  scheme <- satisfy (isAlpha . w2c)
            <:> while1I $ rfc3986_test rfc3986_schemechars
  string "://"
  optional $ userinfo >> string "@"
  
    where
      userinfo = percent_decode $ rfc3986_test $
                 rfc3986_unreserved .|. rfc3986_sub_delims
-}
  

{-
uri :: Iter S m (String, String, String)
uri = absUri
      <|> abspath
      <|> authority
      <|> string "*"
    where
      absUri = do
        s <- scheme
        char ':'
      scheme = satisfy (isAlpha . w2c) <:>
               many (satisfy (\c -> isAlphaNum c || c `elem` "+-.") . w2c)
-}

data HttpReq = HttpReq {
      reqMethod :: String
    , reqURI :: String
    , reqHost :: String
    , reqVersMaj :: Int
    , reqVersMin :: Int
    } deriving (Show)



request_line :: (Monad m) => Iter S m HttpReq
request_line = do
  -- Section 4.1 of RFC2616:  "In the interest of robustness, servers
  -- SHOULD ignore any empty line(s) received where a Request-Line is
  -- expected. In other words, if the server is reading the protocol
  -- stream at the beginning of a message and receives a CRLF first,
  -- it should ignore the CRLF."
  skipMany crlf
  method <- while1I (isUpper . w2c)
  spaces
  host <- (while1I (isLower . w2c) <* string ":/") <|> return L8.empty
  uri <- char '/' <:> while1I (not . isSpace . w2c)
  spaces
  (major, minor) <- hTTPvers
  optional spaces
  skipI crlf
  return HttpReq { reqMethod = unpack $ method
                 , reqURI = unpack $ uri
                 , reqHost = ""
                 , reqVersMaj = major
                 , reqVersMin = minor
                 }

qvalue :: (Monad m) => Iter S m Int
qvalue = do char 'q'; olws; char '='; olws; frac <|> one
    where
      frac = do char '0'
                char '.' \/ return 0 $ \_ ->
                    whileMinMaxI 1 3 (isDigit . w2c) \/ return 0 $ readI
      one = do char '1'
               optional $ do char '.'
                             optional $ whileMinMaxI 0 3 (== eord '0')
               return 1000

kEqVal :: (Monad m) => Iter S m S -> Iter S m (S, S)
kEqVal kiter = do
  olws
  k <- kiter
  olws; char '='; olws
  v <- token <|> quoted_string
  return (k, v)

cookie_hdr :: (Monad m) => Iter S m [(S, S)]
cookie_hdr = do
  string "Cookie:"
  vers <- kEqVal $ string "$Version"
  sep
  sepBy1 (kEqVal token) sep <* (spaces >> crlf)
    where
      sep = do olws; char ';' <|> char ','
            

hdrLine :: (Monad m) => Iter S m S
hdrLine = lineI <++> foldrI L8.append L8.empty contLine
    where contLine = lws <++> lineI




hdr :: (Monad m) => Iter S m [S]
hdr = many hdrLine


lineChar :: (Monad m) => Iter S m Word8
lineChar = satisfy (\c -> c /= eord '\r' && c /= eord '\n')

linesI :: (Monad m) => Iter L8.ByteString m [L8.ByteString]
linesI = many1 lineChar `sepBy` crlf

put :: L8.ByteString -> IO ()
put = Prelude.putStrLn . show . L8.unpack

enumHdr :: (Monad m) => EnumO L8.ByteString m a
enumHdr = enumPure $ L8.pack $ "Header: value\r\n   cont\r\n"
          ++ "Test: a header\r\n               with extra line\r\n"
          ++ "Test2: another header\n"
          ++ "Test3: a fourth\r\n"

main :: IO ()
-- main = enumHdr |$ hdr >>= mapM_ L8.putStrLn
main = enumHandle stdin |$
       inumToChunks
        ..| inumLog "chunks.log" True 
        ..| inumFromChunks
        ..| handleI stdout
           

