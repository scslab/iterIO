{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Data.IterIO.Http (HttpReq(..)
                        , httpreqI
                        , inumToChunks, inumFromChunks
                        , comment, qvalue
                        ) where

import Data.Array.Unboxed
import Data.Bits
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Map (Map)
import qualified Data.Map as Map
import Data.ByteString.Internal (w2c, c2w)
-- import Data.Bits
import Data.Char
import Data.Int
import Data.List
import Data.Word
import Text.Printf

import Data.IterIO
import Data.IterIO.Parse
-- import Data.IterIO.Search

-- import System.IO
-- import Debug.Trace

type L = L8.ByteString

type S = S.ByteString

strictify :: L -> S
strictify = S.concat . L.toChunks

eord :: (Enum e) => Char -> e
eord = toEnum . ord

optional :: (ChunkData t, Monad m) => Iter t m a -> Iter t m ()
optional iter = skipI iter <|> return ()

--
-- Basic pieces
--

-- | Secton 19.3 of RFC2616: "The line terminator for message-header
-- fields is the sequence CRLF.  However, we recommend that
-- applications, when parsing such headers, recognize a single LF as a
-- line terminator and ignore the leading CR."
crlf :: (Monad m) => Iter L m Word8
crlf = char '\r' *> char '\n' <|> char '\n'

-- | Spaces and tabs
spaces :: (Monad m) => Iter L m ()
spaces = skipWhile1I (\c -> c == eord ' ' || c == eord '\t')
         <?> "spaces"

-- | Linear whitespace, defined as:
--
-- >  LWS            = [CRLF] 1*( SP | HT )
--
-- Parses as a single space
lws :: (Monad m) => Iter L m L
lws = optional crlf >> L8.singleton ' ' <$ spaces <?> "linear white space"

-- | @olws = 'optional' 'lws'@
olws :: (Monad m) => Iter L m ()
olws = optional lws

-- | non-control characters
noctl :: (Monad m) => Iter L m L
noctl = while1I (\c -> c >= 0x20 && c < 0x7f) <?> "non-control characters"

-- | TEXT = 1*(any OCTET except CTLs | LWS)
text :: (Monad m) => Iter L m L
text = concat1I (noctl <|> lws) <?> "text (Data.IterIO.Http)"

-- | 'text' excluding some list of except characters.
text_except :: (Monad m) => String -> Iter L m L
text_except except = concat1I (while1I ok <|> lws)
    where
      except' = fmap c2w except
      ok c = c >= 0x20 && c < 0x7f && not (c `elem` except')

-- | Parse one hex digit and return its value from 0-15.
hex :: (Monad m) => Iter L m Int
hex = headI >>= digit <?> "hex digit"
    where
      digit c | c > 127   = expectedI (show $ w2c c) "hex digit"
              | otherwise = case hexTab ! c of
                              -1 -> expectedI (show $ w2c c) "hex digit"
                              n  -> return $ fromIntegral n
      hexTab :: UArray Word8 Int8
      hexTab = listArray (0,127) $ fmap digitval ['\0'..'\177']
      digitval c | isHexDigit c = toEnum $ digitToInt c
                 | otherwise    = -1

-- | Parse a raw hexadecimal number (no \"0x...\" prefix).
hexInt :: (Monad m) => Iter L m Int
hexInt = foldM1I digit 0 hex
    where
      maxok = maxBound `shiftR` 4
      digit n d | n > maxok = throwI (IterParseErr "hex integer too large")
                | otherwise = return $ (n `shiftL` 4) .|. d

-- | 1*\<any CHAR except CTLs or separators\>
token :: (Monad m) => Iter L m S
token = strictify <$> token'

-- | Lazy 'L.ByteString' version of 'token'.
token' :: (Monad m) => Iter L m L
token' = while1I (\c -> c < 127 && tokenTab ! c) <?> "token"
    where
      tokenTab :: UArray Word8 Bool
      tokenTab = listArray (0,127) $ fmap isTokenChar [0..127]
      isTokenChar c = c > 0x20 && c < 0x7f && not (elem (chr c) separators)
      separators = "()<>@,;:\\\"/[]?={} \t\177"

-- | Percent-decode input for as long as the non percent-escaped
-- characters match some predicate.
percent_decode :: (Monad m) => (Word8 -> Bool) -> Iter L m L
percent_decode test = foldrI L.cons' L.empty getc
    where
      getc = do
        c <- headI
        case c :: Word8 of
          _ | c == eord '%' -> getval
          _ | test c        -> return c
          _                 -> expectedI (show c) "percent_decode predicate"
      getval = do hi <- hex; lo <- hex; return $ toEnum $ 16 * hi + lo

-- | Parse a backslash-escaped character.
quoted_pair :: (Monad m) => Iter L m L
quoted_pair = char '\\' <:> headI <:> nil

-- | 'text' and 'quoted_pair's surrounded by double quotes.
quoted_string :: (Monad m) => Iter L m S
quoted_string = do char '"'
                   ret <- concatI (text_except "\"" <|> quoted_pair)
                   char '"'
                   return $ strictify ret

-- | 'text' and 'quoted_pair's surrounded by parentheses.
comment :: (Monad m) => Iter L m L
comment = char '('
          <:> concatI (text_except "()" <|> quoted_pair <|> comment)
          <++> string ")"
          <?> "comment"

-- | Parses q=N where 0.000 <= N <= 1.000, and returns the result
-- multiplied by 1000 as an integer (i.e., 1.0 returns 1000).
qvalue :: (Monad m) => Iter L m Int
qvalue = do char 'q'; olws; char '='; olws; frac <|> one
    where
      frac = do char '0'
                char '.' \/ return 0 $ \_ ->
                    whileMinMaxI 1 3 (isDigit . w2c) \/ return 0 $ readI
      one = do char '1'
               optional $ do char '.'
                             optional $ whileMinMaxI 0 3 (== eord '0')
               return 1000

parameter :: (Monad m) => Iter L m (S, S)
parameter = do
  olws
  k <- token
  olws; char '='; olws
  v <- token <|> quoted_string
  return (k, v)


--
-- URI parsing
--

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

rfc3986_pcharslash :: Word8
rfc3986_pcharslash = 0x20

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
               , if isAlphaNum c || c `elem` "-._~!$&'()*+,;=:@/"
                 then rfc3986_pcharslash else 0
               ]

rfc3986_test :: Word8 -> Word8 -> Bool
rfc3986_test mask c = rfc3986_syntax ! c .&. mask /= 0

{-
isUnreserved :: Word8 -> Bool
isUnreserved c = rfc3986_syntax ! c .&. rfc3986_unreserved /= 0
-}

hostI :: (Monad m) => Iter L m (S, Maybe Int)
hostI = (,) <$> host <*> (Just <$> port <|> return Nothing) <?> "host"
    where
      host = strictify <$> (bracketed <|> percent_decode regnamechar)
      port = do _ <- char ':'; whileI (isDigit . w2c) >>= readI
      regnamechar c = (rfc3986_syntax ! c
                       .&. (rfc3986_unreserved .|. rfc3986_sub_delims)) /= 0
      addrchar c = 0 /= rfc3986_syntax ! c .&. rfc3986_addrchars
      bracketed = char '[' <:> percent_decode addrchar <++> char ']' <:> nil

pathI :: (Monad m) => Iter L m (S, S)
pathI = dopath <?> "path"
    where
      dopath = do
        path <- strictify <$>
                (ensureI (== eord '/')
                 *> percent_decode (rfc3986_test rfc3986_pcharslash))
                <|> return (S8.pack "/")
        query <- char '?' *> (strictify <$> whileI qpcharslash) <|> nil
        return (path, query)
      qpcharslash c = rfc3986_test rfc3986_pcharslash c || c == eord '?'
 
-- | Returns (scheme, host, path, query)
absUri :: (Monad m) => Iter L m (S, S, Maybe Int, S, S)
absUri = do
  scheme <- strictify <$> satisfy (isAlpha . w2c)
            <:> while1I (rfc3986_test rfc3986_schemechars)
  string "://"
  optional $ userinfo >> string "@"
  authority <- hostI
  (path, query) <- pathI
  return (scheme, fst authority, snd authority, path, query)
    where
      userinfo = percent_decode $ \c ->
                 rfc3986_test (rfc3986_unreserved .|. rfc3986_sub_delims) c
                 || c == eord ':'
  
-- | Returns (scheme, host, path, query)
uri :: (Monad m) => Iter L m (S, S, Maybe Int, S, S)
uri = absUri
      <|> path
      <|> char '*' *> return (S.empty, S.empty, Nothing, S8.pack "*", S.empty)
      <?> "URI"
    where
      path = do (p, q) <- ensureI (== eord '/') *> pathI
                return (S.empty, S.empty, Nothing, p, q)


--
-- HTTP request and header parsing
--

data HttpReq = HttpReq {
      reqMethod :: S
    , reqPath :: S
    , reqQuery :: S
    , reqHost :: S
    , reqPort :: Maybe Int
    , reqVers :: (Int, Int)
    , reqHeaders :: [(S, S)]
    , reqCookies :: [(S, S)]
    , reqContentType :: Maybe (S, [(S,S)])
    } deriving (Show)

defaultHttpReq :: HttpReq
defaultHttpReq = HttpReq { reqMethod = S.empty
                         , reqPath = S.empty
                         , reqQuery = S.empty
                         , reqHost = S.empty
                         , reqPort = Nothing
                         , reqVers = (0, 0)
                         , reqHeaders = []
                         , reqCookies = []
                         , reqContentType = Nothing
                         }

hTTPvers :: (Monad m) => Iter L m (Int, Int)
hTTPvers = do
  string "HTTP/"
  major <- whileI (isDigit . w2c) >>= readI
  char '.'
  minor <- whileI (isDigit . w2c) >>= readI
  return (major, minor)

-- | HTTP request line, defined by RFC2616 as:
--
-- > Request-Line   = Method SP Request-URI SP HTTP-Version CRLF
request_line :: (Monad m) => Iter L m HttpReq
request_line = do
  method <- strictify <$> while1I (isUpper . w2c)
  spaces
  (_, host, mport, path, query) <- uri
  spaces
  (major, minor) <- hTTPvers
  optional spaces
  skipI crlf
  return defaultHttpReq {
                 reqMethod = method
               , reqPath = path
               , reqQuery = query
               , reqHost = host
               , reqPort = mport
               , reqVers = (major, minor)
               }

request_headers :: (Monad m) => Map S (HttpReq -> Iter L m HttpReq)
request_headers = Map.fromList $
                  map (\(a, b) -> (S8.map toLower $ S8.pack a, b)) $
    [
      ("Host", host_hdr)
    , ("Cookie", cookie_hdr)
    , ("Content-Type", content_type_hdr)
    ]

host_hdr :: (Monad m) => HttpReq -> Iter L m HttpReq
host_hdr req = do
  (host, mport) <- hostI
  return req { reqHost = host, reqPort = mport }

cookie_hdr :: (Monad m) => HttpReq -> Iter L m HttpReq
cookie_hdr req = do
  cookies <- sepBy1 parameter sep
  return req { reqCookies = cookies }
    where
      sep = do olws; char ';' <|> char ','

{-
peekaboo :: (Monad m) => String -> Iter L m ()
peekaboo str = IterF $ \c@(Chunk t _) ->
               trace (str ++ ": " ++ L8.unpack t) $ Done () c
-}

content_type_hdr :: (Monad m) => HttpReq -> Iter L m HttpReq
content_type_hdr req = do
  typ <- token <++> char '/' <:> token
  parms <- many $ olws >> char ';' >> parameter
  return req { reqContentType = Just (typ, parms) }

any_hdr :: (Monad m) => HttpReq -> Iter L m HttpReq
any_hdr req = do
  field <- S8.map toLower <$> token
  char ':'
  olws
  val <- strictify <$> text
  crlf
  let req' = req { reqHeaders = (field, val) : reqHeaders req }
  case Map.lookup field request_headers of
    Nothing -> return req'
    Just f  -> enumPure (L.fromChunks [val]) .|$
               (f req' <* (optional spaces >> eofI)
                      <?> (S8.unpack field ++ " header"))

httpreqI :: Monad m => Iter L m HttpReq
httpreqI = do
  -- Section 4.1 of RFC2616:  "In the interest of robustness, servers
  -- SHOULD ignore any empty line(s) received where a Request-Line is
  -- expected. In other words, if the server is reading the protocol
  -- stream at the beginning of a message and receives a CRLF first,
  -- it should ignore the CRLF."
  skipMany crlf
  request_line >>= next_hdr
    where
      next_hdr req = seq req $ any_hdr req \/ return req $ next_hdr


--
-- Cunk encoding and decoding
--

-- | HTTP Chunk encoder
inumToChunks :: (Monad m) => EnumI L L m a
inumToChunks = enumI $ iterToCodec doChunk
    where
      doChunk = do
        Chunk s eof <- chunkI
        let len       = L8.length s
            chunksize = L8.pack $ printf "%x\r\n" len
            trailer   = if eof && len > 0
                        then L8.pack "\r\n0\r\n\r\n"
                        else L8.pack "\r\n"
        return $ L8.append chunksize $ L8.append s trailer

-- | HTTP Chunk decoder
inumFromChunks :: (Monad m) => EnumI L L m a
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

{-
postReq :: L
postReq = L8.pack
 "POST /testSubmit HTTP/1.1\n\
 \Host: localhost:8000\n\
 \User-Agent: Mozilla/5.0 (X11; U; Linux i686 (x86_64); en-US; rv:1.9.2.8) Gecko/20100722 Firefox/3.6.8\n\
 \Accept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8\n\
 \Accept-Language: en-us,en;q=0.5\n\
 \Accept-Encoding: gzip,deflate\n\
 \Accept-Charset: ISO-8859-1,utf-8;q=0.7,*;q=0.7\n\
 \Keep-Alive: 115\n\
 \Connection: keep-alive\n\
 \Content-Type: multipart/form-data; boundary=---------------------------28986267117678495841915281966\n\
 \Content-Length: 561\n\
 \\n\
 \-----------------------------28986267117678495841915281966\n\
 \Content-Disposition: form-data; name=\"justatestkey\"\n\
 \\n\
 \nothing\n\
 \-----------------------------28986267117678495841915281966\n\
 \Content-Disposition: form-data; name=\"hate\"\n\
 \\n\
 \666\n\
 \-----------------------------28986267117678495841915281966\n\
 \Content-Disposition: form-data; name=\"file1\"; filename=\"x\"\n\
 \Content-Type: application/octet-stream\n\
 \\n\
 \search scs.stanford.edu uun.org\n\
 \nameserver 127.0.0.1\n\
 \nameserver 64.81.79.2\n\
 \nameserver 216.231.41.2\n\
 \\n\
 \-----------------------------28986267117678495841915281966--\n"


-- | Mime boundary characters
bcharTab :: UArray Word8 Bool
bcharTab = listArray (0,127) $ fmap isBChar ['\0'..'\177']
    where
      isBChar c = isAlphaNum c || elem c otherBChars
      otherBChars = "'()/+_,-./:=? "
-}




{-

lineChar :: (Monad m) => Iter L m Word8
lineChar = satisfy (\c -> c /= eord '\r' && c /= eord '\n')

linesI :: (Monad m) => Iter L8.ByteString m [L8.ByteString]
linesI = many1 lineChar `sepBy` crlf

put :: L8.ByteString -> IO ()
put = Prelude.putStrLn . show . L8.unpack

enumHdr :: (Monad m) => EnumO L8.ByteString m a
enumHdr = enumPure $ L.append header
          $ L.cycle $ L8.pack "And some extra crap\r\n"
    where
      header = L8.pack $ "\
        \GET /blah/blah/blah?really HTTP/1.1\r\n\
        \Host: localhost:8000\r\n\
        \User-Agent: Mozilla/5.0 (X11; U; Linux x86_64; en_US; rv:1.9.2.8) Gecko/20100813 Gentoo Firefox/3.6.8\r\n\
        \Accept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8\r\n\
        \Accept-Language: en-us,en;q=0.5\r\n\
        \Accept-Encoding: gzip,deflate\r\n\
        \Accept-Charset: ISO-8859-1,utf-8;q=0.7,*;q=0.7\r\n\
        \Keep-Alive: 115\r\n\
        \Connection: keep-alive\r\n\
        \Cache-Control: max-age=0\r\n\r\n"

          




main :: IO ()
-- main = enumHdr |$ hdr >>= mapM_ L8.putStrLn
main = enumHandle stdin |$
       inumToChunks
        ..| inumLog "chunks.log" True 
        ..| inumFromChunks
        ..| handleI stdout
           
-}
