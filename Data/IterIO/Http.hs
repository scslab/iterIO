{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Data.IterIO.Http (-- * HTTP Request support
                         HttpReq(..), reqNormalPath
                        , httpReqI, inumHttpBody
                        , inumToChunks, inumFromChunks
                        , http_fmt_time, dateI
                        , FormField(..), foldForm
                        -- , urlencodedFormI, multipartI, inumMultipart
                        -- , foldUrlencoded, foldMultipart, foldQuery
                        -- * HTTP Response support
                        , HttpStatus(..)
                        , stat100, stat200, stat301, stat302, stat303, stat304
                        , stat400, stat401, stat403, stat404, stat405
                        , stat500, stat501
                        , HttpResp(..), defaultHttpResp
                        , mkHttpHead, mkHtmlResp, mkContentLenResp, mkOnumResp
                        , resp301, resp303, resp404, resp405, resp500
                        , enumHttpResp
                        -- * HTTP connection handling
                        , HttpRequestHandler
                        , HttpServerConf(..), nullHttpServer, ioHttpServer
                        , inumHttpServer
                        -- -- * For debugging
                        -- , postReq, encReq, mptest, mptest'
                        -- , formTestMultipart, formTestUrlencoded
                        ) where

import Control.Exception (SomeException(..))
import Control.Monad
import Control.Monad.Identity
import Control.Monad.Trans
import Data.Array.Unboxed
import Data.Bits
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.ByteString.Internal (w2c, c2w)
-- import Data.Bits
import Data.Char
import Data.Int
import Data.List
import Data.Time
import Data.Typeable
import Data.Word
import System.Locale
import System.IO
import Text.Printf

import Data.IterIO
import Data.IterIO.Parse
import Data.IterIO.Search

-- import System.IO

type L = L8.ByteString
type S = S.ByteString

strictify :: L -> S
strictify = S.concat . L.toChunks

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
lws = optionalI crlf >> L8.singleton ' ' <$ spaces <?> "linear white space"

-- | @olws = 'optionalI' 'lws'@
olws :: (Monad m) => Iter L m ()
olws = optionalI lws

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
      digit n d | n > maxok = throwI (IterMiscParseErr "hex integer too large")
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
        case c of
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

{-
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
                    whileMinMaxI 0 3 (isDigit . w2c) \/ return 0 $ readI
      one = do char '1'
               optionalI $ do char '.'
                             optionalI $ whileMinMaxI 0 3 (== eord '0')
               return 1000
-}

parameter :: (Monad m) => Iter L m (S, S)
parameter = do
  olws
  k <- token
  olws; char '='; olws
  v <- token <|> quoted_string
  return (k, v)

--
-- Date/time
--

-- | Formats a time in the format specified by RFC 2616.
http_fmt_time :: UTCTime -> String
http_fmt_time = formatTime defaultTimeLocale "%a, %_d %b %Y %H:%M:%S GMT"

dowmap :: Map L Int
dowmap = Map.fromList $ flip zip ([0..6] ++ [0..6]) $
         map L8.pack ["Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"
                     , "Sunday", "Monday", "Tuesday", "Wednesday"
                     , "Thursday", "Friday", "Saturday", "Sunday"]

weekdayI :: (Monad m) => Iter L.ByteString m Int
weekdayI = mapI dowmap <?> "Day of Week"

monmap :: Map L Int
monmap = Map.fromList $ flip zip [1..12] $
         map L8.pack ["Jan", "Feb", "Mar", "Apr", "May", "Jun"
                     , "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"]

monthI :: (Monad m) => Iter L.ByteString m Int
monthI = mapI monmap <?> "Month"

timeI :: (Monad m) => Iter L.ByteString m TimeOfDay
timeI = do
  hours <- whileMinMaxI 2 2 (isDigit . w2c) >>= readI <?> "Hours"
  char ':'
  minutes <- whileMinMaxI 2 2 (isDigit . w2c) >>= readI <?> "Minutes"
  char ':'
  seconds <- whileMinMaxI 2 2 (isDigit . w2c) >>= readI <?> "Seconds"
  when (hours >= 24 || minutes >= 60 || seconds > 62) $
       throwI $ IterMiscParseErr "timeI: Invalid hours/minutes/seconds"
  return $ TimeOfDay hours minutes (fromIntegral (seconds :: Int))

rfc822_time :: (Monad m) => Iter L m UTCTime
rfc822_time = do
  weekdayI
  char ','
  spaces
  mday <- whileMinMaxI 2 2 (isDigit . w2c) >>= readI <?> "Day of Month"
  spaces
  month <- monthI
  spaces
  year <- whileMinMaxI 4 5 (isDigit . w2c) >>= readI <?> "Year"
  spaces
  tod <- timeI
  spaces
  string "GMT"
  return $ localTimeToUTC utc LocalTime {
                localDay = fromGregorian year month mday
              , localTimeOfDay = tod
              }

rfc850_time :: (Monad m) => Iter L m UTCTime
rfc850_time = do
  weekdayI
  char ','
  spaces
  mday <- whileMinMaxI 2 2 (isDigit . w2c) >>= readI <?> "Day of Month"
  char '-'
  month <- monthI
  char '-'
  year <- do y2 <- whileMinMaxI 2 2 (isDigit . w2c) >>= readI <?> "Year"
             return $ if y2 < 70 then y2 + 2000 else y2 + 1900
  spaces
  tod <- timeI
  spaces
  string "GMT"
  return $ localTimeToUTC utc LocalTime {
                localDay = fromGregorian year month mday
              , localTimeOfDay = tod
              }

asctime_time :: (Monad m) => Iter L m UTCTime
asctime_time = do
  weekdayI
  spaces
  month <- monthI
  spaces
  mday <- whileMinMaxI 1 2 (isDigit . w2c) >>= readI <?> "Day of Month"
  spaces
  tod <- timeI
  spaces
  year <- whileMinMaxI 4 5 (isDigit . w2c) >>= readI <?> "Year"
  return $ localTimeToUTC utc LocalTime {
                localDay = fromGregorian year month mday
              , localTimeOfDay = tod
              }

-- | Parses a Date/Time string in any one of the three formats
-- specified by RFC 2616.
dateI :: (Monad m) => Iter L.ByteString m UTCTime
dateI = rfc822_time <|> rfc850_time <|> asctime_time <?> "HTTP date/time"

--
-- URI parsing (RFC 3986)
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
      host = S8.map toLower <$> strictify <$>
             (bracketed <|> percent_decode regnamechar)
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
  optionalI $ userinfo >> string "@"
  authority <- hostI
  (path, query) <- pathI
  return (scheme, fst authority, snd authority, path, query)
    where
      userinfo = percent_decode $ \c ->
                 rfc3986_test (rfc3986_unreserved .|. rfc3986_sub_delims) c
                 || c == eord ':'
  
-- | Returns (scheme, host, path, query).
uri :: (Monad m) => Iter L m (S, S, Maybe Int, S, S)
uri = absUri
      <|> path
      <|> char '*' *> return (S.empty, S.empty, Nothing, S8.pack "*", S.empty)
      <?> "URI"
    where
      path = do (p, q) <- ensureI (== eord '/') *> pathI
                return (S.empty, S.empty, Nothing, p, q)

-- | Turn a path into a list of components
path2list :: S -> [S]
path2list path = runIdentity $ inumPure path |$
                 slash [] `catchI` \(IterNoParse _) _ -> return []
    where
      slash acc = while1I (eord '/' ==) \/ eofI *> return (reverse acc) $
                  const $ comp acc
      comp acc  = do n <- while1I (eord '/' /=)
                     case () of
                       () | n == S8.pack "." -> slash acc
                       () | n == S8.pack ".." ->
                              if null acc then slash [] else slash $ tail acc
                       () -> slash $ n:acc

--
-- HTTP request and header parsing
--

-- | Data structure representing an HTTP request message.
data HttpReq = HttpReq {
      reqMethod :: !S.ByteString
    -- ^ Method (e.g., GET, POST, ...).
    , reqPath :: !S.ByteString
    -- ^ Raw path from the URL (not needed if you use @reqPathList@
    -- and @reqPathParams@).
    , reqPathLst :: ![S.ByteString]
    -- ^ URL request path, broken into a list of directory components,
    -- and normalized to remove @\".\"@ and process @\"..\"@.
    , reqPathParams :: ![S.ByteString]
    -- ^ Used by 'routeVar' to save pathname components that are
    -- variables (used as a stack, so the last variable saved is the
    -- first one in the list).
    , reqPathCtx :: ![S.ByteString]
    -- ^ Stores pathname components that have been stripped off of
    -- @reqPathLst@ during routing.
    , reqQuery :: !S.ByteString
    -- ^ The portion of the URL after the @?@ character (if any).
    , reqHost :: !S.ByteString
    -- ^ Lower-case host header (or the host from the request line, if
    -- the request is for an absolute URI).
    , reqPort :: !(Maybe Int)
    -- ^ Port number if supplied in Host header.
    , reqVers :: !(Int, Int)
    -- ^ HTTP version major and minor number from the request line.
    , reqHeaders :: ![(S.ByteString, S.ByteString)]
    -- ^ List of all header field names and values in the HTTP
    -- request.  Field names are converted to lowercase to allow
    -- easier searching.
    , reqCookies :: ![(S.ByteString, S.ByteString)]
    -- ^ List of Cookies supplied in the request.
    , reqContentType :: !(Maybe (S.ByteString, [(S.ByteString,S.ByteString)]))
    -- ^ Parsed version of the Content-Type header, if any.  The first
    -- 'S.ByteString' is the actual content type.  Following this is a
    -- list of parameter names and values.  The most useful parameter
    -- is @\"boundary\"@, used with the @multipart/form-data@ content
    -- type.
    , reqContentLength :: !(Maybe Int)
    -- ^ Value of the content-Length header, if any.
    , reqTransferEncoding :: ![S.ByteString]
    -- ^ A list of the encodings in the Transfer-Encoding header.
    , reqIfModifiedSince :: !(Maybe UTCTime)
    } deriving (Typeable, Show)

defaultHttpReq :: HttpReq
defaultHttpReq = HttpReq { reqMethod = S.empty
                         , reqPath = S.empty
                         , reqPathLst = []
                         , reqPathParams = []
                         , reqPathCtx = []
                         , reqQuery = S.empty
                         , reqHost = S.empty
                         , reqPort = Nothing
                         , reqVers = (0, 0)
                         , reqHeaders = []
                         , reqCookies = []
                         , reqContentType = Nothing
                         , reqContentLength = Nothing
                         , reqTransferEncoding = []
                         , reqIfModifiedSince = Nothing
                         }

-- | Returns a normalized version of the path in the URL (e.g., where
-- @\".\"@ has been eliminated, @\"..\"@ has been processed, there is
-- exactly one @\'/\'@ between each directory component, and the query
-- has been stripped off).
reqNormalPath :: HttpReq -> S.ByteString
reqNormalPath rq =
    S.intercalate slash $ S.empty : reqPathCtx rq ++ reqPathLst rq
    where slash = S8.singleton '/'

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
  optionalI spaces
  skipI crlf
  return defaultHttpReq {
                 reqMethod = method
               , reqPath = path
               , reqPathLst = path2list path
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
    , ("Content-Length", content_length_hdr)
    , ("Transfer-Encoding", transfer_encoding_hdr)
    , ("If-Modified-Since", if_modified_since_hdr)
    ]

host_hdr :: (Monad m) => HttpReq -> Iter L m HttpReq
host_hdr req = do
  (host, mport) <- hostI
  return req { reqHost = host, reqPort = mport }

cookie_hdr :: (Monad m) => HttpReq -> Iter L m HttpReq
cookie_hdr req = ifParse cookiesI setCookies ignore
  where
    cookiesI = sepBy1 parameter sep <* eofI
    sep = do olws; char ';' <|> char ','
    setCookies cookies = return $ req { reqCookies = cookies }
    ignore = nullI >> return req

content_type_hdr :: (Monad m) => HttpReq -> Iter L m HttpReq
content_type_hdr req = do
  typ <- token <++> char '/' <:> token
  parms <- many $ olws >> char ';' >> parameter
  return req { reqContentType = Just (typ, parms) }

content_length_hdr :: (Monad m) => HttpReq -> Iter L m HttpReq
content_length_hdr req = do
  len <- olws >> (while1I (isDigit . w2c) >>= readI) <* olws
  return req { reqContentLength = Just len }

transfer_encoding_hdr :: (Monad m) => HttpReq -> Iter L m HttpReq
transfer_encoding_hdr req = do
  tclist <- many tc
  return req { reqTransferEncoding = tclist }
  where
    tc = do
      olws
      coding <- S8.map toLower <$> token
      skipMany $ olws >> char ';' >> parameter
      return coding

if_modified_since_hdr :: (Monad m) => HttpReq -> Iter L m HttpReq
if_modified_since_hdr req = do
  modtime <- dateI
  return req { reqIfModifiedSince = Just modtime }

hdr_field_val :: (Monad m) => Iter L m (S, S)
hdr_field_val = do
  field <- S8.map toLower <$> token
  char ':'
  olws
  val <- strictify <$> text
  crlf
  return (field, val)

any_hdr :: (Monad m) => HttpReq -> Iter L m HttpReq
any_hdr req = do
  (field, val) <- hdr_field_val
  let req' = req { reqHeaders = (field, val) : reqHeaders req }
  case Map.lookup field request_headers of
    Nothing -> return req'
    Just f  -> do
      r <- inumPure (L.fromChunks [val]) .|$
               (f req' <* (optionalI spaces >> eofI)
                      <?> (S8.unpack field ++ " header"))
      return r

-- | Parse an HTTP header, returning an 'HttpReq' data structure.
httpReqI :: Monad m => Iter L.ByteString m HttpReq
httpReqI = do
  -- Section 4.1 of RFC2616:  "In the interest of robustness, servers
  -- SHOULD ignore any empty line(s) received where a Request-Line is
  -- expected. In other words, if the server is reading the protocol
  -- stream at the beginning of a message and receives a CRLF first,
  -- it should ignore the CRLF."
  skipMany crlf
  (request_line >>= next_hdr) <* crlf
    where next_hdr req = seq req $ any_hdr req \/ return req $ next_hdr


--
-- Chunk encoding and decoding (RFC 2616)
--

-- | An HTTP Chunk encoder (as specified by RFC 2616).
inumToChunks :: (Monad m) => Inum L.ByteString L.ByteString m a
inumToChunks = mkInumM loop
    where
      loop = do
        Chunk s eof <- chunkI
        let len       = L8.length s
            chunksize = L8.pack $ printf "%x\r\n" len
            trailer   = if eof && len > 0
                        then L8.pack "\r\n0\r\n\r\n"
                        else L8.pack "\r\n"
        ifeed $ L8.concat [chunksize, s, trailer]
        unless eof loop

-- | An HTTP Chunk decoder (as specified by RFC 2616).
inumFromChunks :: (Monad m) => Inum L.ByteString L.ByteString m a
inumFromChunks = mkInumM $ getchunk
    where
      osp = skipWhileI $ \c -> c == eord ' ' || c == eord '\t'
      chunk_ext_val = do char '='; osp; token <|> quoted_string; osp
      chunk_ext = do char ';'; osp; token; osp; optionalI chunk_ext_val
      getchunk = do
        size <- hexInt <* (osp >> skipMany chunk_ext >> crlf)
        if size > 0 then ipipe (inumTakeExact size) >> getchunk
                    else do
                      skipMany (noctl >> crlf)
                      skipI crlf

-- | This 'Inum' reads to the end of an HTTP message body (and not
-- beyond) and decodes the Transfer-Encoding.  It handles straight
-- content of a size specified by the Content-Length header and
-- chunk-encoded content.
inumHttpBody :: (Monad m) => HttpReq -> Inum L.ByteString L.ByteString m a
inumHttpBody req =
    case reqTransferEncoding req of
      lst | null lst || lst == [S8.pack "identity"] ->
              if hasclen then inumTakeExact (fromJust $ reqContentLength req)
                         else inumNull -- No message body present
      lst | lst == [S8.pack "chunked"] -> inumFromChunks
      lst -> inumFromChunks |. tcfold (reverse lst)
    where
      hasclen = isJust $ reqContentLength req
      tcfold [] = inumNop
      tcfold (h:t) 
          | h == S8.pack "identity" = tcfold t
          | h == S8.pack "chunked"  = tcfold t -- Has to be first one
          --- | h == S8.pack "gzip"     = inumGunzip |. tcfold t
          | otherwise = mkInum $
                        fail $ "unknown Transfer-Coding " ++ chunkShow h

{-
-- | This 'Inum' reads to the end of an HTTP message body (and not
-- beyond) and decodes the Transfer-Encoding.  It handles straight
-- content of a size specified by the Content-Length header,
-- chunk-encoded content, and content that has been gzipped then
-- chunk-encoded.
inumHttpBodyZ :: (MonadIO m) => HttpReq -> Inum L.ByteString L.ByteString m a
inumHttpBodyZ req =
    case reqTransferEncoding req of
      lst | null lst || lst == [S8.pack "identity"] ->
              if hasclen then inumTakeExact (fromJust $ reqContentLength req)
                         else return -- No message body present
      lst | lst == [S8.pack "chunked"] -> inumFromChunks
      lst -> inumFromChunks |. tcfold (reverse lst)
    where
      hasclen = isJust $ reqContentLength req
      tcfold [] = inumNop
      tcfold (h:t) 
          | h == S8.pack "identity" = tcfold t
          | h == S8.pack "chunked"  = tcfold t -- Has to be first one
          | h == S8.pack "gzip"     = inumGunzip |. tcfold t
          | otherwise = mkInum $ fail $ "unknown Transfer-Coding "
                        ++ chunkShow h
-}

--
-- Support for decoding form data
--

-- | Data structure representing the name and metadata of a control in
-- a submitted form.
data FormField = FormField {
      ffName :: !S.ByteString
    -- ^ Name of the form control being processed
    , ffParams :: ![(S.ByteString, S.ByteString)]
    -- ^ Parameters from the @Content-Disposition:@ header.  This only
    -- applies to @Content-Type: multipart/form-data@, and will be
    -- empty for forms of type application/x-www-form-urlencoded or
    -- forms submitted in the URL parameters of a GET request.
    , ffHeaders :: ![(S.ByteString, S.ByteString)]
    -- ^ Extra headers following the @Content-Disposition:@ header of
    -- a @multipart/form-data@ post.  Empty for other kinds of form
    -- submission.
    } deriving (Show)

defaultFormField :: FormField
defaultFormField = FormField {
                     ffName = S.empty
                   , ffParams = []
                   , ffHeaders = []
                   }

-- | Parses a form, and folds a function over each control.  The value
-- of each control is available through Iteratee input.  Thus, you can
-- extract the submitted value with 'pureI', or redirect it elsewhere
-- by executing another 'Iter'.  For example, to parse a form and
-- print it to standard output (without buffering possibly large file
-- uploads in memory):
--
-- >  do let docontrol _ field = do
-- >           liftIO $ putStrLn $
-- >               "The value of " ++ (S8.unpack $ ffName field) ++ " is:"
-- >           stdoutI                   -- Send form value to standard output
-- >           liftIO $ putStrLn "\n"
-- >     foldForm req docontrol ()
--
-- Or to produce a list of (field, value) pairs, you can say something
-- like:
-- 
-- >  do let docontrol acc field = do
-- >           val <- pureI
-- >           return $ (ffName field, val) : acc
-- >     foldForm req docontrol []
--
-- Note that for POSTed forms of enctype
-- @application/x-www-form-urlencoded@, @foldForm@ will read to the
-- end of its input.  Thus, it is important to ensure @foldForm@ is
-- called from within an 'inumHttpBody' enumerator (which is
-- guaranteed by 'inumHttpServer').
foldForm :: (Monad m) =>
            HttpReq
         -> (a -> FormField -> Iter L.ByteString m a)
         -> a
         -> Iter L.ByteString m a
foldForm req = case reqContentType req of
                 Nothing -> foldQuery req
                 Just (mt, _) | mt == urlencoded -> foldUrlencoded req
                 Just (mt, _) | mt == multipart  -> foldMultipart req
                 _ -> \_ _ -> throwI $ IterMiscParseErr $
                      "foldForm: invalid Content-Type"


--
-- application/x-www-form-urlencoded decoding
--
-- The HTML 4.01 spec says:
--
--   This is the default content type. Forms submitted with this
--   content type must be encoded as follows:
--       
--    1. Control names and values are escaped. Space characters are
--       replaced by `+', and then reserved characters are escaped as
--       described in [RFC1738], section 2.2: Non-alphanumeric characters
--       are replaced by `%HH', a percent sign and two hexadecimal digits
--       representing the ASCII code of the character. Line breaks are
--       represented as "CR LF" pairs (i.e., `%0D%0A').
-- 
--    2. The control names/values are listed in the order they appear in
--       the document. The name is separated from the value by `=' and
--       name/value pairs are separated from each other by `&'.
--
-- RFC 1738 says:
--   ...only alphanumerics, the special characters "$-_.+!*'(),", and
--   reserved characters used for their reserved purposes may be used
--   unencoded within a URL.
--
-- On the other hand, RFC 3986 says the following are reserved:
--   :/?#[]@!$&'()*+,;=
--
-- And that the only unreserved characters are:
--   unreserved  = ALPHA / DIGIT / "-" / "." / "_" / "~"
--
-- In practice, browsers seem to encode everything (including "~"),
-- except for ALPHA, DIGIT, and the four characters:
--   -._*
--
-- Given the confusion, we'll just accept almost everything except '&'
-- and '='.

urlencoded :: S
urlencoded = S8.pack "application/x-www-form-urlencoded"

urlencTab :: UArray Word8 Bool
urlencTab = listArray (0, 127) $ fmap ok ['\0'..'\177']
    where ok c | c <= ' '        = False
               | c >= '\177'     = False
               | c `elem` "%+&=" = False
               | otherwise       = True

controlI :: (Monad m) => Iter L m (S, S)
controlI = flip (<?>) "form control NAME=VALUE" $ do
  name <- encval
  value <- (char '=' >> encval) <|> nil
  return (name, value)
    where
      encval = liftM strictify $ concatI $
               someI (percent_decode (urlencTab !))
               <|> L8.singleton ' ' <$ char '+'

{-
urlencodedFormI :: (Monad m) => Iter L m [(S,S)]
urlencodedFormI = sepBy controlI (char '&')
-}

inumBind :: (ChunkData t, Monad m) =>
            Iter t m a -> (a -> Iter t m a) -> Iter t m a
inumBind m k = tryI' m >>= either reRunIter k
infixl 1 `inumBind`

foldControls :: (Monad m) => (a -> FormField -> Iter L m a) -> a -> Iter L m a
foldControls f z =
    controlI \/ return z $ \(k, v) ->
    inumPure (L.fromChunks [v]) .|
             f z defaultFormField { ffName = k } `inumBind` \a ->
    char '&' \/ return a $ \_ -> foldControls f a

foldUrlencoded :: (Monad m) =>
                  HttpReq -> (a -> FormField -> Iter L m a) -> a -> Iter L m a
foldUrlencoded _req f z = foldControls f z
{-
    case reqContentLength req of
      Just len -> inumTakeExact len .| foldControls f z
      Nothing  -> throwI $ IterMiscParseErr $
                  "foldUrlencoded: Missing Content-legth"
-}

foldQuery :: (Monad m) =>
             HttpReq -> (a -> FormField -> Iter L m a) -> a -> Iter L m a
foldQuery req f z = inumPure (L.fromChunks [reqQuery req]) .| foldControls f z

--
-- multipart/form-data decoding, as specified throughout the following:
--
-- RFC 2045 - MIME part 1, including Content-Type header grammar
-- RFC 2046 - MIME part 2, including multipart boundary grammar
-- RFC 2047 - (splitting up parameters - not implemented yet here)
-- RFC 2183 - The Content-Disposition header grammar
-- 
-- Less useful, but normative:
--
-- RFC 2388 - multipart/form data spec (mostly references above)
--

{-
-- | Mime boundary characters
bcharTab :: UArray Word8 Bool
bcharTab = listArray (0,127) $ fmap isBChar ['\0'..'\177']
    where isBChar c = isAlphaNum c || elem c otherBChars
          otherBChars = "'()/+_,-./:=? "
-}

multipart :: S
multipart = S8.pack "multipart/form-data"

reqBoundary :: HttpReq -> Maybe S
reqBoundary req = case reqContentType req of
                    Just (typ, parms) | typ == multipart ->
                                          lookup (S8.pack "boundary") parms
                    _ -> Nothing

multipartI :: (Monad m) => HttpReq -> Iter L m (Maybe (FormField))
multipartI req = case reqBoundary req of
                   Just b  -> findpart $ S8.pack "--" `S8.append` b
                   Nothing -> return Nothing
  where
    nextLine :: (Monad m) => Iter L m ()
    nextLine = skipWhileI (\c -> c `elem` map eord " \t\r") >>
               char '\n' >> return ()
    findpart b = do
      match $ L.fromChunks [b]
      done <- ((string "--" >> return True) <|> return False) <* nextLine
      if done then return Nothing else Just <$> parsepart
    parsepart = do
      cdhdr@(field, val) <- hdr_field_val
      inumPure field .|$ stringCase "Content-Disposition"
      parms <- inumPure (L.fromChunks [val]) .|$
               sepBy (parameter <|> (token >>= \t -> return (t, S.empty)))
                     (olws >> char ';')
      hdrs <- many hdr_field_val
      crlf
      return FormField {
                   ffName = maybe S.empty id $ lookup (S8.pack "name") parms
                 , ffParams = parms
                 , ffHeaders = cdhdr:hdrs
                 }

inumMultipart :: (Monad m) => HttpReq -> Inum L L m a
inumMultipart req iter = flip mkInumM (iter <* nullI) $ do
  b <- bstr
  ipipe $ inumStopString b
  (crlf <?> chunkShow b)
    where
      bstr = case reqBoundary req of
               Just b  -> return $ S8.pack "\r\n--" `S8.append` b
               Nothing -> throwI $ IterMiscParseErr "inumMultipart: no parts"

foldMultipart :: (Monad m) =>
                 HttpReq -> (a -> FormField -> Iter L m a) -> a -> Iter L m a
foldMultipart req f z = multipartI req >>= doPart
    where
      doPart Nothing = return z
      doPart (Just mp) =
          inumMultipart req .| (f z mp <* nullI) `inumBind` \a ->
          foldMultipart req f a


--
-- HTTP Response support
--

-- | HTTP status code and text description of response, for the first
-- line of an HTTP response message.  A bunch of pre-defined statuses
-- from RFC 2616 are supplied under the names 'stat200', 'stat404',
-- 'stat500', etc.
data HttpStatus = HttpStatus !Int !S.ByteString deriving Show

mkStat :: Int -> String -> HttpStatus
mkStat n s = HttpStatus n $ S8.pack s

fmtStat :: HttpStatus -> L
fmtStat (HttpStatus n s) = L.fromChunks [
                            S8.pack $ "HTTP/1.1 " ++ show n ++ " "
                           , s, S8.pack "\r\n"]

stat100, stat200
           , stat301, stat302, stat303, stat304
           , stat400, stat401, stat403, stat404, stat405
           , stat500, stat501 :: HttpStatus
stat100 = mkStat 100 "Continue"
stat200 = mkStat 200 "OK"
stat301 = mkStat 301 "Moved Permanently"
stat302 = mkStat 302 "Found"
stat303 = mkStat 303 "See Other"
stat304 = mkStat 304 "Not Modified"
stat400 = mkStat 400 "Bad Request"
stat401 = mkStat 401 "Unauthorized"
stat403 = mkStat 403 "Forbidden"
stat404 = mkStat 404 "Not Found"
stat405 = mkStat 405 "Method not allowed"
stat500 = mkStat 500 "Internal Server Error"
stat501 = mkStat 501 "Not Implemented"

-- | A data structure describing an HTTP response message to be sent,
-- parameterized by the Monad in which the response will be written to
-- the network.
data HttpResp m = HttpResp {
      respStatus :: !HttpStatus
    -- ^ The response status.
    , respHeaders :: ![S.ByteString]
    -- ^ Headers to send back
    , respChunk :: !Bool
    -- ^ True if the message body should be passed through
    -- 'inumToChunks' and a \"@Transfer-Encoding: chunked@\" header
    -- should be added.  Generally this should be 'True' unless you
    -- have added a @Content-Length@ header, manually set up chunk
    -- encoding by fusing it in 'respBody', or are not returning a
    -- message body with the reply.
    , respBody :: !(Onum L.ByteString m (IterR L.ByteString m ()))
    -- ^ 'Onum' producing the message body.  Use 'inumNull' (which is
    -- an empty 'Inum') to produce an empty body for responses that do
    -- not contain a body.
    }

respAddHeader :: S.ByteString -> HttpResp m -> HttpResp m
respAddHeader hdr resp = resp { respHeaders = hdr : respHeaders resp }

instance Show (HttpResp m) where
    showsPrec _ resp rest = "HttpResp (" ++ show (respStatus resp)
                            ++ ") " ++ show (respHeaders resp) ++ rest

-- | An empty HTTP response, to which you must add headers and
-- possibly a message body.
defaultHttpResp :: (Monad m) => HttpResp m
defaultHttpResp = HttpResp { respStatus = stat200
                           , respHeaders = []
                           , respChunk = True
                           , respBody = inumNull
                           }

-- | Generate an 'HttpResp' without a body.
mkHttpHead :: (Monad m) => HttpStatus -> HttpResp m
mkHttpHead stat = HttpResp { respStatus = stat
                           , respHeaders = []
                           , respChunk = False
                           , respBody = inumNull }

-- | Generate an 'HttpResp' with a body of type @text/html@.
mkHtmlResp :: (Monad m) =>
              HttpStatus
           -> L.ByteString      -- ^ Body as a pure lazy 'L.ByteString'
           -> HttpResp m
mkHtmlResp stat html = resp
    where resp0 = mkHttpHead stat `asTypeOf` resp
          ctype = S8.pack "Content-Type: text/html"
          len = S8.pack $ "Content-Length: " ++ show (L8.length html)
          resp  = resp0 { respHeaders = respHeaders resp0 ++ [ctype, len]
                        , respBody = inumPure html
                        }

mkContentLenResp :: (Monad m)
                 => HttpStatus
                 -> String
                 -> L.ByteString
                 -> HttpResp m
mkContentLenResp stat ctype body =
  HttpResp { respStatus = stat
           , respHeaders = [contentType, contentLength]
           , respChunk = False
           , respBody = inumPure body }
 where
  contentType = S8.pack $ "Content-Type: " ++ ctype
  contentLength = S8.pack $ "Content-Length: " ++ show (L8.length body)

mkOnumResp :: (Monad m)
           => HttpStatus
           -> String
           -> Onum L.ByteString m (IterR L.ByteString m ())
           -> HttpResp m
mkOnumResp stat ctype body =
  HttpResp { respStatus = stat
           , respHeaders = [contentType]
           , respChunk = True
           , respBody = body }
 where
  contentType = S8.pack $ "Content-Type: " ++ ctype

htmlEscapeChar :: Char -> Maybe String
htmlEscapeChar '<'  = Just "&lt;"
htmlEscapeChar '>'  = Just "&gt;"
htmlEscapeChar '&'  = Just "&amp;"
htmlEscapeChar '"'  = Just "&quot;"
htmlEscapeChar '\'' = Just "&amp;"
htmlEscapeChar _   = Nothing

htmlEscape :: String -> L.ByteString
htmlEscape str = L8.unfoldr next (str, "")
    where
      next (s, h:t)  = Just (h, (s, t))
      next (h:t, "") = maybe (Just (h, (t, ""))) (curry next t) $
                       htmlEscapeChar h
      next ("", "")  = Nothing

-- | Generate a 301 (redirect) response
resp301 :: (Monad m) => String -> HttpResp m
resp301 target =
    respAddHeader (S8.pack $ "Location: " ++ target) $ mkHtmlResp stat301 html
    where html = L8.concat
                 [L8.pack
                  "<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML 2.0//EN\">\n\
                  \<HTML><HEAD>\n\
                  \<TITLE>301 Moved Permanently</TITLE>\n\
                  \</HEAD><BODY>\n\
                  \<H1>Moved Permanently</H1>\n\
                  \<P>The document has moved <A HREF=\""
                 , htmlEscape target
                 , L8.pack "\">here</A>.</P>\n"]

-- | Generate a 303 (see other) response
resp303 :: (Monad m) => String -> HttpResp m
resp303 target =
    respAddHeader (S8.pack $ "Location: " ++ target) $ mkHtmlResp stat303 html
    where html = L8.concat
                 [L8.pack
                  "<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML 2.0//EN\">\n\
                  \<HTML><HEAD>\n\
                  \<TITLE>303 See Other</TITLE>\n\
                  \</HEAD><BODY>\n\
                  \<H1>See Other</H1>\n\
                  \<P>The document has moved <A HREF=\""
                 , htmlEscape target
                 , L8.pack "\">here</A>.</P>\n"]

-- | Generate a 404 (not found) response.
resp404 :: (Monad m) => HttpReq -> HttpResp m
resp404 req = mkHtmlResp stat404 html
    where html = L8.concat
                 [L8.pack
                  "<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML 2.0//EN\">\n\
                  \<HTML><HEAD>\n\
                  \<TITLE>404 Not Found</TITLE>\n\
                  \</HEAD><BODY>\n\
                  \<H1>Not Found</H1>\n\
                  \<P>The requested URL "
                 , htmlEscape $ S8.unpack (reqNormalPath req)
                 , L8.pack " was not found on this server.</P>\n\
                           \</BODY></HTML>\n"]

-- | Generate a 404 (method not allowed) response.
resp405 :: (Monad m) => HttpReq -> HttpResp m
resp405 req = mkHtmlResp stat405 html
    where html = L8.concat
                 [L8.pack
                  "<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML 2.0//EN\">\n\
                  \<HTML><HEAD>\n\
                  \<TITLE>405 Method Not Allowed</TITLE>\n\
                  \</HEAD><BODY>\n\
                  \<H1>Method Not Allowed</H1>\n\
                  \<P>The requested method "
                 , L.fromChunks [reqMethod req]
                 , L8.pack " is not allowed for the URL "
                 , htmlEscape $ S8.unpack (reqNormalPath req)
                 , L8.pack ".</P>\n\
                           \</BODY></HTML>\n"]

-- | Generate a 500 (internal server error) response.
resp500 :: (Monad m) => String -> HttpResp m
resp500 msg = mkHtmlResp stat500 html
    where html = L8.concat
                 [L8.pack
                  "<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML 2.0//EN\">\n\
                  \<HTML><HEAD>\n\
                  \<TITLE>500 Internal Server Error</TITLE>\n\
                  \</HEAD><BODY>\n\
                  \<H1>Internal Server Error</H1>\n\
                  \<P>"
                 , htmlEscape msg
                 , L8.pack "</P>\n</BODY></HTML>\n"]

-- | Format and enumerate a response header and body.
enumHttpResp :: (Monad m) =>
                HttpResp m
             -> Maybe UTCTime   -- ^ Time for @Date:@ header (if desired)
             -> Onum L.ByteString m ()
enumHttpResp resp mdate = inumPure fmtresp `cat` (respBody resp |. maybeChunk)
    where
      fmtresp = L.append (fmtStat $ respStatus resp) hdrs
      hdrs = foldr (L.append . hdr) (L8.pack "\r\n") $
             (if respChunk resp
              then ((S8.pack "Transfer-Encoding: chunked") :)
              else id) $
             (maybe id (\t -> (S8.pack ("Date: " ++ http_fmt_time t) :)) mdate)
             (respHeaders resp)
      hdr h = L.fromChunks [h, S8.pack "\r\n"]
      maybeChunk = if respChunk resp then inumToChunks else inumNop

-- | Given the headers of an HTTP request, provides an iteratee that
-- will process the request body (if any) and return a response.
type HttpRequestHandler m = HttpReq -> Iter L.ByteString m (HttpResp m)

-- | Data structure describing the configuration of an HTTP server for
-- 'inumHttpServer'.
data HttpServerConf m = HttpServerConf {
      srvLogger :: !(String -> Iter L.ByteString m ())
    , srvDate :: !(Iter L.ByteString m (Maybe UTCTime))
    , srvHandler :: !(HttpRequestHandler m)
    }

-- | Generate a null 'HttpServerConf' structure with no logging and no
-- Date header.
nullHttpServer :: (Monad m) => HttpRequestHandler m -> HttpServerConf m
nullHttpServer handler = HttpServerConf {
                           srvLogger = const $ return ()
                         , srvDate = return Nothing
                         , srvHandler = handler
                         }

-- | Generate an 'HttpServerConf' structure that uses IO calls to log to
-- standard error and get the current time for the Date header.
ioHttpServer :: (MonadIO m) => HttpRequestHandler m -> HttpServerConf m
ioHttpServer handler = HttpServerConf {
                         srvLogger = liftIO . hPutStrLn stderr
                       , srvDate = liftIO $ Just `liftM` getCurrentTime
                       , srvHandler = handler
                       }

-- | An 'Inum' that behaves like an HTTP server.
inumHttpServer :: (MonadIO m) =>
                  HttpServerConf m  -- ^ Server configuration
               -> Inum L.ByteString L.ByteString m ()
inumHttpServer server = mkInumM loop
    where
      loop = do
        eof <- atEOFI
        unless eof doreq
      doreq = do
        req <- httpReqI
        let handler = srvHandler server req
        resp <- liftIterM $ inumHttpBody req .|
                (catchI handler errHandler <* nullI)
        now <- liftIterM $ srvDate server
        tryI (irun $ enumHttpResp resp now) >>=
             either (fatal . fst) (const loop)
      errHandler e@(SomeException _) _ = do
        srvLogger server $ "Response error: " ++ show e
        return $ resp500 $ show e
      fatal e@(SomeException _) = do
        liftIterM $ srvLogger server $ "Reply error: " ++ show e
        return ()


{-

--
-- Everything below here is crap for testing
--

formTest :: L -> IO ()
formTest b = inumPure b |$ handleReq
 where
  handleReq = do
    req <- httpReqI
    parts <- foldForm req getPart []
    liftIO $ putStrLn $ "### Summary\n" ++ show parts
  getPart result mp = do
    liftIO $ do putStrLn $ "### Part " ++ show (length result); print mp; putStrLn ""
    stdoutI
    liftIO $ putStr "\n\n"
    return (mp:result)

formTestMultipart :: IO ()
formTestMultipart = formTest postReq

formTestUrlencoded :: IO ()
formTestUrlencoded = formTest postReqUrlencoded

{-
dumpCtl :: () -> Multipart -> Iter L IO ()
dumpCtl () mp = do
  liftIO $ S.putStr (ffName mp) >> putStrLn ":"
  stdoutI
  liftIO $ putStrLn "\n"

x :: L
x = L8.pack "p1=v1&p2=v2"
-}

mptest :: IO ()
mptest = inumPure postReq |$ (httpReqI >>= getHead)
    where
      getHead req = do
        mmp <- multipartI req
        case mmp of
          Nothing -> return ()
          Just mp -> do liftIO $ print mp
                        (inumMultipart req ) .| stdoutI
                        (inumMultipart req ) .| nullI
                        (inumMultipart req ) .| nullI
                        (inumMultipart req ) .| nullI
                        (inumMultipart req ) .| nullI
                        crlf
                        liftIO $ putStr "\n\n"
                        getHead req

mptest' :: IO ()
mptest' = inumPure postReq |$ (httpReqI >>= getParts 0)
    where
      getParts :: (MonadIO m) => Integer -> HttpReq -> Iter L m ()
      getParts n req = do
        mmp <- multipartI req
        case mmp of
          Nothing -> return ()
          Just mp -> do liftIO $ do
                          putStrLn $ "### Part " ++ show n
                          print mp
                          putStrLn ""
                        (inumMultipart req) .| stdoutI
                        liftIO $ putStr "\n\n"
                        getParts (n+1) req
 

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
 \nothing\r\n\
 \-----------------------------28986267117678495841915281966\n\
 \Content-Disposition: form-data; name=\"hate\"\n\
 \\n\
 \666\r\n\
 \-----------------------------28986267117678495841915281966\n\
 \Content-Disposition: form-data; name=\"file1\"; filename=\"x\"\n\
 \Content-Type: application/octet-stream\n\
 \\n\
 \search scs.stanford.edu uun.org\n\
 \nameserver 127.0.0.1\n\
 \nameserver 64.81.79.2\n\
 \nameserver 216.231.41.2\n\
 \\r\n\
 \-----------------------------28986267117678495841915281966--\n"

postReqUrlencoded :: L
postReqUrlencoded = L8.pack
 "POST /testSubmit HTTP/1.1\n\
 \Host: localhost:8000\n\
 \User-Agent: Mozilla/5.0 (X11; U; Linux i686 (x86_64); en-US; rv:1.9.2.8) Gecko/20100722 Firefox/3.6.8\n\
 \Accept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8\n\
 \Accept-Language: en-us,en;q=0.5\n\
 \Accept-Encoding: gzip,deflate\n\
 \Accept-Charset: ISO-8859-1,utf-8;q=0.7,*;q=0.7\n\
 \Keep-Alive: 115\n\
 \Connection: keep-alive\n\
 \Content-Type: application/x-www-form-urlencoded\n\
 \Content-Length: 11\n\
 \\n\
 \p1=v1&p2=v2"


encReq :: L
encReq = L8.pack "justatestkey=nothing&hate=666&file1=mtab"

-}
