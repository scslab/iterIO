module Main (main) where

import           Control.Concurrent (threadDelay)
import           Control.Monad.Trans
import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy.Char8 as L
import           Data.IterIO
import           Data.IterIO.Http
import           Data.IterIO.Zlib
import           Data.List (intersperse)
import qualified Data.ListLike as LL
import qualified System.IO as IO
import           Text.XHtml.Strict hiding (p)

import Server
import Message

type L = L.ByteString
type S = S.ByteString


main :: IO ()
main = server 8000 handleRequest

--
-- Request handler
--

handleRequest :: IO.Handle -> IO ()
handleRequest h = enumHandle' h |$ do
  req <- httpreqI
  case S.unpack $ reqMethod req of
    "GET" ->
      case reqPathLst req of
        [] -> echo req
        (x:_) -> case S.unpack x of
                  "query" -> ok $ formPage "GET" Nothing
                  "form" -> ok $ formPage "POST" Nothing
                  "form-urlencoded" -> ok $ formPage "POST" (Just urlencoded)
                  "form-multipart" -> ok $ formPage "POST" (Just multipart)
                  "echo-file" -> ok $ echoFilePage
                  "gzip-file" -> ok $ gzipFilePage
                  "slow" -> do liftIO $ threadDelay $ 5 * 1000 * 1000
                               echo req
                  _ -> echo req
    "POST" ->
      case reqPathLst req of
        (x:_) -> case S.unpack x of
                   "echo-file" -> echoFile req h >> return ()
                   "gzip-file" -> gzipFile req h >> return ()
                   "gzip" -> inumGzipResponse .| handleI h  -- process the input raw, not form-encoded
                   "submit" -> echo req
                   _ -> notFound'
        _ -> echo req
    _ -> notFound'
 where
  respondI response = inumPure response .| handleI h
  xhtmlResponseI status headers x = respondI $ xhtmlResponse status headers (toHtml x)
  ok = xhtmlResponseI statusOK []
  {-
  seeOther url = xhtmlResponseI statusSeeOther ["Location: " ++ url]
  badRequest = xhtmlResponseI statusBadRequest []
  -}
  notFound = xhtmlResponseI statusNotFound []
  notFound' = notFound "Not Found."

  echo req = parmsI req >>= ok . page "Request" . request2Html req


--
-- Services for uploaded files
--

echoFile :: (MonadIO m) => HttpReq -> IO.Handle -> Iter L m (Maybe ())
echoFile req h = withParm "input" req $ inumEchoResponse .| handleI h

inumEchoResponse :: (MonadIO m) => Inum L L m a
inumEchoResponse = mkInumAutoM $ do
  _ <- ifeed $ headersL echoHeaders
  ipipe inumToChunks

gzipFile :: (MonadIO m) => HttpReq -> IO.Handle -> Iter L m (Maybe ())
gzipFile req h = withParm "input" req $ inumGzipResponse .| handleI h

inumGzipResponse :: (MonadIO m) => Inum L L m a
inumGzipResponse = mkInumAutoM $ do
  _ <- ifeed $ headersL gzipHeaders
  ipipe (inumGzip |. inumToChunks)

gzipHeaders :: [String]
gzipHeaders = ["HTTP/1.1 200 OK", "Content-Type: application/x-gzip", "Transfer-Encoding: chunked"]

echoHeaders :: [String]
echoHeaders = ["HTTP/1.1 200 OK", "Content-Type: application/octet-stream", "Transfer-Encoding: chunked"]


--
-- Form handling
--

type Parms = [(Multipart, L, Int)]

parmsI :: (Monad m) => HttpReq -> Iter L m Parms
parmsI req = foldForm req getPart []
 where
  getPart parts mp = do
    front <- takeExactI 50
    backLen <- countI
    return ((mp,front,backLen):parts)


--
-- Html rendering
--

request2Html :: HttpReq -> Parms -> Html
request2Html req parms = toHtml
  [ header2Html req
  , parms2Html parms
  , thediv << (intersperse (toHtml " | ") $ map toHtml
                [ hotlink "/query" << "GET"
                , hotlink "/form" << "POST (default encoding)"
                , hotlink "/form-urlencoded" << "POST (urlencoded)"
                , hotlink "/form-multipart" << "POST (multipart)"
                , hotlink "/echo-file" << "Echo file"
                , hotlink "/gzip-file" << "Gzip file"
                ])
  ]

parms2Html :: Parms -> Html
parms2Html parms =
  if null parms
    then noHtml
    else thediv << [ h3 << "Parameters"
                   , ulist << (map ((li <<) . parm2Html) parms)
                   ]
 where
  parm2Html (mp,front,backLen) = toHtml
    [ strong << (S.unpack (mpName mp) ++ ": ")
    , thespan << L.unpack front
    , if backLen > 0
        then emphasize << ("... (" ++ show (fromIntegral (L.length front) + backLen) ++ " bytes)")
        else noHtml
    ]

header2Html :: HttpReq -> Html
header2Html r = toHtml [ requestLine, headers, cookies ]
 where
  requestLine = paragraph <<
     [ toHtml $ S.unpack (reqMethod r) ++ " "
     , strong <<
         (S.unpack (reqHost r)
          ++ (maybe "" (\p -> ":" ++ show p) $ reqPort r)
          ++ S.unpack (reqPath r)
          ++ (if S.null q
                then ""
                else "?" ++ S.unpack q))
     , toHtml $ " HTTP/" ++ show major ++ "." ++ show minor
     ]
  (major,minor) = reqVers r
  q = reqQuery r
  headers = defs2Html "Headers" $ reqHeaders r
  cookies = defs2Html "Cookies" $ reqCookies r
  def2Html (h,v) = toHtml [ strong << (S.unpack h ++ ": ")
                          , toHtml $ S.unpack v ]
  defs2Html hdr dd =
    if null dd
       then noHtml
       else thediv << [ h3 << hdr
                      , ulist << (map ((li <<) . def2Html) dd)
                      ]


formPage :: String -> Maybe S -> Html
formPage meth encM = page t $ toHtml
  [ h1 << t
  , paragraph << if meth == "GET"
                   then "It will be submitted as a query in the HTTP request-line."
                   else maybe "It will be submitted with the browser's default Content-Type (likely urlencoded)."
                              (("It will be submitted with Content-Type: " ++) . S.unpack)
                              encM
  , form ! ([ action "/submit", method meth ]
            ++ maybe [] ((:[]) . enctype . S.unpack) encM) <<
      ([ text "data1"
       , text "data2"
       ]
       ++ (if meth == "POST" && encM == Just multipart
            then [file "file1", file "file2"]
            else [])
       ++ [ paragraph << submit "what" meth ]
       )
  ]
 where
  text n = paragraph << [ strong << (n ++ ": "), textfield n ]
  file n = paragraph << [ strong << (n ++ ": "), afile n ]
  t = "Please complete this form"

echoFilePage :: Html
echoFilePage = page t $ toHtml
  [ h1 << t
  , form ! [ action "/echo-file", method "POST", enctype (S.unpack multipart) ] <<
      [ paragraph << "Please select a file to echo.  The server will send it right back to you."
      , afile "input"
      , submit "what" "Echo"
      ]
  ]
 where
  t = "Echo a file"

gzipFilePage :: Html
gzipFilePage = page t $ toHtml
  [ h1 << t
  , form ! [ action "/gzip-file", method "POST", enctype (S.unpack multipart) ] <<
      [ paragraph << "Please select a file to gzip:"
      , afile "input"
      , submit "what" "Gzip"
      ]
  ]
 where
  t = "Gzip a file"

page :: String -> Html -> Html
page pageTitle contents =
  thehtml << [ header << thetitle << pageTitle
             , body << contents
             ]

--
-- Utilities
--

countI :: (Monad m, ChunkData t, LL.ListLike t e) =>
          Iter t m Int
countI = more 0
 where
  more n = do
    eof <- atEOFI
    if eof
      then return n
      else do buf <- dataI
              more (n + LL.length buf)

