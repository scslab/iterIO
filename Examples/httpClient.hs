{-# LANGUAGE OverloadedStrings #-}
import Data.IterIO
import Data.IterIO.Http
import Data.IterIO.HttpClient
import OpenSSL
import qualified OpenSSL.Session as SSL
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy as L
import System.Environment

import Control.Concurrent

main :: IO ()
main = withOpenSSL $ do
  args <- getArgs
  case read . head $ args of
   0 -> httpExample
   1 -> httpsExample
   2 -> httpChunkedExample
   3 -> httpsChunkedExample
   _ -> return ()

httpExample = do
  res <- simpleGetHttp "http://tools.ietf.org/html/rfc2616"
  enumHttpResp res |$ stdoutI
  
httpsExample = do
  ctx <- SSL.context
  SSL.contextSetCADirectory ctx "/etc/ssl/certs/"
  res <- simpleGetHttps "https://tools.ietf.org/html/rfc2616" ctx
  enumHttpResp res |$ stdoutI

httpChunkedExample = do
  res <- simpleGetHttp "http://www.google.com"
  enumHttpResp res |$ stdoutI

httpsChunkedExample = do
  ctx <- SSL.context
  SSL.contextSetCADirectory ctx "/etc/ssl/certs/"
  res <- simpleGetHttps "https://encrypted.google.com" ctx
  enumHttpResp res |$ stdoutI
