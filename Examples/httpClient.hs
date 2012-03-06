import Data.IterIO
import Data.IterIO.Http
import Data.IterIO.HttpClient
import OpenSSL
import qualified OpenSSL.Session as SSL


exampleNr :: Int
exampleNr = 3

main :: IO ()
main = withOpenSSL $ 
  case exampleNr of
   0 -> httpExample
   1 -> httpsExample
   2 -> httpChunkedExample
   3 -> httpsChunkedExample
   _ -> return ()

httpExample = do
  res <- simpleHttp "http://tools.ietf.org/html/rfc2616" Nothing
  enumHttpResp res |$ stdoutI
  
httpsExample = do
  ctx <- SSL.context
  SSL.contextSetCADirectory ctx "/etc/ssl/certs/"
  res <- simpleHttp "https://tools.ietf.org/html/rfc2616" (Just ctx)
  enumHttpResp res |$ stdoutI

httpChunkedExample = do
  res <- simpleHttp "http://www.google.com" Nothing
  enumHttpResp res |$ stdoutI

httpsChunkedExample = do
  ctx <- SSL.context
  SSL.contextSetCADirectory ctx "/etc/ssl/certs/"
  res <- simpleHttp "https://encrypted.google.com" (Just ctx)
  enumHttpResp res |$ stdoutI
