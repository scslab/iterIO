
module Data.IterIO.HttpRoute
    (HttpRoute(..)
    , routeConst, routeFn, routeReq
    , routeMethod, routeHost, routeTop
    , HttpMap, routeMap, routeName, routePath, routeVar
    , mimeTypesI, routeFileSys
    ) where

import           Control.Monad
import           Control.Monad.Trans
import           Data.Char (toLower)
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy as L
import qualified Data.Map as Map
import           Data.Monoid
import           Data.Time (UTCTime)
import           Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import           System.FilePath
import           System.IO.Error (isDoesNotExistError)
import           System.Posix.Files

import           Data.IterIO
import           Data.IterIO.Http
import           Data.IterIO.Parse

--
-- Request routing
--

-- | Simple HTTP request routing structure for 'inumHttpServer'.  This
-- is a wrapper around a function on 'HttpReq' structures.  If the
-- function accepts the 'HttpReq', it returns 'Just' a response
-- action.  Otherwise it returns 'Nothing'.
--
-- @HttpRoute@ is a 'Monoid', and hence can be concatenated with
-- 'mappend' or 'mconcat'.  For example, you can say something like:
--
-- > simpleServer :: Iter L.ByteString IO ()  -- Output to web browser
-- >              -> Onum L.ByteString IO ()  -- Input from web browser
-- >              -> IO ()
-- > simpleServer iter enum = enum |$ inumHttpServer routing .| iter
-- >     where routing = mconcat [ routeTop $ routeConst $ resp301 "/cabal"
-- >                             , routeName "cabal" $ routeFn serve_cabal
-- >                             ]
--
-- The above function will redirect requests for @/@ to the URL
-- @/cabal@ using an HTTP 301 (Moved Permanently) response.  Any
-- request for a path under @/cabal/@ will then have the first
-- component (@\"cabal\"@) stripped from 'reqPathLst', have that same
-- component added to 'reqPathCtx', and finally be routed to the
-- function @serve_cabal@.
data HttpRoute m = HttpRoute {
      runHttpRoute :: !(HttpReq -> Maybe (Iter L.ByteString m (HttpResp m)))
    }

instance Monoid (HttpRoute m) where
    mempty = HttpRoute $ const Nothing
    mappend (HttpRoute a) (HttpRoute b) =
        HttpRoute $ \req -> a req `mplus` b req

popPath :: Bool -> HttpReq -> HttpReq
popPath isParm req =
    case reqPathLst req of
      h:t -> req { reqPathLst = t
                 , reqPathCtx = reqPathCtx req ++ [h]
                 , reqPathParams = if isParm then h : reqPathParams req
                                             else reqPathParams req
                 }
      _   -> error "Data.IterIO.Http.popPath: empty path"

-- | Route all requests to a constant response action that does not
-- depend on the request.  This route always succeeds, so anything
-- 'mappend'ed will never be used.
routeConst :: (Monad m) => HttpResp m -> HttpRoute m
routeConst resp = HttpRoute $ const $ Just $ return resp

-- | Route all requests to a particular function.  This route always
-- succeeds, so anything 'mappend'ed will never be used.
routeFn :: (HttpReq -> Iter L.ByteString m (HttpResp m)) -> HttpRoute m
routeFn fn = HttpRoute $ Just . fn

-- | Construct a route by inspecting the request.
routeReq :: (HttpReq -> HttpRoute m) -> HttpRoute m
routeReq fn = HttpRoute $ \req ->
                let (HttpRoute route) = fn req
                in route req

-- | Route the root directory (/).
routeTop :: HttpRoute m -> HttpRoute m
routeTop (HttpRoute route) = HttpRoute $ \req ->
                             if null $ reqPathLst req then route req
                             else Nothing

-- | Route requests whose \"Host:\" header matches a particular
-- string.
routeHost :: String -- ^ String to compare against host (must be lower-case)
          -> HttpRoute m   -- ^ Target route to follow if host matches
          -> HttpRoute m
routeHost host (HttpRoute route) = HttpRoute check
    where shost = S8.pack $ map toLower host
          check req | reqHost req /= shost = Nothing
                    | otherwise            = route req

-- | Route based on the method (GET, POST, etc.) in a request.
routeMethod :: String           -- ^ String method should match
            -> HttpRoute m      -- ^ Target route to take if method matches
            -> HttpRoute m
routeMethod method (HttpRoute route) = HttpRoute check
    where smethod = S8.pack method
          check req | reqMethod req /= smethod = Nothing
                    | otherwise                = route req

-- | Type alias for the argument of 'routeMap'.
type HttpMap m = [(String, HttpRoute m)]

-- | @routeMap@ builds an efficient map out of a list of
-- @(directory_name, 'HttpRoute')@ pairs.
routeMap :: HttpMap m -> HttpRoute m
routeMap lst = HttpRoute check
    where
      check req = case reqPathLst req of
                    h:_ -> maybe Nothing
                           (\(HttpRoute route) -> route $ popPath False req)
                           (Map.lookup h rmap)
                    _   -> Nothing
      packfirst (a, b) = (S8.pack a, b)
      rmap = Map.fromListWithKey nocombine $ map packfirst lst
      nocombine k _ _ = error $ "routeMap: duplicate key for " ++ S8.unpack k

-- | Routes a specific directory name, like 'routeMap' for a singleton
-- map.
routeName :: String -> HttpRoute m -> HttpRoute m
routeName name (HttpRoute route) = HttpRoute check
    where sname = S8.pack name
          headok (h:_) | h == sname = True
          headok _                  = False
          check req | headok (reqPathLst req) = route $ popPath False req
          check _                             = Nothing

-- | Routes a specific path, like 'routeName', except that the path
-- can include several directories.
routePath :: String -> HttpRoute m -> HttpRoute m
routePath path route = foldr routeName route dirs
    where dirs = case splitDirectories path of
                   "/":t -> t
                   t     -> t

-- | Matches any directory name, but additionally pushes it onto the
-- front of the 'reqPathParams' list in the 'HttpReq' structure.  This
-- allows the name to serve as a variable argument to the eventual
-- handling function.
routeVar :: HttpRoute m -> HttpRoute m
routeVar (HttpRoute route) = HttpRoute check
    where check req = case reqPathLst req of
                        _:_ -> route $ popPath True req
                        _   -> Nothing

--
-- Routing to Filesystem
--

-- | Parses mime.types file data.  Returns a function mapping file
-- suffixes to mime types.  The argument is a default mime type for
-- suffixes to do not match any in the mime.types data.  (Reasonable
-- defaults might be @\"text/html\", @\"text/plain\"@, or, more
-- pedantically but less usefully, @\"application/octet-stream\"@.)
--
-- Since this likely doesn't change, it is convenient just to define
-- it once in your program, for instance with something like:
--
-- > mimeMap :: String -> S8.ByteString
-- > mimeMap = unsafePerformIO $ do
-- >             path <- findMimeTypes ["mime.types"
-- >                                   , "/etc/mime.types"
-- >                                   , "/var/www/conf/mime.types"]
-- >             enumFile path |$ mimeTypesI "application/octet-stream"
-- >     where
-- >       findMimeTypes (h:t) = do exist <- fileExist h
-- >                                if exist then return h else findMimeTypes t
-- >       findMimeTypes []    = return "mime.types" -- cause error
mimeTypesI :: (Monad m) =>
              String
           -> Iter S8.ByteString m (String -> S8.ByteString)
mimeTypesI deftype = do
  mmap <- Map.fromList <$> foldrI (++) [] ((mimeLine <|> nil) <* eol)
  return $ \suffix -> maybe (S8.pack deftype) id $ Map.lookup suffix mmap
    where
      mimeLine = do
        typ <- word
        foldrI (\a b -> (S8.unpack a, typ):b) [] (space >> word)
      word = while1I $ \c -> c > eord ' ' && c <= eord '~'
      space = skipWhile1I $ \c -> c == eord ' ' || c == eord '\t'
      comment = char '#' >> skipWhileI (/= eord '\n')
      eol = do
        optionalI space
        optionalI comment
        optionalI (char '\r'); char '\n'

modTimeUTC :: FileStatus -> UTCTime
modTimeUTC = posixSecondsToUTCTime . realToFrac . modificationTime

-- | Route a request to a file system directory tree.  Bad things can
-- happen if the directory tree is modified while this function is
-- running.
routeFileSys :: (MonadIO m) =>
                (String -> S8.ByteString)
             -- ^ Map of file suffixes to mime types (see 'mimeTypesI')
             -> FilePath
             -- ^ Default file name to append when the request is for
             -- a directory (e.g., @\"index.html\"@)
             -> FilePath
             -- ^ Pathname of directory to serve from file system
             -> HttpRoute m
routeFileSys typemap idx dir0 = HttpRoute $ Just . check
    where
      dir = if null dir0 then "." else dir0
      check req = do
        let path = dir ++ concatMap (('/' :) . S8.unpack) (reqPathLst req)
        estat <- tryI $ liftIO $ getFileStatus path
        case estat of
          Right st | isRegularFile st -> doFile req path st
                   | isDirectory st -> return $ resp301 $
                                       S8.unpack (reqNormalPath req) ++ '/':idx
                   | otherwise -> return $ resp404 req
          Left (e,_ ) | isDoesNotExistError e -> return $ resp404 req
                      | otherwise             -> return $ resp500 (show e)
      doFile req path st
          | reqMethod req == S8.pack "GET"
            && maybe True (< (modTimeUTC st)) (reqIfModifiedSince req) =
              return $ resp { respBody = enumFile path }
          | reqMethod req == S8.pack "GET" =
              return $ resp { respStatus = stat304 }
          | reqMethod req == S8.pack "HEAD" =
              return $ resp { respStatus = stat200 }
          | otherwise = return $ resp405 req
          where resp = defaultHttpResp { respChunk = False
                                       , respHeaders = mkHeaders req st }
                                            
      mkHeaders req st =
          [ S8.pack $ "Last-Modified: " ++ (http_fmt_time $ modTimeUTC st)
          , S8.pack $ "Content-Length: " ++ (show $ fileSize st)
          , S8.pack "Content-Type: " `S8.append` typemap (fileExt req) ]
      fileExt req =
          case reqPathLst req of
            [] -> []
            l  -> case takeExtension (S8.unpack $ last l) of [] -> []; _:t -> t

