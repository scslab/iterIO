
module Data.IterIO.HttpRoute
    (HttpRoute(..)
    , runHttpRoute
    , routeConst, routeFn, routeReq
    , routeMethod, routeHost, routeTop
    , HttpMap, routeMap, routeMap', routeName, routePath, routeVar
    , mimeTypesI, routeFileSys, FileSystemCalls(..), routeGenFileSys
    ) where

import           Control.Monad
import           Control.Monad.Trans
import           Data.Char (toLower)
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy as L
import qualified Data.Map as Map
import           Data.Maybe
import           Data.Monoid
import           Data.Time (UTCTime)
import           Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import           System.FilePath
import           System.IO
import           System.IO.Error (isDoesNotExistError)
import           System.Posix.Files
import           System.Posix.IO
import           System.Posix.Types

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
-- > simpleServer iter enum = enum |$ inumHttpServer server .| iter
-- >     where htdocs = "/var/www/htdocs"
-- >           server = ioHttpServer $ runHttpRoute routing
-- >           routing = mconcat [ routeTop $ routeConst $ resp301 "/start.html"
-- >                             , routeName "apps" $ routeMap apps
-- >                             , routeFileSys mimeMap "index.html" htdocs
-- >                             ]
-- >           apps = [ ("app1", routeFn app1)
-- >                  , ("app2", routeFn app2) ]
-- > 
-- > app1 :: (Monad m) => HttpReq -> Iter L.ByteString m (HttpResp m)
-- > app1 = ...
--
-- The above function will redirect requests for @/@ to the URL
-- @/start.html@ using an HTTP 301 (Moved Permanently) response.  Any
-- request for a path under @/apps/@ will be redirected to the
-- functions @app1@, @app2@, etc.  Finally, any other file name will
-- be served out of the file system under the @\"\/var\/www\/htdocs\"@
-- directory.  (This example assumes @mimeMap@ has been constructed as
-- discussed for 'mimeTypesI'.)
data HttpRoute m =
    HttpRoute !(HttpReq -> Maybe (Iter L.ByteString m (HttpResp m)))

runHttpRoute :: (Monad m) =>
                HttpRoute m -> HttpReq -> Iter L.ByteString m (HttpResp m)
runHttpRoute (HttpRoute route) rq = fromMaybe (return $ resp404 rq) $ route rq

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

-- | Route based on the method (GET, POST, HEAD, etc.) in a request.
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
-- @(directory_name, 'HttpRoute')@ pairs.  It matches all requests and
-- returns a 404 error if there is a request for a name not present in
-- the map.
routeMap :: (Monad m) => HttpMap m -> HttpRoute m
routeMap lst = routeMap' lst `mappend` routeFn (return . resp404)

-- | @routeMap'@ is like @routeMap@, but only matches names that exist
-- in the map.  Thus, multiple @routeMap'@ results can be combined
-- with 'mappend'.  By contrast, combining @routeMap@ results with
-- 'mappend' is useless--the first one will match all requests (and
-- return a 404 error for names that do not appear in the map).
routeMap' :: HttpMap m -> HttpRoute m
routeMap' lst = HttpRoute check
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

-- | Parses @mime.types@ file data.  Returns a function mapping file
-- suffixes to mime types.  The argument is a default mime type for
-- suffixes to do not match any in the mime.types data.  (Reasonable
-- defaults might be @\"text\/html\"@, @\"text\/plain\"@, or, more
-- pedantically but less usefully, @\"application\/octet-stream\"@.)
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

-- | An abstract representation of file system calls returning an
-- opaque handle type @d@ in an 'Iter' parameterized by an arbitrary
-- 'Monad' @m@.  This representation allows one to use
-- 'routeGenFileSys' in a monad that is not an instance of 'MonadIO'.
data FileSystemCalls d m = FileSystemCalls {
      fs_stat :: !(FilePath -> Iter L.ByteString m FileStatus)
    , fs_open :: !(FilePath -> Iter L.ByteString m d)
    , fs_close :: !(d -> Iter L.ByteString m ())
    , fs_fstat :: !(d -> Iter L.ByteString m FileStatus)
    , fs_enum :: !(d -> Iter L.ByteString m
                        (Onum L.ByteString m (IterR L.ByteString m ())))
    }

-- | Default file system calls for instances of the @MonadIO@ class.
defaultFileSystemCalls :: (MonadIO m) => FileSystemCalls Fd m
defaultFileSystemCalls = FileSystemCalls { fs_stat = liftIO . getFileStatus
                                         , fs_open = liftIO . pathToFd
                                         , fs_close = liftIO . closeFd
                                         , fs_fstat = liftIO . getFdStatus
                                         , fs_enum = liftIO . fdToOnum
                                         }
    where pathToFd path = openFd path ReadOnly Nothing defaultFileFlags
          fdToOnum fd = do h <- fdToHandle fd
                           return $ enumHandle h `inumFinally` liftIO (hClose h)

modTimeUTC :: FileStatus -> UTCTime
modTimeUTC = posixSecondsToUTCTime . realToFrac . modificationTime

-- | Route a request to a directory tree in the file system.  It gets
-- the Content-Length from the target file's attributes (after opening
-- the file).  Thus, overwriting files on an active server could cause
-- problems, while renaming new files into place should be safe.
routeFileSys :: (MonadIO m) =>
                (String -> S8.ByteString)
             -- ^ Map of file suffixes to mime types (see 'mimeTypesI')
             -> FilePath
             -- ^ Default file name to append when the request is for
             -- a directory (e.g., @\"index.html\"@).  Empty string
             -- results in no directory access.  Do not use @\".\"@ or
             -- @\"..\"@ or you will cause an infinite redirect cycle.
             -> FilePath
             -- ^ Pathname of directory to serve from file system
             -> HttpRoute m
routeFileSys = routeGenFileSys defaultFileSystemCalls

routeGenFileSys :: (Monad m) =>
                   FileSystemCalls d m
                -> (String -> S8.ByteString)
                -> FilePath
                -> FilePath
                -> HttpRoute m
routeGenFileSys fs typemap index dir0 = HttpRoute $ Just . check
    where
      dir = if null dir0 then "." else dir0
      check req = do
        let path = dir ++ concatMap (('/' :) . S8.unpack) (reqPathLst req)
        estat <- tryI $ fs_stat fs path
        case estat of
          Right st | isRegularFile st     -> doFile req path st
                   | not (isDirectory st) -> return $ resp404 req
                   | null index           -> return $ resp403 req
                   | otherwise -> return $ resp301 $
                                  S8.unpack (reqNormalPath req) ++ '/':index
          Left (e,_ ) | isDoesNotExistError e -> return $ resp404 req
                      | otherwise             -> return $ resp500 (show e)
      doFile req path st
          | reqMethod req == S8.pack "GET"
            && maybe True (< (modTimeUTC st)) (reqIfModifiedSince req) = do
              fd <- fs_open fs path
              -- Use attributes from opened file in case file name changes
              st' <- fs_fstat fs fd `onExceptionI` fs_close fs fd
              if isRegularFile st'
                then do body <- fs_enum fs fd `onExceptionI` fs_close fs fd
                        return $ resp { respHeaders = mkHeaders req st'
                                      , respBody = body }
                else do fs_close fs fd -- File no longer file -- re-try
                        check req
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


