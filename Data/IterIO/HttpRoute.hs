
module Data.IterIO.HttpRoute
    (HttpRoute(..)
    , runHttpRoute
    , routeConst, routeFn, routeReq
    , routeMethod, routeHost, routeTop
    , HttpMap, routeMap, routeMap', routeName, routePath, routeVar
    , mimeTypesI, dirRedir, routeFileSys, FileSystemCalls(..), routeGenFileSys
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
  mmap <- Map.fromList <$> concatI ((mimeLine <|> nil) <* eol)
  return $ \suffix -> maybe (S8.pack deftype) id $ Map.lookup suffix mmap
    where
      mimeLine = do
        typ <- word
        many $ do space; ext <- word; return (S8.unpack ext, typ)
      word = while1I $ \c -> c > eord ' ' && c <= eord '~'
      space = skipWhile1I $ \c -> c == eord ' ' || c == eord '\t'
      comment = char '#' >> skipWhileI (/= eord '\n')
      eol = do
        optionalI space
        optionalI comment
        optionalI (char '\r'); char '\n'

-- | An abstract representation of file system calls returning an
-- opaque handle type @h@ in an 'Iter' parameterized by an arbitrary
-- 'Monad' @m@.  This representation allows one to use
-- 'routeGenFileSys' in a monad that is not an instance of 'MonadIO'.
data FileSystemCalls h m = FileSystemCalls {
      fs_stat :: !(FilePath -> Iter L.ByteString m FileStatus)
    -- ^ Return file attributes.
    , fs_open :: !(FilePath -> Iter L.ByteString m h)
    -- ^ Open file and return an opaque handle of type @h@.
    , fs_close :: !(h -> Iter L.ByteString m ())
    -- ^ Close an open file.  You must call this unless you apply the
    -- enumerator returned by @fs_enum@.
    , fs_fstat :: !(h -> Iter L.ByteString m FileStatus)
    -- ^ Return the attributes of an open file.
    , fs_enum :: !(h -> Iter L.ByteString m
                        (Onum L.ByteString m (IterR L.ByteString m ())))
    -- ^ Enumerate the contents of an open file, then close the file.
    -- If you apply the 'Onum' returned by @fs_enum@, you do not need
    -- to call @fs_close@.
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

-- | @dirRedir indexFileName@ redirects requests to the URL formed by
-- appending @\"/\" ++ indexFileName@ to the requested URL.
dirRedir :: (Monad m) => FilePath -> FilePath -> HttpRoute m
dirRedir index _path = routeFn $ \req -> return $
                       resp301 $ S8.unpack (reqNormalPath req) ++ '/':index

modTimeUTC :: FileStatus -> UTCTime
modTimeUTC = posixSecondsToUTCTime . realToFrac . modificationTime

-- | Route a request to a directory tree in the file system.  It gets
-- the Content-Length from the target file's attributes (after opening
-- the file).  Thus, overwriting files on an active server could cause
-- problems, while renaming new files into place should be safe.
routeFileSys :: (MonadIO m) =>
                (String -> S8.ByteString)
             -- ^ Map of file suffixes to mime types (see 'mimeTypesI')
             -> (FilePath -> HttpRoute m)
             -- ^ Handler to invoke when the URL maps to a directory
             -- in the file system.  Reasonable options include:
             --
             -- * @('const' 'mempty')@ to do nothing, which results in a
             --   403 forbidden,
             --
             -- * @('dirRedir' \"index.html\")@ to redirect directory
             --   accesses to an index file, and
             --
             -- * a recursive invocation such as @(routeFileSys
             -- typemap . (++ \"/index.html\"))@ to re-route the
             -- request directly to an index file.
             -> FilePath
             -- ^ Pathname of directory to serve from file system
             -> HttpRoute m
routeFileSys = routeGenFileSys defaultFileSystemCalls

-- | A generalized version of 'routeFileSys' that takes a
-- 'FileSystemCalls' object and can therefore work outside of the
-- 'MonadIO' monad.  Other than the 'FileSystemCalls' object, the
-- arguments and their meaning are identical to 'routeFileSys'.
routeGenFileSys :: (Monad m) =>
                   FileSystemCalls h m
                -> (String -> S8.ByteString)
                -> (FilePath -> HttpRoute m)
                -> FilePath
                -> HttpRoute m
routeGenFileSys fs typemap index dir0 = HttpRoute $ Just . check
    where
      dir = if null dir0 then "." else dir0
      checkErr req e _ | isDoesNotExistError e = return $ resp404 req
                       | otherwise             = return $ resp500 (show e)
      check req = flip catchI (checkErr req) $ do
        let path = dir ++ concatMap (('/' :) . S8.unpack) (reqPathLst req)
        st <- fs_stat fs path
        case () of
          _ | isRegularFile st     -> doFile req path st
            | not (isDirectory st) -> return $ resp404 req
            | otherwise -> runHttpRoute
                           (index path `mappend` routeConst (resp403 req)) req
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


