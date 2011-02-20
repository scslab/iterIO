module Data.IterIO.HttpRoute (HttpRoute(..)
                 , routeConst, routeFn, routeReq
                 , routeMethod, routeHost, routeTop
                 , HttpMap, routeMap, routeName, routeVar
                 ) where

import           Control.Monad
import           Data.Char (toLower)
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy as L
import qualified Data.Map as Map
import           Data.Monoid

import           Data.IterIO
import           Data.IterIO.Http

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

-- | Matches any directory name, but additionally pushes it onto the
-- front of the 'reqPathParams' list in the 'HttpReq' structure.  This
-- allows the name to serve as a variable argument to the eventual
-- handling function.
routeVar :: HttpRoute m -> HttpRoute m
routeVar (HttpRoute route) = HttpRoute check
    where check req = case reqPathLst req of
                        _:_ -> route $ popPath True req
                        _   -> Nothing

