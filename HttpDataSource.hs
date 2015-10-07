{-# LANGUAGE
    StandaloneDeriving, GADTs, TypeFamilies,
    FlexibleInstances, MultiParamTypeClasses, GeneralizedNewtypeDeriving,
    OverloadedStrings, DeriveDataTypeable
 #-}

module HttpDataSource
  ( getURL
  , initDataSource
  , HttpException(..)
  ) where

import Data.Hashable
import Data.Typeable
import qualified Data.Map as Map
import Control.Monad
import Data.Maybe
import Data.List
import Haxl.Core
import Database.SQLite
import Control.Exception
import Control.Concurrent.Async
import Network.HTTP.Conduit
import qualified Data.ByteString.Lazy as L
import Text.Printf
import qualified Data.Text as Text
import Data.Text (Text)

-- -----------------------------------------------------------------------------
-- Request type

data HttpRequest a where
  GetURL :: String -> HttpRequest L.ByteString

deriving instance Show (HttpRequest a)
deriving instance Typeable HttpRequest

instance Show1 HttpRequest where show1 = show

deriving instance Eq (HttpRequest a)

instance Hashable (HttpRequest a) where
  hashWithSalt salt (GetURL u) = hashWithSalt salt u

-- -----------------------------------------------------------------------------
-- Requests

getURL :: String -> GenHaxl u L.ByteString
getURL = dataFetch . GetURL

instance StateKey HttpRequest where
  data State HttpRequest = HttpState Manager

initDataSource :: IO (State HttpRequest)
initDataSource = HttpState <$> newManager tlsManagerSettings

instance DataSourceName HttpRequest where
  dataSourceName _ = "HttpDataSource"

instance DataSource u HttpRequest where
  fetch (HttpState mgr) _flags _userEnv blockedFetches =
    SyncFetch $ do
       printf "Fetching %d urls.\n" (length blockedFetches)
       void $ mapConcurrently (fetchURL mgr) blockedFetches

fetchURL :: Manager -> BlockedFetch HttpRequest -> IO ()
fetchURL mgr (BlockedFetch (GetURL url) var) = do
  e <- Control.Exception.try $ do
    req <- parseUrl url
    responseBody <$> httpLbs req mgr
  either (putFailure var) (putSuccess var)
    (e :: Either SomeException L.ByteString)

