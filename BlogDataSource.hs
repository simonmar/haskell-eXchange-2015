{-# LANGUAGE
    StandaloneDeriving, GADTs, TypeFamilies,
    FlexibleInstances, MultiParamTypeClasses, GeneralizedNewtypeDeriving,
    OverloadedStrings, DeriveDataTypeable
 #-}

module BlogDataSource
  ( PostId, PostContent
  , getPostIds
  , getPostContent
  , initDataSource
  , BlogRequest(..)
  , BlogDBException(..)
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


-- -----------------------------------------------------------------------------
-- Types

type PostId = Int
type PostContent = String


-- -----------------------------------------------------------------------------
-- Request type

data BlogRequest a where
  FetchPosts       :: BlogRequest [PostId]
  FetchPostContent :: PostId -> BlogRequest PostContent

deriving instance Show (BlogRequest a)
deriving instance Typeable BlogRequest

instance Show1 BlogRequest where show1 = show

deriving instance Eq (BlogRequest a)

instance Hashable (BlogRequest a) where
  hashWithSalt salt FetchPosts = hashWithSalt salt (0::Int)
  hashWithSalt salt (FetchPostContent p) = hashWithSalt salt (1::Int, p)


-- -----------------------------------------------------------------------------
-- Requests

getPostIds :: GenHaxl u [PostId]
getPostIds = dataFetch FetchPosts

getPostContent :: PostId -> GenHaxl u PostContent
getPostContent = dataFetch . FetchPostContent

-- more operations ...


-- -----------------------------------------------------------------------------
-- Data source implementation

instance StateKey BlogRequest where
  data State BlogRequest = BlogDataState SQLiteHandle

initDataSource :: IO (State BlogRequest)
initDataSource = BlogDataState <$> openConnection "blog.sqlite"

instance DataSourceName BlogRequest where
  dataSourceName _ = "BlogDataSource"

instance DataSource u BlogRequest where
  fetch (BlogDataState db) _flags _userEnv blockedFetches =
    SyncFetch $ batchFetch db blockedFetches



-- -----------------------------------------------------------------------------
-- Group requests by type

batchFetch :: SQLiteHandle -> [BlockedFetch BlogRequest] -> IO ()
batchFetch db = doFetch db . foldr collect emptyBatches

type Batches
  = ( [ResultVar [PostId]]              -- FetchPosts
    , [(PostId, ResultVar PostContent)] -- FetchPostContent
    )

emptyBatches :: Batches
emptyBatches = ([],[])

collect :: BlockedFetch BlogRequest -> Batches -> Batches
collect (BlockedFetch FetchPosts v) (as,bs) = (v:as,bs)
collect (BlockedFetch (FetchPostContent x) v) (as,bs) = (as,(x,v):bs)


-- -----------------------------------------------------------------------------
-- Fetch data for each batch

doFetch :: SQLiteHandle -> Batches -> IO ()
doFetch db (as,bs) = do
  sqlMultiFetch db as id
    "select postid from postinfo;"
    (\row -> do [(_,Int id)] <- Just row; return (fromIntegral id))
    id
    (\_ ids -> Just ids)

  sqlMultiFetch db bs snd
    ("select postid,content from postcontent where postid in " ++
       idList (map fst bs))
    (\row -> do
       [(_,Int id),(_,Text content)] <- Just row
       return (fromIntegral id, content))
    Map.fromList
    (\(x,_) -> Map.lookup x)


sqlMultiFetch
  :: SQLiteHandle
  -> [x]
  -> (x -> ResultVar a)
  -> String
  -> (Row Value -> Maybe y)
  -> ([y] -> z)
  -> (x -> z -> Maybe a)
  -> IO ()

sqlMultiFetch _  [] _ _ _ _ _ = return ()
sqlMultiFetch db requests getvar query parserow collate extract = do
  results <- sql db query
  case results of
    Left s -> failAll (BlogDBException s)
    Right [rows] -> do
      let fetched = collate (catMaybes (map parserow rows))
      forM_ requests $ \q ->
        case extract q fetched of
          Nothing -> putFailure (getvar q) (BlogDBException "missing result")
          Just r -> putSuccess (getvar q) r
    _other -> failAll (BlogDBException "invalid result")
 where
  failAll e = forM_ requests $ \q -> putFailure (getvar q) e

idList :: [PostId] -> String
idList ids = "(" ++ intercalate "," (map show ids) ++ ")"

sql :: SQLiteHandle -> String -> IO (Either String [[Row Value]])
sql db query = do
  putStrLn query
  execStatement db query

data BlogDBException = BlogDBException String
  deriving (Show, Typeable)

instance Exception BlogDBException where
  toException = transientErrorToException
  fromException = transientErrorFromException

