module BlogDB where

import Database.SQLite
import Control.Exception
import Data.Typeable
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader

-- -----------------------------------------------------------------------------
-- A monad

type Blog a = ReaderT SQLiteHandle IO a

run :: Blog a -> IO a
run m = do
  db <- openConnection "blog.sqlite"
  runReaderT m db


-- -----------------------------------------------------------------------------
-- An API

type PostId = Int
type PostContent = String

getPostIds     :: Blog [PostId]
getPostContent :: PostId -> Blog PostContent
-- more operations...


-- -----------------------------------------------------------------------------
-- Implementation

sql :: String -> Blog (Either String [[Row Value]])
sql query = do
  db <- ask
  liftIO $ do
    putStrLn query
    execStatement db query


getPostIds = do
  r <- sql "select postid from postinfo;"
  case r of
    Right [rows] -> return [ fromIntegral id | [(_,Int id)] <- rows ]
    Left s -> liftIO $ throwIO (BlogDBException s)
    _ -> liftIO $ throwIO (BlogDBException "invalid result")

getPostContent x = do
  r <- sql ("select content from postcontent where postid = " ++
            show x ++ ";")
  case r of
    Right [[[(_,Text str)]]] -> return str
    Left s -> liftIO $ throwIO (BlogDBException s)
    _ -> liftIO $ throwIO (BlogDBException "invalid result")

data BlogDBException = BlogDBException String
  deriving (Show, Typeable)

instance Exception BlogDBException
