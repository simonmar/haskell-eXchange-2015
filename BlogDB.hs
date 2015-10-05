module BlogDB where

import Database.SQLite
import Control.Exception
import Data.Typeable
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader

type PostId = Int
type PostContent = String

type Blog a = ReaderT SQLiteHandle IO a

runBlog :: Blog a -> IO a
runBlog m = do
  db <- openConnection "blog.sqlite"
  runReaderT m db

getPostIds     :: Blog [PostId]
getPostContent :: PostId -> Blog PostContent
-- ... more operations


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
