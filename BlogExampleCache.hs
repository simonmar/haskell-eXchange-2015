{-# LANGUAGE ApplicativeDo #-}
module BlogExampleCache where

import BlogDataSource
import HaxlBlog
import Haxl.Core

runDump :: Haxl a -> IO a
runDump h = do
  db <- initDataSource
  env <- initEnv (stateSet db stateEmpty) ()
  r <- runHaxl env h
  runHaxl env dumpCacheAsHaskell >>= putStr
  return r

example = do
  a <- getPostContent 1
  b <- getPostContent 2
  return (a ++ b)
