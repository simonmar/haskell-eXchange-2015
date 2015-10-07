module LogExample where

import LogDataSource
import BlogDataSource
import Haxl.Core

type Haxl a = GenHaxl () a

run :: Haxl a -> IO a
run h = do
  log <- LogDataSource.initDataSource
  db <- BlogDataSource.initDataSource
  env <- initEnv (stateSet db (stateSet log stateEmpty)) ()
  runHaxl env h
