module CmdExample where

import CmdDataSource
import Haxl.Core

type Haxl a = GenHaxl () a

run :: Haxl a -> IO a
run h = do
  st <- CmdDataSource.initDataSource
  env <- initEnv (stateSet st stateEmpty) ()
  runHaxl env h
