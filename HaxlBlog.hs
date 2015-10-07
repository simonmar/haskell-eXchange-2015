{-# LANGUAGE
    StandaloneDeriving, GADTs, TypeFamilies,
    FlexibleInstances, MultiParamTypeClasses, GeneralizedNewtypeDeriving,
    OverloadedStrings, DeriveDataTypeable
 #-}

module HaxlBlog (
    PostId, PostContent,
    getPostIds, getPostContent,
    Haxl,
    run
  ) where


import BlogDataSource
import Haxl.Core

type Haxl a = GenHaxl () a

run :: Haxl a -> IO a
run h = do
  db <- initDataSource
  env <- initEnv (stateSet db stateEmpty) ()
  runHaxl env h

