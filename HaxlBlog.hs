{-# LANGUAGE
    StandaloneDeriving, GADTs, TypeFamilies,
    FlexibleInstances, MultiParamTypeClasses, GeneralizedNewtypeDeriving,
    OverloadedStrings, DeriveDataTypeable
 #-}

module HaxlBlog (
    -- * User API
    PostId, PostContent,
    getPostIds, getPostContent,

    -- * Data source API
    runBlog,
  ) where


import BlogDataSource

import Haxl.Core

-- -----------------------------------------------------------------------------
-- Requests

type Haxl a = GenHaxl () a


runBlog :: Haxl a -> IO a
runBlog h = do
  db <- initDataSource
  env <- initEnv (stateSet db stateEmpty) ()
  runHaxl env h
