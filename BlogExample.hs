module BlogExample where

import HaxlBlog

example1 = do
  ids <- getPostIds
  first3 <- mapM getPostContent (take 3 ids)
  last3 <- mapM getPostContent (take 3 (reverse ids))
  return (first3 ++ last3)
