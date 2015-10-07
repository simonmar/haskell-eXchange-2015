module BlogExample where

import HaxlBlog
import Haxl.Core

example1 = (,) <$> getPostContent 1 <*> getPostContent 2






example2 = do
  a <- getPostContent 1
  b <- getPostContent 2
  return (a ++ b)








example3 = do
  a <- getPostContent 1
  b <- if length a > 10 then getPostContent 3 else getPostContent 4
  c <- getPostContent 2
  return (concat [a,b,c])







example4 = do
  a <- getPostContent 1
  b <- if length a > 10 then getPostContent 3 else getPostContent 4
  c <- getPostContent 2
  d <- if length c > 10 then getPostContent 5 else getPostContent 6
  return (concat [a,b,c,d])
