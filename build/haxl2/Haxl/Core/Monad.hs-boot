{-# LANGUAGE RoleAnnotations #-}
module Haxl.Core.Monad where

type role IVar nominal representational
data IVar u a

-- showIVar :: IVar u a -> String
