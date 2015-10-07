{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Main (main) where

import Haxl.Prelude
import Prelude ()

import Haxl.Core
import Haxl.Core.Monad (unsafeLiftIO)
import System.FilePath
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Monoid
import System.Directory
import Control.Monad (when)
import qualified Data.Text as Text
import Data.Typeable
import Data.Hashable
import Data.List
import Data.Maybe
import System.Environment

import CmdDataSource

type Haxl a = GenHaxl () a

main :: IO ()
main = do
  runCmdState <- initDataSource
  env <- initEnv (stateSet runCmdState stateEmpty) ()
  args <- getArgs
  r <- runHaxl env $ haskellProgram "prog" args
  print r

-- -----------------------------------------------------------------------------
-- Memoizing build steps

newtype BuildFile target result = BuildFile target
  deriving (Typeable, Eq, Show, Hashable)

-- Cache the build of a particular target
buildFile
  :: ( Show target, Show result
     , Hashable target, Eq target, Typeable target, Typeable result )
  => target
  -> Haxl result
  -> Haxl result

buildFile f = cachedComputation (BuildFile f)

-- -----------------------------------------------------------------------------
-- Compiling Haskell programs

haskellProgram :: FilePath -> [FilePath] -> Haxl ()
haskellProgram prog hsfiles = buildFile prog $ do
  deps <- haskellDependencies hsfiles "deps"
  _ <- mapM (haskellModule deps . dropExtensions) hsfiles
  cmd "ghc" (["-o", prog] ++ map (mkObj . dropExtensions) hsfiles)

type Module = String
type HaskellDeps = Map Module [Module]

haskellModule :: HaskellDeps -> Module -> Haxl ()
haskellModule deps mod = buildFile obj $ do
  _ <- mapM (haskellModule deps) $ Map.findWithDefault [] mod deps
  cmd "ghc" ["-c", src, "-o", obj]
 where
  src = mkHs mod; obj = mkObj mod

mkHs = (<.> "hs")
mkObj = (<.> "o")

haskellDependencies :: [FilePath] -> FilePath -> Haxl (Map FilePath [FilePath])
haskellDependencies hsfiles depfile = buildFile depfile $ do
  cmd "ghc" (["-M", "-dep-suffix", "", "-dep-makefile=" ++ depfile] ++ hsfiles)
  deps <- unsafeLiftIO $ readFile depfile
  return (parseDepFile deps)

parseDepFile :: String -> Map Module [Module]
parseDepFile f = Map.fromListWith (++)
  [ (dropExtensions m, [dropExtensions dep])
  | [m,_,dep] <- map words (lines f)
  , ".o" `isSuffixOf` m
  , ".hi" `isSuffixOf` dep ]
