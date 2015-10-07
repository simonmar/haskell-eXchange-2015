{-# OPTIONS_GHC -Wall -fno-warn-name-shadowing -fno-warn-unused-do-bind #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}

module RunCmd.DataSource (
    initGlobalState,
    RunCmdReq(..),
  ) where

import Haxl.Prelude
import Prelude ()

import Haxl.Core

import Control.Concurrent
import Control.Exception
import Control.Monad (zipWithM, forever, void)
import Data.Time
import Text.Printf
import Data.Typeable
import Data.Hashable
import Control.Concurrent.Async
import System.Process
import System.Exit
import System.IO
import GHC.IO.Exception ( IOErrorType(..), IOException(..) )
import Foreign.C

data RunCmdReq a where
  RunCmdReq :: String -> [String] -> RunCmdReq ExitCode
  deriving Typeable -- requests must be Typeable

deriving instance Eq (RunCmdReq a)
deriving instance Show (RunCmdReq a)

instance Show1 RunCmdReq where show1 = show

instance Hashable (RunCmdReq a) where
   hashWithSalt s (RunCmdReq prog args) = hashWithSalt s (0::Int,prog,args)

instance StateKey RunCmdReq where
  data State RunCmdReq = RunCmdState { logChan :: Chan String }

instance DataSourceName RunCmdReq where
  dataSourceName _ = "RunCmd"

instance DataSource u RunCmdReq where
  fetch = buildCmdFetch

initGlobalState :: IO (State RunCmdReq)
initGlobalState = do
  chan <- newChan
  forkIO (forever $ readChan chan >>= putStr)
  return RunCmdState { logChan = chan }

buildCmdFetch :: State RunCmdReq             -- current state
             -> Flags                        -- tracing verbosity, etc.
             -> u                            -- user environment
             -> [BlockedFetch RunCmdReq]    -- requests to fetch
             -> PerformFetch                 -- tells the framework how to fetch

buildCmdFetch RunCmdState { logChan = chan } _flags _user bfs =
  FullyAsyncFetch $ do
    t <- getCurrentTime
    zipWithM (\ n b -> async (fetch1 t chan n b)) [1..] bfs
    return ()


fetch1 :: UTCTime -> Chan String -> Int -> BlockedFetch RunCmdReq -> IO ()
fetch1 t0 chan n (BlockedFetch (RunCmdReq cmd args) rvar) = do
  writeChan chan $ printf "[%d] %s\n" n (unwords (cmd:args))
  r <- Control.Exception.try $
         withCreateProcess_ (proc cmd args) { delegate_ctlc = True } $
            \_ _ _ p -> waitForProcess p
  t1 <- getCurrentTime
  let t = realToFrac (diffUTCTime t1 t0) :: Double
  let status = case r of
                 Right ExitSuccess -> "OK"
                 Right (ExitFailure n) -> "exit(" ++ show n ++ ")"
                 Left e -> show e
  writeChan chan $ printf "[%d] %s %.2fs\n" n status t
  putResult rvar r


-- Copied from System.Process, because it doesn't export withCreateProcess

withCreateProcess_
  :: CreateProcess
  -> (Maybe Handle -> Maybe Handle -> Maybe Handle -> ProcessHandle -> IO a)
  -> IO a
withCreateProcess_ c action =
    bracketOnError (createProcess c) cleanupProcess
                     (\(m_in, m_out, m_err, ph) -> action m_in m_out m_err ph)


cleanupProcess :: (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)
               -> IO ()
cleanupProcess (mb_stdin, mb_stdout, mb_stderr, ph) = do
    terminateProcess ph
    -- Note, it's important that other threads that might be reading/writing
    -- these handles also get killed off, since otherwise they might be holding
    -- the handle lock and prevent us from closing, leading to deadlock.
    maybe (return ()) (ignoreSigPipe . hClose) mb_stdin
    maybe (return ()) hClose mb_stdout
    maybe (return ()) hClose mb_stderr
    -- terminateProcess does not guarantee that it terminates the process.
    -- Indeed on Unix it's SIGTERM, which asks nicely but does not guarantee
    -- that it stops. If it doesn't stop, we don't want to hang, so we wait
    -- asynchronously using forkIO.
    _ <- forkIO (waitForProcess ph >> return ())
    return ()

ignoreSigPipe :: IO () -> IO ()
ignoreSigPipe = handle $ \e -> case e of
                                   IOError { ioe_type  = ResourceVanished
                                           , ioe_errno = Just ioe }
                                     | Errno ioe == ePIPE -> return ()
                                   _ -> throwIO e
