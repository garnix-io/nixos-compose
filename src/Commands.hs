module Commands
  ( list,
    start,
    stop,
    ssh,
    status,
  )
where

import Context
import Control.Concurrent (threadDelay)
import Control.Monad (forM_)
import Cradle
import Data.List.NonEmpty qualified as NonEmpty
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Options (StartOptions (..))
import State
import StdLib
import System.Directory (doesFileExist)
import System.IO (stderr)
import System.Posix (sigKILL, signalProcess)
import System.Process (getPid)
import Utils
import Prelude

list :: Context -> IO ()
list ctx = do
  vms <- listVms (nixVms ctx) ctx
  T.putStrLn $ case vms of
    [] -> "no vms configured"
    vms -> "configured vms: " <> T.intercalate ", " (map vmNameToText vms)

start :: Context -> StartOptions -> IO ()
start ctx startOptions = do
  vmNames <- case startOptions of
    StartAll -> do
      vmNames <- listVms (nixVms ctx) ctx
      when (null vmNames) $ do
        T.hPutStrLn stderr "No vms are defined. Nothing to do."
        throwIO $ ExitFailure 1
      pure vmNames
    StartSome vmNames -> pure $ NonEmpty.toList vmNames
  forM_ vmNames $ \vmName -> do
    alreadyRunning <- listRunningVms ctx
    if vmName `elem` alreadyRunning
      then do
        T.putStrLn $ vmNameToText vmName <> ": already running"
      else do
        vmKeyPath <- getStateFile ctx vmName "vmkey"
        exists <- doesFileExist vmKeyPath
        when exists $ do
          error $ vmKeyPath <> " already exists"
        () <-
          runWithErrorHandling $
            -- todo: make runtime dep
            Cradle.cmd "ssh-keygen"
              & Cradle.addArgs ["-f", vmKeyPath, "-N", ""]
        ph <- buildAndRun (nixVms ctx) ctx vmName
        registerProcess ctx ph
        pid <- getPid ph <&> fromMaybe (error "no pid")
        state <- readState ctx vmName
        writeState ctx vmName (state {pid = Just $ fromIntegral pid})
        waitForVm ctx vmName

stop :: Context -> VmName -> IO ()
stop ctx vmName = do
  state <- readState ctx vmName
  case state ^. #pid of
    Just pid -> signalProcess sigKILL $ fromIntegral pid
    Nothing -> error "pid missing from state file"
  removeState ctx vmName

waitForVm :: Context -> VmName -> IO ()
waitForVm ctx vmName = do
  (StdoutRaw _, StderrRaw _, exitCode) <- sshIntoHost (nixVms ctx) ctx vmName ["true"]
  when (exitCode /= Cradle.ExitSuccess) $ do
    threadDelay 1_000_000
    waitForVm ctx vmName

ssh :: Context -> VmName -> [Text] -> IO ()
ssh ctx vmName command = do
  sshIntoHost (nixVms ctx) ctx vmName command

status :: Context -> [VmName] -> IO ()
status ctx vmName = do
  runningVms <- sort <$> State.listRunningVms ctx
  T.putStr $ T.unlines $ case vmName of
    [] -> flip map runningVms $ \runningVm ->
      vmNameToText runningVm <> ": running"
    vmNames -> flip map vmNames $ \vmName ->
      if vmName `elem` runningVms
        then vmNameToText vmName <> ": running"
        else vmNameToText vmName <> ": not running"
