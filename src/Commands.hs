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
import Cradle
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Options (StartOptions (..), Verbosity, VmName (..))
import State
import StdLib
import System.Directory (doesFileExist)
import System.IO (stderr)
import System.Posix (sigKILL, signalProcess)
import System.Process (getPid)
import Utils
import Vde qualified
import Prelude

list :: Context -> IO ()
list ctx = do
  vms <- listVms (nixVms ctx) ctx
  T.putStrLn $ case vms of
    [] -> "no vms configured"
    vms -> "configured vms: " <> T.intercalate ", " (map vmNameToText vms)

start :: Context -> Verbosity -> StartOptions -> IO ()
start ctx verbosity startOptions = do
  vmNames <- case startOptions of
    StartAll -> do
      vmNames <- listVms (nixVms ctx) ctx
      case vmNames of
        [] -> do
          T.hPutStrLn stderr "No vms are defined. Nothing to do."
          throwIO $ ExitFailure 1
        a : r -> pure $ a :| r
    StartSome vmNames -> pure vmNames
  Vde.startIfNotRunning ctx
  forM_ vmNames $ \vmName -> do
    alreadyRunning <- listRunningVms ctx
    if vmName `elem` alreadyRunning
      then do
        T.putStrLn $ vmNameToText vmName <> ": already running"
      else do
        vmKeyPath <- getVmFilePath ctx vmName "vmkey"
        exists <- doesFileExist vmKeyPath
        when exists $ do
          error $ vmKeyPath <> " already exists"
        () <-
          runWithErrorHandling $
            Cradle.cmd "ssh-keygen"
              & Cradle.addArgs ["-f", vmKeyPath, "-N", ""]
        (vmScript, port) <- logStep "Building NixOS config..." $ do
          buildVmScript (nixVms ctx) ctx vmName
        logStep "Starting VM..." $ do
          ph <- runVm (nixVms ctx) ctx verbosity vmName vmScript
          registerProcess ctx (Vm vmName) ph
          pid <- getPid ph <&> fromMaybe (error "no pid")
          State.writeVmState ctx vmName (VmState {pid = fromIntegral pid, port})
          waitForVm ctx vmName

stop :: Context -> VmName -> IO ()
stop ctx vmName = do
  state <- readVmState ctx vmName
  signalProcess sigKILL $ fromIntegral (state ^. #pid)
  removeVm ctx vmName
  running <- listRunningVms ctx
  when (null running) $ do
    Vde.stop ctx

waitForVm :: Context -> VmName -> IO ()
waitForVm ctx vmName = do
  (StdoutRaw _, StderrRaw _, exitCode) <- sshIntoVm (nixVms ctx) ctx vmName "true"
  when (exitCode /= Cradle.ExitSuccess) $ do
    threadDelay 1_000_000
    waitForVm ctx vmName

ssh :: Context -> VmName -> Text -> IO ()
ssh ctx vmName command = do
  exitCode :: ExitCode <- sshIntoVm (nixVms ctx) ctx vmName command
  throwIO exitCode

status :: Context -> [VmName] -> IO ()
status ctx args = do
  configuredVms <- listVms (nixVms ctx) ctx
  runningVms <- sort <$> State.listRunningVms ctx
  when (null runningVms) $ do
    Vde.stop ctx
  T.putStr $ T.unlines $ case configuredVms of
    [] -> ["no vms configured"]
    configuredVms -> do
      let vmNames = case args of
            [] -> configuredVms
            vmNames -> vmNames
      flip map vmNames $ \vmName ->
        if vmName `elem` runningVms
          then vmNameToText vmName <> ": running"
          else vmNameToText vmName <> ": not running"
