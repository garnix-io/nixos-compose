module Commands
  ( list,
    up,
    down,
    ssh,
    status,
    Commands.ip,
  )
where

import Context
import Control.Concurrent (threadDelay)
import Control.Exception.Safe (SomeException, try)
import Cradle
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map qualified as Map
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Net.IPv4 qualified as IPv4
import Options (AllOrSomeVms (..), Verbosity, VmName (..))
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

up :: Context -> Verbosity -> AllOrSomeVms -> IO ()
up ctx verbosity upOptions = do
  vmNames <- case upOptions of
    All -> do
      vmNames <- listVms (nixVms ctx) ctx
      case vmNames of
        [] -> do
          T.hPutStrLn stderr "No vms are defined. Nothing to do."
          throwIO $ ExitFailure 1
        a : r -> pure $ a :| r
    Some vmNames -> pure vmNames
  forM_ vmNames $ \vmName -> do
    ip <- getNextIp ctx
    existing <- claimVm ctx vmName $ Starting {ip}
    case existing of
      Left existing -> T.putStrLn $ vmNameToText vmName <> ": already " <> vmStateToText existing
      Right () -> do
        (pid, port) <- removeVmWhenFailing ctx vmName $ do
          vmKeyPath <- getVmFilePath ctx vmName "vmkey"
          exists <- doesFileExist vmKeyPath
          when exists $ do
            error $ vmKeyPath <> " already exists"
          () <-
            runWithErrorHandling $
              Cradle.cmd "ssh-keygen"
                & Cradle.addArgs ["-f", vmKeyPath, "-N", ""]
          T.hPutStrLn stderr "Building NixOS config..."
          (vmScript, port) <- buildVmScript (nixVms ctx) ctx vmName ip
          T.hPutStrLn stderr "Done"
          T.hPutStrLn stderr "Starting VM..."
          ph <- (ctx ^. #nixVms . #runVm) ctx verbosity vmName vmScript
          registerProcess ctx (Vm vmName) ph
          pid <- System.Process.getPid ph <&> fromMaybe (error "no pid")
          pure (pid, port)
        State.writeVmState ctx vmName (Running {pid = fromIntegral pid, port, ip})
        waitForVm ctx vmName
        T.hPutStrLn stderr "Done"
  updateVmHostEntries ctx

removeVmWhenFailing :: Context -> VmName -> IO a -> IO a
removeVmWhenFailing ctx vmName action = do
  result :: Either SomeException a <- try action
  case result of
    Left e -> do
      State.removeVm ctx vmName
      throwIO e
    Right a -> pure a

down :: Context -> AllOrSomeVms -> IO ()
down ctx vmNames = do
  toStop <- case vmNames of
    Some vmNames -> pure vmNames
    All -> do
      all <- listRunningVms ctx
      case Map.keys all of
        [] -> do
          T.putStrLn "no vms running, nothing to do"
          throwIO ExitSuccess
        a : r -> pure $ a :| r
  state <- readState ctx
  forM_ toStop $ \vmName -> do
    case Map.lookup vmName (state ^. #vms) of
      Nothing -> T.putStrLn $ vmNameToText vmName <> " is not running, nothing to do"
      Just vmState -> case vmState of
        Starting {} -> do
          T.hPutStrLn stderr $ vmNameToText vmName <> ": building, cannot stop a building vm"
          throwIO $ ExitFailure 1
        Running {pid} -> do
          T.putStrLn $ "stopping " <> vmNameToText vmName
          signalProcess sigKILL $ fromIntegral pid
          removeVm ctx vmName

waitForVm :: Context -> VmName -> IO ()
waitForVm ctx vmName = do
  (StdoutRaw _, StderrRaw _, exitCode) <- (ctx ^. #nixVms . #sshIntoVm . to runSshIntoVm) ctx vmName "true"
  when (exitCode /= Cradle.ExitSuccess) $ do
    threadDelay 1_000_000
    waitForVm ctx vmName

ssh :: Context -> VmName -> Text -> IO ()
ssh ctx vmName command = do
  exitCode :: ExitCode <- (ctx ^. #nixVms . #sshIntoVm . to runSshIntoVm) ctx vmName command
  throwIO exitCode

status :: Context -> [VmName] -> IO ()
status ctx args = do
  configuredVms <- listVms (nixVms ctx) ctx
  runningVms <- State.listRunningVms ctx
  T.putStr $ T.unlines $ case configuredVms of
    [] -> ["no vms configured"]
    configuredVms -> do
      let vmNames = case args of
            [] -> configuredVms
            vmNames -> vmNames
      flip map vmNames $ \vmName ->
        vmNameToText vmName
          <> ": "
          <> maybe "not running" vmStateToText (Map.lookup vmName runningVms)

ip :: Context -> VmName -> IO ()
ip ctx vm = modifyState_ ctx $ \state -> do
  case Map.lookup vm (state ^. #vms) of
    Nothing -> do
      T.hPutStrLn stderr $ "vm not running: " <> vmNameToText vm
      throwIO $ ExitFailure 1
    Just vmState -> T.putStrLn $ IPv4.encode (vmState ^. #ip)
  pure state

updateVmHostEntries :: Context -> IO ()
updateVmHostEntries ctx = do
  runningVms <- Map.keys <$> listRunningVms ctx
  forM_ runningVms $ \targetVmName -> do
    case parseHostname $ vmNameToText targetVmName of
      Nothing -> T.hPutStrLn stderr $ "WARN: \"" <> vmNameToText targetVmName <> "\" is not a valid hostname. It will not be added to /etc/hosts."
      Just targetHostname -> do
        targetIp <- (^. #ip) <$> readVmState ctx targetVmName
        forM_ runningVms $ \updatingVmName -> do
          updateVmHostsEntry (nixVms ctx) ctx updatingVmName targetHostname targetIp
