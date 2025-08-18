module Commands
  ( list,
    up,
    down,
    ssh,
    status,
    Commands.ip,
    tap,
  )
where

import Context
import Context.Utils
import Control.Exception.Safe (onException, throwIO)
import Cradle
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map qualified as Map
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Logging
import Net.IPv4 qualified as IPv4
import Options (AllOrSomeVms (..), DryRunFlag, Verbosity, VmName (..))
import State
import StdLib
import System.Directory (doesFileExist)
import System.Posix (sigKILL, signalProcess)
import System.Process (ProcessHandle, getPid, getProcessExitCode)
import Utils
import Vde qualified

list :: Context -> IO ()
list ctx = do
  vms <- listVms (nixVms ctx) ctx
  output ctx $ case vms of
    [] -> "no vms configured"
    vms -> "configured vms:\n" <> T.intercalate "\n" (map (("  - " <>) . vmNameToText) vms)

up :: Context -> Verbosity -> AllOrSomeVms -> IO ()
up ctx verbosity upOptions = do
  vmNames <- case upOptions of
    All -> do
      vmNames <- listVms (nixVms ctx) ctx
      case vmNames of
        [] -> abort ctx "No vms are defined. Nothing to do."
        a : r -> pure $ a :| r
    Some vmNames -> pure vmNames
  forM_ vmNames $ \vmName -> do
    ip <- getNextIp ctx
    existing <- claimVm ctx vmName $ Starting {ip}
    case existing of
      Left existing -> output ctx $ vmNameToText vmName <> ": already " <> vmStateToText existing
      Right () -> do
        (ph, pid, port) <- removeVmWhenFailing ctx vmName $ do
          vmKeyPath <- getVmFilePath ctx vmName "vmkey"
          exists <- doesFileExist vmKeyPath
          when exists $ do
            impossible ctx $ cs vmKeyPath <> " already exists"
          () <-
            runWithErrorHandling ctx $
              Cradle.cmd "ssh-keygen"
                & Cradle.addArgs ["-f", vmKeyPath, "-N", ""]
          (ctx ^. #logger . #setPhase) vmName "building"
          (vmScript, port) <- buildVmScript (nixVms ctx) ctx vmName ip
          (ctx ^. #logger . #setPhase) vmName "starting"
          ph <- (ctx ^. #nixVms . #runVm) ctx verbosity vmName vmScript
          registerProcess ctx (Vm vmName) ph
          pid <-
            System.Process.getPid ph
              >>= maybe (impossible ctx "qemu process has no pid") pure
          pure (ph, pid, port)
        State.writeVmState ctx vmName (Running {pid, port, ip})
        waitForVm ctx vmName ph
        (ctx ^. #logger . #clearPhase) vmName
  updateVmHostEntries ctx

removeVmWhenFailing :: Context -> VmName -> IO a -> IO a
removeVmWhenFailing ctx vmName action = do
  onException action $ do
    State.removeVm ctx vmName

down :: Context -> AllOrSomeVms -> IO ()
down ctx vmNames = do
  toStop <- case vmNames of
    Some vmNames -> pure vmNames
    All -> do
      all <- listRunningVms ctx
      case Map.keys all of
        [] -> do
          info ctx "no vms running, nothing to do"
          exitSuccess
        a : r -> pure $ a :| r
  state <- readState ctx
  forM_ toStop $ \vmName -> do
    case Map.lookup vmName (state ^. #vms) of
      Nothing -> output ctx $ vmNameToText vmName <> " is not running, nothing to do"
      Just vmState -> case vmState of
        Starting {} -> abort ctx $ vmNameToText vmName <> ": building, cannot stop a building vm"
        Running {pid} -> do
          output ctx $ "stopping " <> vmNameToText vmName
          signalProcess sigKILL pid
          removeVm ctx vmName

waitForVm :: Context -> VmName -> ProcessHandle -> IO ()
waitForVm ctx vmName ph = do
  (StdoutRaw _, StderrRaw _, sshExitCode) <- (ctx ^. #nixVms . #sshIntoVm . to runSshIntoVm) ctx vmName "true"
  case sshExitCode of
    ExitSuccess -> pure ()
    ExitFailure _ -> do
      vmScriptExitCode <- getProcessExitCode ph
      case vmScriptExitCode of
        Nothing -> waitForVm ctx vmName ph
        Just vmScriptExitCode -> do
          stdout <- getVmFilePath ctx vmName "stdout.log" >>= T.readFile
          stderr <- getVmFilePath ctx vmName "stderr.log" >>= T.readFile
          do
            info ctx (T.intercalate "\n" ["VM failed to start:\n", stdout, stderr])
            exitWith $ case vmScriptExitCode of
              ExitSuccess -> ExitFailure 1
              vmScriptExitCode -> vmScriptExitCode

ssh :: Context -> VmName -> Text -> IO ()
ssh ctx vmName command = do
  exitCode :: ExitCode <- (ctx ^. #nixVms . #sshIntoVm . to runSshIntoVm) ctx vmName command
  throwIO exitCode

status :: Context -> [VmName] -> IO ()
status ctx args = do
  configuredVms <- listVms (nixVms ctx) ctx
  runningVms <- State.listRunningVms ctx
  output ctx $ T.intercalate "\n" $ case configuredVms of
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
    Nothing -> abort ctx $ "vm not running: " <> vmNameToText vm
    Just vmState -> output ctx $ IPv4.encode (vmState ^. #ip)
  pure state

updateVmHostEntries :: Context -> IO ()
updateVmHostEntries ctx = do
  runningVms <- Map.keys <$> listRunningVms ctx
  forM_ runningVms $ \targetVmName -> do
    case parseHostname $ vmNameToText targetVmName of
      Nothing -> info ctx $ "WARN: \"" <> vmNameToText targetVmName <> "\" is not a valid hostname. It will not be added to /etc/hosts."
      Just targetHostname -> do
        targetIp <- (^. #ip) <$> readVmState ctx targetVmName
        forM_ runningVms $ \updatingVmName -> do
          updateVmHostsEntry (nixVms ctx) ctx updatingVmName targetHostname targetIp

tap :: Context -> DryRunFlag -> IO ()
tap ctx dryRunFlag = do
  state <- readState ctx
  case state ^. #vde of
    Nothing -> do
      info ctx "Cannot start `tap` device with no VMs running"
      throwIO $ ExitFailure 1
    Just _ -> do
      Vde.setupTapDevice ctx dryRunFlag hostIp
