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
import Data.Containers.ListUtils (nubOrd)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map qualified as Map
import Data.Text qualified as T
import Data.Text.IO qualified as T
import GHC.Conc (atomically)
import Ki qualified
import Logging
import Net.IPv4 qualified as IPv4
import Options (AllOrSomeVms (..), DryRunFlag, Verbosity, VmName (..))
import State
import StdLib
import System.Console.ANSI qualified as ANSI
import System.Directory (doesFileExist)
import System.IO qualified
import System.Posix (sigKILL, signalProcess)
import System.Process (ProcessHandle, getPid, getProcessExitCode)
import Table (renderTable, unstyledText)
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

  Ki.scoped $ \scope -> do
    forM_ vmNames $ \vmName -> do
      ip <- getNextIp ctx
      Ki.fork scope $ do
        existing <- claimVm ctx vmName $ Building {ip}
        case existing of
          Left existing ->
            output ctx $
              vmNameToText vmName
                <> ": already "
                <> unstyledText (vmStateToText (Just existing))
          Right () -> do
            (ctx ^. #logger . #setPhase) vmName "building"
            (pid, port) <- removeVmWhenFailing ctx vmName $ do
              vmKeyPath <- getVmFilePath ctx vmName "vmkey"
              exists <- doesFileExist vmKeyPath
              when exists $ do
                impossible ctx $ cs vmKeyPath <> " already exists"
              () <-
                runWithErrorHandling ctx $
                  Cradle.cmd "ssh-keygen"
                    & Cradle.addArgs ["-f", vmKeyPath, "-N", ""]
              (vmScript, port) <- buildVmScript (nixVms ctx) ctx vmName ip
              State.writeVmState ctx vmName $ Booting {ip}
              (ctx ^. #logger . #setPhase) vmName "booting"
              ph <- (ctx ^. #nixVms . #runVm) ctx verbosity vmName vmScript
              registerProcess ctx (Vm vmName) ph
              pid <-
                System.Process.getPid ph
                  >>= maybe (impossible ctx "qemu process has no pid") pure
              waitForVm ctx vmName port ph
              pure (pid, port)
            State.writeVmState ctx vmName (Running {pid, port, ip})
            (ctx ^. #logger . #clearPhase) vmName
    atomically $ Ki.awaitAll scope
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
        Building {} -> abort ctx $ vmNameToText vmName <> ": building, cannot stop a building vm"
        Booting {} -> abort ctx $ vmNameToText vmName <> ": booting, cannot stop a booting vm"
        Running {pid} -> do
          output ctx $ "stopping " <> vmNameToText vmName
          signalProcess sigKILL pid
          removeVm ctx vmName

waitForVm :: Context -> VmName -> Port -> ProcessHandle -> IO ()
waitForVm ctx vmName port ph = do
  (StdoutRaw _, StderrRaw _, sshExitCode) <- (ctx ^. #nixVms . #sshIntoVm . to runSshIntoVm) ctx vmName port "true"
  case sshExitCode of
    ExitSuccess -> pure ()
    ExitFailure _ -> do
      vmScriptExitCode <- getProcessExitCode ph
      case vmScriptExitCode of
        Nothing -> waitForVm ctx vmName port ph
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
  vmState <- State.readVmState ctx vmName
  case vmState of
    Building {} -> do
      abort ctx "cannot ssh into a building vm"
    Booting {} -> do
      abort ctx "cannot ssh into a building vm"
    Running {port} -> do
      exitCode :: ExitCode <- (ctx ^. #nixVms . #sshIntoVm . to runSshIntoVm) ctx vmName port command
      throwIO exitCode

status :: Context -> [VmName] -> IO ()
status ctx args = do
  configuredVms <- listVms (nixVms ctx) ctx
  runningVms <- State.listRunningVms ctx
  let listedVms = case args of
        [] -> nubOrd (configuredVms <> Map.keys runningVms)
        args -> args
  case listedVms of
    [] -> output ctx "no vms configured, no vms running"
    vmNames -> do
      supportsAnsi <- ANSI.hNowSupportsANSI System.IO.stdout
      output ctx $
        T.stripEnd $
          renderTable supportsAnsi $
            flip map vmNames $ \vmName ->
              [ ("name", cs $ vmNameToText vmName),
                ("status", vmStateToText (Map.lookup vmName runningVms))
              ]

ip :: Context -> VmName -> IO ()
ip ctx vm = modifyState_ ctx $ \state -> do
  case Map.lookup vm (state ^. #vms) of
    Nothing -> abort ctx $ "vm not running: " <> vmNameToText vm
    Just vmState -> output ctx $ IPv4.encode (vmState ^. #ip)
  pure state

updateVmHostEntries :: Context -> IO ()
updateVmHostEntries ctx = do
  runningVms <- listRunningVms ctx
  forM_ (Map.keys runningVms) $ \targetVmName -> do
    case parseHostname $ vmNameToText targetVmName of
      Nothing -> info ctx $ "WARN: \"" <> vmNameToText targetVmName <> "\" is not a valid hostname. It will not be added to /etc/hosts."
      Just targetHostname -> do
        targetIp <- (^. #ip) <$> readVmState ctx targetVmName
        forM_ (Map.toList runningVms) $ \(updatingVmName, updatingVmState) -> do
          case updatingVmState of
            Building {} -> pure ()
            Booting {} -> pure ()
            Running {port} -> do
              updateVmHostsEntry (nixVms ctx) ctx updatingVmName port targetHostname targetIp

tap :: Context -> DryRunFlag -> IO ()
tap ctx dryRunFlag = do
  state <- readState ctx
  case state ^. #vde of
    Nothing -> do
      info ctx "Cannot start `tap` device with no VMs running"
      throwIO $ ExitFailure 1
    Just _ -> do
      Vde.setupTapDevice ctx dryRunFlag hostIp
