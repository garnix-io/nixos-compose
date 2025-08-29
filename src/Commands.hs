module Commands
  ( list,
    down,
    ssh,
    status,
    Commands.ip,
    tap,
  )
where

import Context
import Context.Utils
import Control.Exception.Safe (throwIO)
import Cradle
import Data.Containers.ListUtils (nubOrd)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map qualified as Map
import Data.Text qualified as T
import Logging
import Net.IPv4 qualified as IPv4
import Options (AllOrSomeVms (..), DryRunFlag, VmName (..))
import State
import StdLib
import System.Console.ANSI qualified as ANSI
import System.IO qualified
import System.Posix (sigKILL, signalProcess)
import Table (renderTable)
import Vde qualified

list :: Context -> IO ()
list ctx = do
  vms <- listVms (nixVms ctx) ctx
  output ctx $ case vms of
    [] -> "no vms configured"
    vms -> "configured vms:\n" <> T.intercalate "\n" (map (("  - " <>) . vmNameToText) vms)

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

tap :: Context -> DryRunFlag -> IO ()
tap ctx dryRunFlag = do
  state <- readState ctx
  case state ^. #vde of
    Nothing -> do
      info ctx "Cannot start `tap` device with no VMs running"
      throwIO $ ExitFailure 1
    Just _ -> do
      Vde.setupTapDevice ctx dryRunFlag hostIp
