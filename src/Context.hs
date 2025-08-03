module Context where

import Control.Concurrent (MVar, modifyMVar_)
import Cradle qualified
import Data.Map (Map)
import Data.Map qualified as Map
import Net.IPv4 (IPv4)
import Options (Verbosity, VmName)
import StdLib
import System.IO
import System.Process
import Utils (Port)

data TestState = TestState
  { registeredProcesses :: Map ProcessType ProcessHandle,
    vmHostEntries :: Map (VmName, Text) IPv4
  }
  deriving stock (Generic)

data Context = Context
  { testState :: Maybe (MVar TestState),
    stdin :: Handle,
    workingDir :: FilePath,
    storageDir :: FilePath,
    nixVms :: NixVms
  }
  deriving stock (Generic)

data ProcessType
  = VdeSwitch
  | Vm VmName
  deriving stock (Show, Eq, Ord)

updateTestState :: Context -> (TestState -> IO TestState) -> IO ()
updateTestState ctx update = case ctx ^. #testState of
  Nothing -> pure ()
  Just testState -> modifyMVar_ testState update

registerProcess :: Context -> ProcessType -> ProcessHandle -> IO ()
registerProcess ctx typ handle =
  updateTestState ctx $ pure . (#registeredProcesses %~ Map.insert typ handle)

data NixVms = NixVms
  { listVms :: Context -> IO [VmName],
    buildVmScript :: Context -> VmName -> IPv4 -> IO (FilePath, Port),
    runVm :: Context -> Verbosity -> VmName -> FilePath -> IO ProcessHandle,
    sshIntoVm :: SshIntoVm,
    updateVmHostsEntry :: Context -> VmName -> Text -> IPv4 -> IO ()
  }
  deriving stock (Generic)

-- wrapper type to make `generic-lens` work
newtype SshIntoVm = SshIntoVm
  { runSshIntoVm :: forall o. (Cradle.Output o) => Context -> VmName -> Text -> IO o
  }
