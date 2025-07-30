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

data Context = Context
  { registeredProcesses :: Maybe (MVar (Map ProcessType ProcessHandle)),
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

registerProcess :: Context -> ProcessType -> ProcessHandle -> IO ()
registerProcess ctx typ handle = case ctx ^. #registeredProcesses of
  Nothing -> pure ()
  Just processes -> modifyMVar_ processes $ \map -> do
    pure (Map.insert typ handle map)

data NixVms = NixVms
  { listVms :: Context -> IO [VmName],
    buildVmScript :: Context -> VmName -> IPv4 -> IO (FilePath, Port),
    runVm :: Context -> Verbosity -> VmName -> FilePath -> IO ProcessHandle,
    sshIntoVm :: forall o. (Cradle.Output o) => Context -> VmName -> [Text] -> IO o
  }
