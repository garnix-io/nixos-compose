module Context where

import Cradle qualified
import Options (Verbosity, VmName)
import StdLib
import System.IO
import System.Process
import Utils (Port)

data Context = Context
  { registerProcess :: ProcessHandle -> IO (),
    stdin :: Handle,
    workingDir :: FilePath,
    storageDir :: FilePath,
    nixVms :: NixVms
  }
  deriving stock (Generic)

data NixVms = NixVms
  { listVms :: Context -> IO [VmName],
    buildAndRun :: Context -> Verbosity -> VmName -> IO (ProcessHandle, Port),
    sshIntoHost :: forall o. (Cradle.Output o) => Context -> VmName -> [Text] -> IO o
  }
