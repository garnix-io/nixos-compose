module Context where

import Cradle qualified
import StdLib
import System.IO
import System.Process

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
    buildAndRun :: Context -> VmName -> IO ProcessHandle,
    sshIntoHost :: forall o. (Cradle.Output o) => Context -> VmName -> [Text] -> IO o
  }

newtype VmName = VmName {vmNameToText :: Text}
  deriving stock (Eq, Show, Ord)
