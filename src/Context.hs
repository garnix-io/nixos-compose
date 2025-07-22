module Context where

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

newtype NixVms = NixVms
  { buildAndRun :: Context -> VmName -> IO ProcessHandle
  }

newtype VmName = VmName {vmNameToText :: Text}
  deriving stock (Eq, Show)
