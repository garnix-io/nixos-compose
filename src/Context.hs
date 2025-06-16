module Context where

import StdLib
import System.Directory (getCurrentDirectory)
import System.Environment.XDG.BaseDir qualified as XDG
import System.IO
import System.Process

data Context = Context
  { registerProcess :: ProcessHandle -> IO (),
    stdin :: Handle,
    workingDir :: FilePath,
    storageDir :: FilePath
  }
  deriving stock (Generic)

mkProductionContext :: IO Context
mkProductionContext = do
  workingDir <- getCurrentDirectory
  storageDir <- XDG.getUserDataDir "vmcli"
  pure $
    Context
      { registerProcess = const $ pure (),
        Context.stdin = System.IO.stdin,
        workingDir,
        storageDir
      }

getCacheDir :: IO FilePath
getCacheDir = XDG.getUserDataDir "vmcli"
