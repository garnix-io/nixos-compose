module Context.Production where

import Context
import NixVms qualified
import System.Directory (getCurrentDirectory)
import System.Environment.XDG.BaseDir qualified as XDG
import System.IO

mkContext :: IO Context
mkContext = do
  workingDir <- getCurrentDirectory
  storageDir <- XDG.getUserDataDir "vmcli"
  pure $
    Context
      { registerProcess = const $ pure (),
        Context.stdin = System.IO.stdin,
        workingDir,
        storageDir,
        nixVms = NixVms.production
      }
