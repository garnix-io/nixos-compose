module Context.Production where

import Context
import NixVms qualified
import System.Directory (XdgDirectory (..), getCurrentDirectory, getXdgDirectory)
import System.IO

mkContext :: IO Context
mkContext = do
  workingDir <- getCurrentDirectory
  storageDir <- getXdgDirectory XdgData "nixos-compose"
  pure $
    Context
      { testState = Nothing,
        Context.stdin = System.IO.stdin,
        workingDir,
        storageDir,
        nixVms = NixVms.production
      }
