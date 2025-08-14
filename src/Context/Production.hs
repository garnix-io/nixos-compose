module Context.Production where

import Context
import Logger (withAutoLogger)
import NixVms qualified
import System.Directory (XdgDirectory (..), getCurrentDirectory, getXdgDirectory)
import System.IO

withContext :: (Context -> IO ()) -> IO ()
withContext action = do
  workingDir <- getCurrentDirectory
  storageDir <- getXdgDirectory XdgState "nixos-compose"
  withAutoLogger $ \logger -> do
    action $
      Context
        { testState = Nothing,
          Context.stdin = System.IO.stdin,
          workingDir,
          storageDir,
          nixVms = NixVms.production,
          logger
        }
