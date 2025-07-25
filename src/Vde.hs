module Vde
  ( startIfNotRunning,
    stop,
  )
where

import Context
import State (VdeState (..), getVdeCtlDir, modifyVdeState)
import StdLib
import System.Directory (removeDirectoryRecursive)
import System.Posix (sigKILL, signalProcess)
import System.Process

startIfNotRunning :: Context -> IO ()
startIfNotRunning ctx = do
  modifyVdeState ctx $ \case
    Nothing -> do
      ctlDir <- getVdeCtlDir ctx
      (stdinPipe, _) <- createPipe
      (_, _, _, handle) <-
        createProcess
          (System.Process.proc "vde_switch" ["--sock", ctlDir, "--dirmode", "0700", "--hub"])
            { std_in = UseHandle stdinPipe -- `CreatePipe :: StdStream` doesn't work reliably
            }
      registerProcess ctx handle
      pid <- getPid handle <&> fromMaybe (error "no pid")
      pure $ Just $ VdeState {pid = fromIntegral pid}
    Just x -> do
      -- todo: make sure it's still running?
      pure $ Just x

stop :: Context -> IO ()
stop ctx = do
  modifyVdeState ctx $ \case
    Nothing -> pure Nothing
    Just state -> do
      signalProcess sigKILL $ fromIntegral $ state ^. #pid
      removeDirectoryRecursive =<< getVdeCtlDir ctx
      pure Nothing
