module Vde
  ( startIfNotRunning,
    stop,
  )
where

import Context
import Data.Text.IO qualified as T
import State (VdeState (..), emptyState, getVdeCtlDir, modifyState_)
import StdLib
import System.Directory (doesDirectoryExist, removeDirectoryRecursive)
import System.Posix (sigKILL, signalProcess)
import System.Process

startIfNotRunning :: Context -> IO ()
startIfNotRunning ctx = do
  modifyState_ ctx $ \state -> case state ^. #vde of
    Nothing -> do
      vdeState <- startVde ctx
      pure $ Just $ emptyState & #vde ?~ vdeState
    Just vdeState -> do
      isRunning <- doesDirectoryExist $ "/proc/" <> show (vdeState ^. #pid)
      if isRunning
        then pure $ Just state
        else do
          T.putStrLn "WARN: vde_switch crashed, restarting"
          vdeState <- startVde ctx
          pure $ Just $ state & #vde ?~ vdeState

startVde :: Context -> IO VdeState
startVde ctx = do
  ctlDir <- getVdeCtlDir ctx
  (stdinPipe, _) <- createPipe
  (_, _, _, handle) <-
    createProcess
      (System.Process.proc "vde_switch" ["--sock", ctlDir, "--dirmode", "0700", "--hub"])
        { std_in = UseHandle stdinPipe -- `CreatePipe :: StdStream` doesn't work reliably
        }
  registerProcess ctx VdeSwitch handle
  pid <- getPid handle <&> fromMaybe (error "no pid")
  pure $ VdeState {pid = fromIntegral pid}

stop :: Context -> IO ()
stop ctx = do
  modifyState_ ctx $ \state -> case state ^. #vde of
    Nothing -> pure $ Just state
    Just vdeState -> do
      signalProcess sigKILL $ fromIntegral $ vdeState ^. #pid
      removeDirectoryRecursive =<< getVdeCtlDir ctx
      pure Nothing
