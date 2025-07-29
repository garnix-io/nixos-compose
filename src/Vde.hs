module Vde
  ( startIfNotRunning,
    stop,
  )
where

import Context
import Data.Text.IO qualified as T
import State (VdeState (..), getVdeCtlDir, modifyVdeState)
import StdLib
import System.Directory (doesDirectoryExist, removeDirectoryRecursive)
import System.Posix (sigKILL, signalProcess)
import System.Process

startIfNotRunning :: Context -> IO ()
startIfNotRunning ctx = do
  modifyVdeState ctx $ \case
    Nothing -> Just <$> startVde ctx
    Just state -> do
      isRunning <- doesDirectoryExist $ "/proc/" <> show (state ^. #pid)
      if isRunning
        then pure $ Just state
        else do
          T.putStrLn "WARN: vde_switch crashed, restarting"
          Just <$> startVde ctx

startVde :: Context -> IO VdeState
startVde ctx = do
  ctlDir <- getVdeCtlDir ctx
  (stdinPipe, _) <- createPipe
  (_, _, _, handle) <-
    createProcess
      (System.Process.proc "vde_switch" ["--sock", ctlDir, "--dirmode", "0700", "--hub"])
        { std_in = UseHandle stdinPipe -- `CreatePipe :: StdStream` doesn't work reliably
        }
  registerProcess ctx handle
  pid <- getPid handle <&> fromMaybe (error "no pid")
  pure $ VdeState {pid = fromIntegral pid}

stop :: Context -> IO ()
stop ctx = do
  modifyVdeState ctx $ \case
    Nothing -> pure Nothing
    Just state -> do
      signalProcess sigKILL $ fromIntegral $ state ^. #pid
      removeDirectoryRecursive =<< getVdeCtlDir ctx
      pure Nothing
