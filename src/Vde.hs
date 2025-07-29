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
    Just state -> do
      isRunning <- doesDirectoryExist $ "/proc/" <> show (state ^. #pid)
      unless isRunning $ do
        T.putStrLn "WARN: vde_switch crashed"
      pure $ Just state

stop :: Context -> IO ()
stop ctx = do
  modifyVdeState ctx $ \case
    Nothing -> pure Nothing
    Just state -> do
      signalProcess sigKILL $ fromIntegral $ state ^. #pid
      removeDirectoryRecursive =<< getVdeCtlDir ctx
      pure Nothing
