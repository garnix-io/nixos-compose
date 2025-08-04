module Vde where

import Context
import Control.Exception.Safe (SomeException, try)
import Data.Aeson (FromJSON, ToJSON)
import StdLib
import System.Directory (createDirectoryIfMissing, removeDirectoryRecursive)
import System.Posix (sigKILL, signalProcess)
import System.Process

newtype VdeState = VdeState
  { pid :: ProcessID
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (ToJSON, FromJSON)

start :: Context -> IO VdeState
start ctx = do
  ctlDir <- getVdeCtlDir ctx
  (stdinPipe, _) <- createPipe
  (_, _, _, handle) <-
    createProcess
      (System.Process.proc "vde_switch" ["--sock", ctlDir, "--dirmode", "0700", "--hub"])
        { std_in = UseHandle stdinPipe -- `CreatePipe :: StdStream` doesn't work reliably
        }
  registerProcess ctx VdeSwitch handle
  pid <- System.Process.getPid handle <&> fromMaybe (error "no pid")
  pure $ VdeState {pid}

stop :: Context -> VdeState -> IO ()
stop ctx state = do
  _ :: Either SomeException () <- try $ signalProcess sigKILL $ state ^. #pid
  removeDirectoryRecursive =<< getVdeCtlDir ctx

getVdeCtlDir :: Context -> IO FilePath
getVdeCtlDir ctx = do
  let ctlDir = storageDir ctx </> "vde1.ctl"
  createDirectoryIfMissing True ctlDir
  pure ctlDir
