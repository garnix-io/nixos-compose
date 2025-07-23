module Utils where

import Control.Exception.Safe (throwIO)
import Cradle
import Cradle.ProcessConfiguration qualified
import Data.String.Conversions (cs)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Debug.Trace qualified
import System.IO (hPrint, stderr)

dbg :: (Show a) => a -> IO ()
dbg = hPrint stderr

trace :: (Show a) => a -> a
trace = Debug.Trace.traceShowId

runWithErrorHandling :: (Output o) => ProcessConfiguration -> IO o
runWithErrorHandling pc = do
  (exitCode, StdoutRaw stdout, StderrRaw stderr, o) <- run pc
  case exitCode of
    ExitSuccess -> pure o
    ExitFailure code -> do
      T.hPutStrLn System.IO.stderr $
        "Command exited with code "
          <> cs (show code)
          <> ": "
          <> cs (Cradle.ProcessConfiguration.executable pc)
          <> " "
          <> T.unwords (cs <$> Cradle.ProcessConfiguration.arguments pc)
          <> "\n"
          <> cs stdout
          <> "\n"
          <> cs stderr
      throwIO exitCode
