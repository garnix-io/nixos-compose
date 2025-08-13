module Context.Utils (runWithErrorHandling, info, output) where

import Context
import Cradle
import Cradle.ProcessConfiguration qualified
import Data.Text qualified as T
import StdLib
import System.IO qualified

runWithErrorHandling :: (Cradle.Output o) => Context -> ProcessConfiguration -> IO o
runWithErrorHandling ctx pc = do
  (exitCode, StdoutRaw stdout, StderrRaw stderr, o) <- run pc
  case exitCode of
    ExitSuccess -> pure o
    ExitFailure code -> do
      info
        ctx
        ( "Command exited with code "
            <> cs (show code)
            <> ": "
            <> cs (Cradle.ProcessConfiguration.executable pc)
            <> " "
            <> T.unwords (cs <$> Cradle.ProcessConfiguration.arguments pc)
            <> "\n"
            <> cs stdout
            <> "\n"
            <> cs stderr
        )
      exitWith exitCode

info :: Context -> Text -> IO ()
info ctx = (ctx ^. #logger . #pushLog) System.IO.stderr

output :: Context -> Text -> IO ()
output ctx = (ctx ^. #logger . #pushLog) System.IO.stdout
