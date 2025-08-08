module Logging where

import Control.Exception.Safe (throwIO)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import GHC.Stack (HasCallStack, callStack, prettyCallStack)
import StdLib
import System.IO (stderr, stdout)

-- | Prints the message to `stderr` and exits with exit code 1.
-- This is our standard way of aborting the program on error conditions.
abort :: Text -> IO a
abort message = do
  exitWith [ToStderr message] $ ExitFailure 1

data Output
  = ToStdout {message :: Text}
  | ToStderr {message :: Text}
  deriving stock (Generic)

exitWith :: [Output] -> ExitCode -> IO a
exitWith outputs exitCode = do
  forM_ outputs $ \output -> do
    let handle = case output of
          ToStdout _ -> System.IO.stdout
          ToStderr _ -> System.IO.stderr
    let message = output ^. #message
    let line =
          if "\n" `T.isSuffixOf` message
            then message
            else message <> "\n"
    T.hPutStr handle line
  throwIO exitCode

impossible :: (HasCallStack) => Text -> IO a
impossible message = do
  abort
    ( T.unlines
        [ "nixos-compose encountered an unexpected error: " <> message,
          "Please, consider reporting this as a bug here: https://github.com/garnix-io/nixos-compose/issues",
          "",
          "callstack:",
          cs (prettyCallStack callStack)
        ]
    )
