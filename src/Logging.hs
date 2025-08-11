module Logging where

import Data.Text qualified as T
import Data.Text.IO qualified as T
import GHC.Stack (HasCallStack, callStack, prettyCallStack)
import StdLib
import System.IO (stderr)

-- | Prints the message to `stderr` and exits with exit code 1.
-- This is our standard way of aborting the program on error conditions.
abort :: Text -> IO a
abort message = do
  T.hPutStr stderr $ if "\n" `T.isSuffixOf` message then message else message <> "\n"
  exitWith $ ExitFailure 1

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
