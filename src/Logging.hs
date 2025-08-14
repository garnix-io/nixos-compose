module Logging where

import Context (Context)
import Data.Text qualified as T
import GHC.Stack (HasCallStack, callStack, prettyCallStack)
import StdLib
import Context.Utils

-- | Prints the message to `stderr` and exits with exit code 1.
-- This is our standard way of aborting the program on error conditions.
abort :: Context -> Text -> IO a
abort ctx message = do
  info ctx $ T.stripEnd message
  exitWith $ ExitFailure 1

impossible :: (HasCallStack) => Context -> Text -> IO a
impossible ctx message = do
  abort
    ctx
    ( T.unlines
        [ "nixos-compose encountered an unexpected error: " <> message,
          "Please, consider reporting this as a bug here: https://github.com/garnix-io/nixos-compose/issues",
          "",
          "callstack:",
          cs (prettyCallStack callStack)
        ]
    )
