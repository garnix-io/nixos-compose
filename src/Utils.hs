module Utils where

import Control.Monad (filterM)
import Cradle
import Cradle.ProcessConfiguration qualified
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Debug.Trace qualified
import StdLib
import System.IO (hPrint, stderr)

dbg :: (Show a) => a -> IO ()
dbg = hPrint stderr

trace :: (Show a) => a -> a
trace = Debug.Trace.traceShowId

logStep :: Text -> IO a -> IO a
logStep log action = do
  T.hPutStrLn System.IO.stderr log
  result <- action
  T.hPutStrLn System.IO.stderr "Done"
  pure result

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

filterMapM :: (Monad m, Ord k) => (k -> v -> m Bool) -> Map k v -> m (Map k v)
filterMapM pred map = do
  new <- filterM (uncurry pred) $ Map.toList map
  pure $ Map.fromList new

type Port = Int

isValidHostname :: Text -> Bool
isValidHostname t =
  t /= ""
    && T.all (`elem` allValid) t
    && (`elem` alpha) (T.head t)
    && (`elem` alphaNumeric) (T.last t)
    && not ("--" `T.isInfixOf` t)
  where
    alpha = ['a' .. 'z'] <> ['A' .. 'Z']
    numeric = ['0' .. '9']
    alphaNumeric = alpha <> numeric
    allValid = alphaNumeric <> ['.', '-']
