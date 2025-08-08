module Utils
  ( dbg,
    trace,
    runWithErrorHandling,
    filterMapM,
    Port,
    Hostname,
    parseHostname,
    hostnameToText,
    which,
  )
where

import Control.Monad (filterM)
import Cradle
import Cradle.ProcessConfiguration qualified
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text qualified as T
import Debug.Trace qualified
import Logging
import StdLib
import System.IO (hPrint, stderr)

dbg :: (Show a) => a -> IO ()
dbg = hPrint stderr

trace :: (Show a) => a -> a
trace = Debug.Trace.traceShowId

runWithErrorHandling :: (Cradle.Output o) => ProcessConfiguration -> IO o
runWithErrorHandling pc = do
  (exitCode, StdoutRaw stdout, StderrRaw stderr, o) <- run pc
  case exitCode of
    ExitSuccess -> pure o
    ExitFailure code -> do
      exitWith
        [ ToStderr
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
        ]
        exitCode

filterMapM :: (Monad m, Ord k) => (k -> v -> m Bool) -> Map k v -> m (Map k v)
filterMapM pred map = do
  new <- filterM (uncurry pred) $ Map.toList map
  pure $ Map.fromList new

type Port = Int

newtype Hostname = Hostname {hostnameToText :: Text}
  deriving newtype (Show, Eq, Ord)

parseHostname :: Text -> Maybe Hostname
parseHostname t =
  if t /= ""
    && T.all (`elem` allValid) t
    && (`elem` alpha) (T.head t)
    && (`elem` alphaNumeric) (T.last t)
    && not ("--" `T.isInfixOf` t)
    then Just $ Hostname t
    else Nothing
  where
    alpha = ['a' .. 'z'] <> ['A' .. 'Z']
    numeric = ['0' .. '9']
    alphaNumeric = alpha <> numeric
    allValid = alphaNumeric <> ['.', '-']

which :: FilePath -> IO (Maybe FilePath)
which executable = do
  (exitCode, StdoutTrimmed path) <-
    run $
      cmd "which"
        & addArgs [executable]
        & silenceStderr
  pure $ case exitCode of
    ExitSuccess -> Just $ cs path
    ExitFailure _ -> Nothing
