module Utils
  ( dbg,
    trace,
    filterMapM,
    Port,
    Hostname,
    parseHostname,
    hostnameToText,
    which,
    withLineHandler,
  )
where

import Control.Exception.Safe (catch, throwIO)
import Control.Monad (filterM)
import Cradle
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Debug.Trace qualified
import GHC.Conc (atomically)
import Ki qualified
import SafeCreatePipe (safeCreatePipe)
import StdLib
import System.IO (Handle, hClose, hPrint, stderr)
import System.IO.Error (isEOFError)

dbg :: (Show a) => a -> IO ()
dbg = hPrint stderr

trace :: (Show a) => a -> a
trace = Debug.Trace.traceShowId

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

withLineHandler :: (Text -> IO ()) -> ((Handle, Handle) -> IO a) -> IO a
withLineHandler printLine action = do
  Ki.scoped $ \scope -> do
    (readEndStdout, writeEndStdout) <- safeCreatePipe
    (readEndStderr, writeEndStderr) <- safeCreatePipe
    _ <- Ki.fork scope (streamFromHandle readEndStdout printLine)
    _ <- Ki.fork scope (streamFromHandle readEndStderr printLine)
    r <- action (writeEndStdout, writeEndStderr)
    mapM_ hClose [writeEndStdout, writeEndStderr]
    atomically $ Ki.awaitAll scope
    pure r
  where
    streamFromHandle :: Handle -> (Text -> IO ()) -> IO ()
    streamFromHandle input logLine = do
      line <-
        (Just <$> T.hGetLine input)
          `catch` ( \case
                      e | isEOFError e -> pure Nothing
                      e -> throwIO e
                  )
      case line of
        Nothing -> pure ()
        Just line -> do
          logLine line
          streamFromHandle input logLine
