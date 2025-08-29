module Commands.Up (up) where

import Context
import Context.Utils
import Control.Exception.Safe (catch, onException, throwIO)
import Cradle
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map qualified as Map
import Data.String.AnsiEscapeCodes.Strip.Text (stripAnsiEscapeCodes)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import GHC.Conc (atomically)
import Ki qualified
import Logging
import Options (AllOrSomeVms (..), Verbosity (..), VmName (..))
import SafeCreatePipe (safeCreatePipe)
import State
import StdLib
import System.Directory (doesFileExist)
import System.IO (BufferMode (..), Handle, IOMode (..), hClose, hSetBuffering, openFile, stderr)
import System.IO.Error (isEOFError)
import System.Process (ProcessHandle, getPid, getProcessExitCode)
import Table (unstyledText)
import Utils

up :: Context -> Verbosity -> AllOrSomeVms -> IO ()
up ctx verbosity upOptions = do
  vmNames <- case upOptions of
    All -> do
      vmNames <- listVms (nixVms ctx) ctx
      case vmNames of
        [] -> abort ctx "No vms are defined. Nothing to do."
        a : r -> pure $ a :| r
    Some vmNames -> pure vmNames

  Ki.scoped $ \scope -> do
    forM_ vmNames $ \vmName -> do
      ip <- getNextIp ctx
      Ki.fork scope $ do
        existing <- claimVm ctx vmName $ Building {ip}
        case existing of
          Left existing ->
            output ctx $
              vmNameToText vmName
                <> ": already "
                <> unstyledText (vmStateToText (Just existing))
          Right () -> do
            (ctx ^. #logger . #setPhase) vmName "building"
            (pid, port) <- removeVmWhenFailing ctx vmName $ do
              vmKeyPath <- getVmFilePath ctx vmName "vmkey"
              exists <- doesFileExist vmKeyPath
              when exists $ do
                impossible ctx $ cs vmKeyPath <> " already exists"
              () <-
                runWithErrorHandling ctx $
                  Cradle.cmd "ssh-keygen"
                    & Cradle.addArgs ["-f", vmKeyPath, "-N", ""]
              (vmScript, port) <-
                terminating ctx verbosity vmName $ \handle -> do
                  buildVmScript (nixVms ctx) ctx handle vmName ip
              State.writeVmState ctx vmName $ Booting {ip}
              (ctx ^. #logger . #setPhase) vmName "booting"
              nonTerminating ctx verbosity vmName $ \handle -> do
                (handle, logFile) <- case handle of
                  Nothing -> do
                    logFile <- getVmFilePath ctx vmName "log.txt"
                    handle <- openFile logFile WriteMode
                    pure (handle, Just logFile)
                  Just handle -> pure (handle, Nothing)
                ph <- (ctx ^. #nixVms . #runVm) ctx handle vmName vmScript
                registerProcess ctx (Vm vmName) ph
                pid <-
                  System.Process.getPid ph
                    >>= maybe (impossible ctx "qemu process has no pid") pure
                waitForVm ctx vmName port ph $ \vmScriptExitCode -> do
                  case logFile of
                    Just logFile -> do
                      hClose handle
                      output <- T.readFile logFile
                      info ctx (T.intercalate "\n" ["VM failed to start:\n", T.stripEnd output])
                      exitWith $ case vmScriptExitCode of
                        ExitSuccess -> ExitFailure 1
                        vmScriptExitCode -> vmScriptExitCode
                    Nothing -> do
                      info ctx "VM failed to start"
                      exitWith $ case vmScriptExitCode of
                        ExitSuccess -> ExitFailure 1
                        vmScriptExitCode -> vmScriptExitCode
                pure (pid, port)
            State.writeVmState ctx vmName (Running {pid, port, ip})
            (ctx ^. #logger . #clearPhase) vmName
    atomically $ Ki.awaitAll scope
    updateVmHostEntries ctx

-- | Logs everything written to the handle according to the given verbosity.
-- The handle can be passed to *terminating* child processes.
-- The handle will be closed after the given action is done and the stream threads will be waited for.
terminating :: Context -> Verbosity -> VmName -> (Maybe Handle -> IO a) -> IO a
terminating ctx verbosity vmName action =
  Ki.scoped $ \scope -> do
    handle <- newVerbosityHandle scope verbosity (logLineForVm ctx vmName)
    x <- action handle
    forM_ handle $ \handle -> do
      System.IO.hClose handle
    atomically $ Ki.awaitAll scope
    return x

-- | Logs everything written to the handle according to the given verbosity.
-- The handle can be passed to *non-terminating* child processes.
-- Once the given action is done, the handle *won't* be closed, but logging will stop.
nonTerminating :: Context -> Verbosity -> VmName -> (Maybe Handle -> IO a) -> IO a
nonTerminating ctx verbosity vmName action =
  Ki.scoped $ \scope -> do
    handle <- newVerbosityHandle scope verbosity (logLineForVm ctx vmName)
    action handle

logLineForVm :: Context -> VmName -> Text -> IO ()
logLineForVm ctx vmName line =
  (ctx ^. #logger . #pushLog)
    System.IO.stderr
    $ line
      & stripAnsiEscapeCodes
      & removeNonPrintableChars
      & ((vmNameToText vmName <> "> ") <>)
  where
    removeNonPrintableChars :: Text -> Text
    removeNonPrintableChars = T.filter (>= ' ')

newVerbosityHandle :: Ki.Scope -> Verbosity -> (Text -> IO ()) -> IO (Maybe Handle)
newVerbosityHandle scope verbosity logLine =
  case verbosity of
    DefaultVerbosity -> pure Nothing
    Verbose -> do
      (readEnd, writeEnd) <- safeCreatePipe
      hSetBuffering writeEnd LineBuffering
      _ <- Ki.fork scope (streamFromHandle readEnd logLine)
      pure $ Just writeEnd
  where
    streamFromHandle :: Handle -> (Text -> IO ()) -> IO ()
    streamFromHandle handle logLine = do
      line <-
        (Just <$> T.hGetLine handle)
          `catch` ( \case
                      e | isEOFError e -> pure Nothing
                      e -> throwIO e
                  )
      case line of
        Nothing -> pure ()
        Just line -> do
          logLine line
          streamFromHandle handle logLine

removeVmWhenFailing :: Context -> VmName -> IO a -> IO a
removeVmWhenFailing ctx vmName action = do
  onException action $ do
    State.removeVm ctx vmName

waitForVm :: Context -> VmName -> Port -> ProcessHandle -> (ExitCode -> IO ()) -> IO ()
waitForVm ctx vmName port ph handleCrash = do
  (StdoutRaw _, StderrRaw _, sshExitCode) <- (ctx ^. #nixVms . #sshIntoVm . to runSshIntoVm) ctx vmName port "true"
  case sshExitCode of
    ExitSuccess -> pure ()
    ExitFailure _ -> do
      vmScriptExitCode <- getProcessExitCode ph
      case vmScriptExitCode of
        Nothing -> waitForVm ctx vmName port ph handleCrash
        Just vmScriptExitCode -> handleCrash vmScriptExitCode

updateVmHostEntries :: Context -> IO ()
updateVmHostEntries ctx = do
  runningVms <- listRunningVms ctx
  forM_ (Map.keys runningVms) $ \targetVmName -> do
    case parseHostname $ vmNameToText targetVmName of
      Nothing -> info ctx $ "WARN: \"" <> vmNameToText targetVmName <> "\" is not a valid hostname. It will not be added to /etc/hosts."
      Just targetHostname -> do
        targetIp <- (^. #ip) <$> readVmState ctx targetVmName
        forM_ (Map.toList runningVms) $ \(updatingVmName, updatingVmState) -> do
          case updatingVmState of
            Building {} -> pure ()
            Booting {} -> pure ()
            Running {port} -> do
              updateVmHostsEntry (nixVms ctx) ctx updatingVmName port targetHostname targetIp
