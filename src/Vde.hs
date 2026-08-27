module Vde where

import Context
import Context.Utils
import Control.Concurrent (threadDelay)
import Control.Exception.Safe (SomeException, catch, throwIO, try)
import Cradle qualified
import Data.Aeson (FromJSON, ToJSON)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Logging
import Net.IPv4 (IPv4)
import Net.IPv4 qualified as IPv4
import Options
import StdLib
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, removeDirectoryRecursive, removeFile)
import System.IO (IOMode (WriteMode), withFile)
import System.IO.Error (isDoesNotExistError)
import System.Posix (sigKILL, signalProcess)
import System.Process
import Text.Read (readMaybe)
import Utils (which)

newtype VdeState = VdeState
  { pid :: ProcessID
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (ToJSON, FromJSON)

tapDeviceName :: Text
tapDeviceName = "nixos-compose0"

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
  pid <-
    System.Process.getPid handle
      >>= maybe (impossible ctx "vde_switch process has no pid") pure
  pure $ VdeState {pid}

stop :: Context -> VdeState -> IO ()
stop ctx state = do
  _ :: Either SomeException () <- try $ signalProcess sigKILL $ state ^. #pid
  removeDirectoryRecursive =<< getVdeCtlDir ctx

getVdeCtlDir :: Context -> IO FilePath
getVdeCtlDir ctx = do
  let ctlDir = storageDir ctx </> "vde_switch.ctl"
  createDirectoryIfMissing True ctlDir
  pure ctlDir

-- We have some golden tests for the issued commands (in `TapSpec`), but when
-- changing the sudo process invocations, you should manually make sure that
-- setting up a `tap` device still works. With e.g.:
--
-- - nix shell .#default
-- - nixos-compose up test-vm
-- - nixos-compose tap
-- - curl $(nixos-compose ip test-vm)
-- - nixos-compose down test-vm
setupTapDevice :: Context -> DryRunFlag -> IPv4 -> IO ()
setupTapDevice ctx dryRunFlag ipAddress = do
  tapIsRunning <- vde_plug2tapReadPidFile ctx
  case tapIsRunning of
    Just _ -> output ctx "tap device already running"
    Nothing -> do
      vdeCtlDir <- getVdeCtlDir ctx
      vde_plug2tapPath <-
        which "vde_plug2tap"
          <&> fromMaybe (error "impossible: vde_plug2tap not in path")
      ipPath <-
        which "ip"
          <&> fromMaybe (error "impossible: ip not in path")
      -- vde_plug2tap must stay in the foreground. In particular, run0 tears
      -- down the transient service (and all remaining processes in it) as
      -- soon as a command using --daemon returns.
      let tapCommand =
            [ cs vde_plug2tapPath,
              "--pidfile",
              cs (vde_plug2tapPidFile ctx),
              "--sock",
              cs vdeCtlDir,
              tapDeviceName
            ]
          configureCommands =
            [ [cs ipPath, "addr", "add", IPv4.encode ipAddress <> "/24", "dev", tapDeviceName],
              [cs ipPath, "link", "set", tapDeviceName, "up"]
            ]
          displayedCommands = (tapCommand <> ["&"]) : configureCommands
      sudo <- which "sudo"
      case sudo of
        Nothing -> do
          info
            ctx
            ( T.intercalate
                "\n"
                [ "`sudo` not found in the $PATH, cannot create `tap` device.",
                  "You can run the following commands with elevated privileges to create it manually:",
                  ""
                ]
            )
          output ctx (T.intercalate "\n" $ fmap T.unwords displayedCommands)
          exitWith $ ExitFailure 1
        Just sudoPath -> case dryRunFlag of
          DryRun -> do
            info
              ctx
              ( T.intercalate
                  "\n"
                  [ "Would run the following commands:",
                    ""
                  ]
              )
            output ctx (T.intercalate "\n" $ fmap T.unwords displayedCommands)
            exitSuccess
          NoDryRun -> do
            removeStaleTapPidFile ctx
            tapProcess <- startTapProcess ctx sudoPath tapCommand
            waitForTapDevice ctx tapProcess ipPath
            forM_ configureCommands runWithSudo

stopTapDevice :: Context -> DryRunFlag -> Pid -> IO ()
stopTapDevice ctx dryRunFlag pid =
  case dryRunFlag of
    NoDryRun -> runWithSudo ["kill", "-15", cs (show pid) :: Text]
    DryRun -> do
      info
        ctx
        ( T.intercalate
            "\n"
            [ "Would run the following commands:",
              ""
            ]
        )
      output ctx (T.unwords ["kill", "-15", cs (show pid)])
      exitSuccess

vde_plug2tapPidFile :: Context -> FilePath
vde_plug2tapPidFile ctx = storageDir ctx </> "vde_plug2tap.pid"

vde_plug2tapLogFile :: Context -> FilePath
vde_plug2tapLogFile ctx = storageDir ctx </> "vde_plug2tap.log"

vde_plug2tapReadPidFile :: Context -> IO (Maybe Pid)
vde_plug2tapReadPidFile ctx = do
  content <-
    (Just <$> readFile (vde_plug2tapPidFile ctx))
      `catch` (\e -> if isDoesNotExistError e then pure Nothing else throwIO e)
  case content >>= (readMaybe :: String -> Maybe Pid) of
    Nothing -> pure Nothing
    Just pid -> do
      tapIsRunning <- doesDirectoryExist $ "/proc/" <> show (pid :: ProcessID)
      pure $ if tapIsRunning then Just pid else Nothing

removeStaleTapPidFile :: Context -> IO ()
removeStaleTapPidFile ctx =
  removeFile (vde_plug2tapPidFile ctx)
    `catch` (\e -> if isDoesNotExistError e then pure () else throwIO e)

startTapProcess :: Context -> FilePath -> [Text] -> IO ProcessHandle
startTapProcess ctx sudoPath args =
  withFile (vde_plug2tapLogFile ctx) WriteMode $ \logHandle -> do
    (_, _, _, handle) <-
      createProcess
        (System.Process.proc sudoPath (cs <$> args))
          { std_in = NoStream,
            std_out = UseHandle logHandle,
            std_err = UseHandle logHandle
          }
    registerProcess ctx VdePlug2Tap handle
    pure handle

waitForTapDevice :: Context -> ProcessHandle -> FilePath -> IO ()
waitForTapDevice ctx tapProcess ipPath = waitForPidFile
  where
    waitForPidFile = do
      ensureTapProcessRunning ctx tapProcess
      tapPid <- vde_plug2tapReadPidFile ctx
      case tapPid of
        Nothing -> do
          threadDelay 50_000
          waitForPidFile
        Just _ -> waitForInterface 100

    waitForInterface :: Int -> IO ()
    waitForInterface retries = do
      ensureTapProcessRunning ctx tapProcess
      isReady <- tapDeviceExists ipPath
      unless isReady $
        if retries > 0
          then do
            threadDelay 50_000
            waitForInterface (retries - 1)
          else do
            _ :: Either SomeException () <- try $ terminateProcess tapProcess
            tapStartupFailed ctx $ "the " <> tapDeviceName <> " interface did not appear"

tapDeviceExists :: FilePath -> IO Bool
tapDeviceExists ipPath = do
  (_stdout, _stderr, exitCode) :: (Cradle.StdoutRaw, Cradle.StderrRaw, ExitCode) <-
    Cradle.run $
      Cradle.cmd ipPath
        & Cradle.addArgs ["link", "show", "dev", tapDeviceName]
  pure $ exitCode == ExitSuccess

ensureTapProcessRunning :: Context -> ProcessHandle -> IO ()
ensureTapProcessRunning ctx tapProcess = do
  getProcessExitCode tapProcess >>= \case
    Nothing -> pure ()
    Just exitCode ->
      tapStartupFailed
        ctx
        ("the privileged vde_plug2tap process exited with " <> cs (show exitCode))

tapStartupFailed :: Context -> Text -> IO a
tapStartupFailed ctx reason = do
  logOutput <-
    TIO.readFile (vde_plug2tapLogFile ctx)
      `catch` (\(_ :: SomeException) -> pure "")
  abort ctx $
    T.unlines $
      ["failed to start tap device: " <> reason]
        <> if T.null (T.strip logOutput)
          then []
          else ["vde_plug2tap output:", T.stripEnd logOutput]

runWithSudo :: [Text] -> IO ()
runWithSudo args =
  Cradle.run_ $
    Cradle.cmd "sudo"
      & Cradle.addArgs args
