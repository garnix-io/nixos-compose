module Vde where

import Context
import Context.Utils
import Control.Exception.Safe (SomeException, catch, throwIO, try)
import Cradle qualified
import Data.Aeson (FromJSON, ToJSON)
import Data.Text qualified as T
import Logging
import Net.IPv4 (IPv4)
import Net.IPv4 qualified as IPv4
import Options
import StdLib
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, removeDirectoryRecursive)
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
      commands <- do
        vdeCtlDir <- getVdeCtlDir ctx
        vde_plug2tapPath <-
          which "vde_plug2tap"
            <&> fromMaybe (error "impossible: vde_plug2tap not in path")
        ipPath <-
          which "ip"
            <&> fromMaybe (error "impossible: ip not in path")
        pure
          [ [cs vde_plug2tapPath, "--daemon", "--pidfile", cs (vde_plug2tapPidFile ctx), "--sock", cs vdeCtlDir, "nixos-compose0"],
            [cs ipPath, "addr", "add", IPv4.encode ipAddress <> "/24", "dev", "nixos-compose0" :: Text],
            [cs ipPath, "link", "set", "nixos-compose0", "up" :: Text]
          ]
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
          output ctx (T.intercalate "\n" $ fmap T.unwords commands)
          exitWith $ ExitFailure 1
        Just _ -> case dryRunFlag of
          DryRun -> do
            info
              ctx
              ( T.intercalate
                  "\n"
                  [ "Would run the following commands:",
                    ""
                  ]
              )
            output ctx (T.intercalate "\n" $ fmap T.unwords commands)
            exitSuccess
          NoDryRun -> forM_ commands runWithSudo

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
vde_plug2tapPidFile ctx = do
  storageDir ctx </> "vde_plug2tap.pid"

vde_plug2tapReadPidFile :: Context -> IO (Maybe Pid)
vde_plug2tapReadPidFile ctx = do
  content <-
    (Just <$> readFile (vde_plug2tapPidFile ctx))
      `catch` (\e -> if isDoesNotExistError e then pure Nothing else throwIO e)
  case content of
    Nothing -> pure Nothing
    Just content ->
      case readMaybe content :: Maybe Pid of
        Nothing -> impossible ctx ("pidFile doesn't contain number: " <> cs (show content))
        Just pid -> do
          tapIsRunning <- doesDirectoryExist $ "/proc/" <> show (pid :: ProcessID)
          if tapIsRunning
            then pure $ Just pid
            else pure Nothing

runWithSudo :: [Text] -> IO ()
runWithSudo args =
  Cradle.run_ $
    Cradle.cmd "sudo"
      & Cradle.addArgs args
