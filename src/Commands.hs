module Commands (start, stop, ssh, status) where

import Context
import Control.Concurrent (threadDelay)
import Control.Exception
import Control.Monad (forM_, when)
import Cradle
import Cradle.ProcessConfiguration qualified
import Data.Aeson qualified as Aeson
import Data.List.NonEmpty (NonEmpty)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.String.Conversions (cs)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Network.Socket.Free (getFreePort)
import State
import StdLib
import System.Directory (createDirectoryIfMissing, doesFileExist, listDirectory)
import System.Environment (getEnvironment)
import System.FilePath (takeDirectory, (</>))
import System.IO (IOMode (..), openFile)
import System.IO qualified
import System.Posix (sigKILL, signalProcess)
import System.Process (CreateProcess (..), StdStream (..), createProcess, getPid, proc)
import Prelude

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

logStep :: Text -> IO a -> IO a
logStep log action = do
  T.hPutStrLn System.IO.stderr log
  result <- action
  T.hPutStrLn System.IO.stderr "Done"
  pure result

start :: Context -> NonEmpty VmName -> IO ()
start ctx vmNames = forM_ vmNames $ \vmName -> do
  alreadyRunning <- listRunningVms ctx
  if vmName `elem` alreadyRunning
    then do
      T.putStrLn $ vmNameToText vmName <> ": already running"
    else do
      storageDir <- getStateDir ctx vmName
      vmKeyPath <- getStateFile ctx vmName "vmkey"
      exists <- doesFileExist vmKeyPath
      when exists $ do
        error $ vmKeyPath <> " already exists"
      () <-
        runWithErrorHandling $
          -- todo: make runtime dep
          Cradle.cmd "ssh-keygen"
            & Cradle.addArgs ["-f", vmKeyPath, "-N", ""]

      vmExecutable <- logStep "Building NixOS config..." $ do
        moduleExtensions <- getModuleExtensions ctx vmName
        (Cradle.StdoutTrimmed drvPathJson) <-
          runWithErrorHandling $
            Cradle.cmd "nix"
              & Cradle.setWorkingDir (workingDir ctx)
              & Cradle.addArgs
                [ "--extra-experimental-features",
                  "nix-command flakes",
                  "eval",
                  ".#nixosConfigurations." <> toNixString (vmNameToText vmName),
                  "--json",
                  "--apply",
                  "nixConfig: (nixConfig.extendModules { modules = [" <> moduleExtensions <> "]; }).config.system.build.vm.drvPath"
                ]
        let drvPath :: Text = case Aeson.eitherDecode' $ cs drvPathJson of
              Right t -> t
              Left err -> error err

        (Cradle.StdoutTrimmed outPath) <-
          runWithErrorHandling $
            Cradle.cmd "nix"
              & Cradle.addArgs
                [ "--extra-experimental-features",
                  "nix-command",
                  "build",
                  "--print-out-paths",
                  "--no-link",
                  drvPath <> "^*"
                ]
              & Cradle.setWorkingDir (workingDir ctx)

        files <- listDirectory (cs outPath </> "bin")
        case files of
          [file] -> pure $ cs outPath </> "bin" </> file
          files -> error $ "expected one vm script: " <> show files

      logStep "Starting VM..." $ do
        let nixDiskImage = storageDir </> cs (vmNameToText vmName) </> "image.qcow2"
        createDirectoryIfMissing True (takeDirectory nixDiskImage)
        stdoutHandle <- openFile (storageDir </> "./stdout.log") WriteMode
        stderrHandle <- openFile (storageDir </> "./stderr.log") WriteMode
        (_, _, _, ph) <- do
          parentEnvironment <-
            getEnvironment
              <&> Map.fromList
          createProcess
            (proc vmExecutable [])
              { env = Just $ Map.toList $ Map.insert "NIX_DISK_IMAGE" nixDiskImage parentEnvironment,
                std_out = UseHandle stdoutHandle,
                std_err = UseHandle stderrHandle
              }
        registerProcess ctx ph
        pid <- getPid ph <&> fromMaybe (error "no pid")
        state <- readState ctx vmName
        writeState ctx vmName (state {pid = Just $ fromIntegral pid})
        waitForVm ctx vmName

getModuleExtensions :: Context -> VmName -> IO Text
getModuleExtensions ctx vmName = do
  publicKey <- readFile =<< getStateFile ctx vmName "vmkey.pub"
  port <- getFreePort
  State.writeState ctx vmName (VmState {pid = Nothing, port = port})
  pure $
    cs
      [i|
        {
          services.openssh.enable = true;
          users.users.vmuser = {
            isNormalUser = true;
            group = "wheel";
            openssh.authorizedKeys.keys = [ #{toNixString $ cs publicKey} ];
          };
          virtualisation.vmVariant.virtualisation = {
            graphics = false;
            forwardPorts = [{ from = "host"; host.port = #{port}; guest.port = 22; }];
          };
        }
      |]

toNixString :: Text -> Text
toNixString s = "\"" <> T.concatMap escapeChar (cs s) <> "\""
  where
    escapeChar c = case c of
      '"' -> "\\\""
      '$' -> "\\$"
      '\\' -> "\\\\"
      c -> T.singleton c

stop :: Context -> VmName -> IO ()
stop ctx vmName = do
  state <- readState ctx vmName
  case state ^. #pid of
    Just pid -> signalProcess sigKILL $ fromIntegral pid
    Nothing -> error "pid missing from state file"
  removeState ctx vmName

sshIntoHost :: (Cradle.Output o) => Context -> VmName -> [Text] -> IO o
sshIntoHost ctx vmName command = do
  vmKeyPath <- getStateFile ctx vmName "vmkey"
  port <- State.readState ctx vmName <&> (^. #port)
  Cradle.run $
    Cradle.cmd "ssh"
      & Cradle.setStdinHandle (ctx ^. #stdin)
      & Cradle.addArgs
        ( [ "-i",
            cs vmKeyPath,
            "-l",
            "vmuser",
            "-o",
            "StrictHostKeyChecking=no",
            "-o",
            "UserKnownHostsFile=/dev/null",
            "-o",
            "ConnectTimeout=2",
            "-p",
            cs (show port),
            "localhost"
          ]
            <> command
        )

waitForVm :: Context -> VmName -> IO ()
waitForVm ctx vmName = do
  (StdoutRaw _, StderrRaw _, exitCode) <- sshIntoHost ctx vmName ["true"]
  when (exitCode /= Cradle.ExitSuccess) $ do
    threadDelay 1_000_000
    waitForVm ctx vmName

ssh :: Context -> VmName -> [Text] -> IO ()
ssh ctx vmName command = do
  sshIntoHost ctx vmName command

status :: Context -> VmName -> IO ()
status ctx vmName = do
  runningVms <- State.listRunningVms ctx
  T.putStrLn $ case runningVms of
    vms | vmName `elem` vms -> vmNameToText vmName <> ": running"
    [] -> "no vms running"
    _ -> vmNameToText vmName <> ": not running"
