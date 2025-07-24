module NixVms (NixVms (..), production) where

import Context
import Cradle
import Data.Aeson qualified as Aeson
import Data.Map.Strict qualified as Map
import Data.String.Conversions (cs)
import Data.String.Interpolate (i)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Network.Socket.Free (getFreePort)
import State
import StdLib
import System.Directory (createDirectoryIfMissing, listDirectory)
import System.Environment (getEnvironment)
import System.FilePath (takeDirectory, (</>))
import System.IO (IOMode (..), openFile)
import System.IO qualified
import System.Process (CreateProcess (..), ProcessHandle, StdStream (..), createProcess, proc)
import Utils
import Prelude

production :: NixVms
production =
  NixVms
    { listVms = listVmsImpl,
      buildAndRun = buildAndRunImpl,
      sshIntoHost = sshIntoHostImpl
    }

listVmsImpl :: Context -> IO [VmName]
listVmsImpl ctx = do
  Cradle.StdoutRaw json <-
    runWithErrorHandling $
      Cradle.cmd "nix"
        & Cradle.setWorkingDir (workingDir ctx)
        & addArgs
          ( nixStandardFlags
              <> [ "eval",
                   ".#nixosConfigurations",
                   "--json",
                   "--apply",
                   "configs: builtins.attrNames configs" :: Text
                 ]
          )
  case Aeson.eitherDecode' (cs json) of
    Left err -> error err
    Right (parsed :: [Text]) -> pure $ map VmName parsed

buildAndRunImpl :: Context -> VmName -> IO ProcessHandle
buildAndRunImpl ctx vmName = do
  vmExecutable <- logStep "Building NixOS config..." $ do
    moduleExtensions <- getModuleExtensions ctx vmName
    (Cradle.StdoutTrimmed drvPathJson) <-
      runWithErrorHandling $
        Cradle.cmd "nix"
          & Cradle.setWorkingDir (workingDir ctx)
          & Cradle.addArgs
            ( nixStandardFlags
                <> [ "eval",
                     ".#nixosConfigurations." <> toNixString (vmNameToText vmName),
                     "--json",
                     "--apply",
                     "nixConfig: (nixConfig.extendModules { modules = [" <> moduleExtensions <> "]; }).config.system.build.vm.drvPath"
                   ]
            )
    let drvPath :: Text = case Aeson.eitherDecode' $ cs drvPathJson of
          Right t -> t
          Left err -> error err

    (Cradle.StdoutTrimmed outPath) <-
      runWithErrorHandling $
        Cradle.cmd "nix"
          & Cradle.addArgs
            ( nixStandardFlags
                <> [ "build",
                     "--print-out-paths",
                     "--no-link",
                     drvPath <> "^*"
                   ]
            )
          & Cradle.setWorkingDir (workingDir ctx)

    files <- listDirectory (cs outPath </> "bin")
    case files of
      [file] -> pure $ cs outPath </> "bin" </> file
      files -> error $ "expected one vm script: " <> show files

  logStep "Starting VM..." $ do
    storageDir <- getStateDir ctx vmName
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
    pure ph

nixStandardFlags :: [Text]
nixStandardFlags =
  [ "--extra-experimental-features",
    "nix-command flakes"
  ]

logStep :: Text -> IO a -> IO a
logStep log action = do
  T.hPutStrLn System.IO.stderr log
  result <- action
  T.hPutStrLn System.IO.stderr "Done"
  pure result

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

sshIntoHostImpl :: (Cradle.Output o) => Context -> VmName -> [Text] -> IO o
sshIntoHostImpl ctx vmName command = do
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
