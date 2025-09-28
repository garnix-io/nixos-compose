module NixVms (NixVms (..), production) where

import Context
import Context.Utils (runWithErrorHandling)
import Cradle
import Data.Aeson qualified as Aeson
import Data.Map.Strict qualified as Map
import Data.String.Interpolate (i)
import Data.Text qualified as T
import Logging
import Net.IPv4 (IPv4)
import Net.IPv4 qualified as IPv4
import Network.Socket.Free (getFreePort)
import Options (VmName (..))
import State
import StdLib
import System.Info (os)
import System.Directory (createDirectoryIfMissing, listDirectory)
import System.Environment (getEnvironment)
import System.FilePath (takeDirectory)
import System.IO (Handle)
import System.Process (CreateProcess (..), ProcessHandle, StdStream (..), createProcess, proc)
import Utils
import Vde qualified
import Prelude

production :: NixVms
production =
  NixVms
    { listVms = listVmsImpl,
      buildVmScript = buildVmScriptImpl,
      runVm = runVmImpl,
      sshIntoVm = SshIntoVm sshIntoVmImpl,
      updateVmHostsEntry = updateVmHostsEntryImpl
    }

listVmsImpl :: Context -> IO [VmName]
listVmsImpl ctx = do
  Cradle.StdoutRaw json <-
    runWithErrorHandling ctx $
      Cradle.cmd "nix"
        & Cradle.setWorkingDir (workingDir ctx)
        & addArgs
          ( nixStandardFlags
              <> [ "eval",
                   ".#.",
                   "--json",
                   "--apply",
                   "outputs: builtins.attrNames (outputs.nixosConfigurations or {})"
                 ]
          )
  case Aeson.eitherDecode' (cs json) of
    Left err -> impossible ctx $ cs err
    Right (parsed :: [Text]) -> pure $ map VmName parsed

darwinBuilderArgs :: [Text]
darwinBuilderArgs = if os == "darwin" then ["--builders", "/etc/nix/builder_ed25519"] else [""]

buildVmScriptImpl :: Context -> Maybe Handle -> VmName -> IPv4 -> IO (FilePath, Port)
buildVmScriptImpl ctx handle vmName ip = do
  port <- getFreePort
  moduleExtensions <- getModuleExtensions ctx vmName port ip
  (Cradle.StdoutTrimmed drvPathJson) <-
    runWithErrorHandling ctx $
      Cradle.cmd "nix"
        & Cradle.setWorkingDir (workingDir ctx)
        & Cradle.addArgs
          ( nixStandardFlags
              <> [ "eval",
                   ".#nixosConfigurations." <> toNixString (vmNameToText vmName),
                   "--json",
                   "--apply",
                   "nixConfig: (nixConfig.extendModules { modules = [(" <> moduleExtensions <> ")]; }).config.system.build.vm.drvPath"
                 ]
          )
        & maybe id addStderrHandle handle
  drvPath :: Text <- case Aeson.eitherDecode' $ cs drvPathJson of
    Right t -> pure t
    Left err -> impossible ctx $ cs err

  (Cradle.StdoutTrimmed outPath) <-
    runWithErrorHandling ctx $
      Cradle.cmd "nix"
        & Cradle.addArgs
          ( nixStandardFlags
              <> [ "build",
                   "--print-out-paths",
                   "--no-link",
                   drvPath <> "^*"
                 ]
                 <> darwinBuilderArgs
          )
        & Cradle.setWorkingDir (workingDir ctx)
        & maybe id addStderrHandle handle

  files <- listDirectory (cs outPath </> "bin")
  case files of
    [file] -> pure (cs outPath </> "bin" </> file, port)
    files -> impossible ctx $ "expected one vm script: " <> cs (show files)

nixStandardFlags :: [Text]
nixStandardFlags =
  [ "--extra-experimental-features",
    "nix-command flakes"
  ]

getModuleExtensions :: Context -> VmName -> Port -> IPv4 -> IO Text
getModuleExtensions ctx vmName port ip = do
  publicKey <- readFile =<< getVmFilePath ctx vmName "vmkey.pub"
  pure $
    cs
      [i|
        { pkgs, ... }: {
          console.enable = false;
          services.openssh.enable = true;
          users.users.vmuser = {
            isNormalUser = true;
            group = "wheel";
            openssh.authorizedKeys.keys = [ #{toNixString $ cs publicKey} ];
            packages = [
              (pkgs.writeShellApplication {
                name = "update-vm-hosts-entry";
                text = ''
                  HOSTNAME="$1"
                  IP="$2"
                  sudo sed -i "/ $HOSTNAME\$/d" /etc/hosts
                  sudo tee --append /etc/hosts <<< "$IP $HOSTNAME" > /dev/null
                '';
              })
            ];
          };
          security.sudo.extraRules = [
            { users = [ "vmuser" ]; commands = [ { command = "ALL"; options = [ "NOPASSWD" ]; } ]; }
          ];
          virtualisation.vmVariant.virtualisation = {
            graphics = false;
            forwardPorts = [{ from = "host"; host.port = #{port}; guest.port = 22; }];
          };
          networking.interfaces.eth1.ipv4.addresses = [{
            address = "#{IPv4.encode ip :: Text}";
            prefixLength = 24;
          }];
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

runVmImpl :: Context -> Handle -> VmName -> FilePath -> IO ProcessHandle
runVmImpl ctx handle vmName vmExecutable = do
  nixDiskImage <- getVmFilePath ctx vmName "image.qcow2"
  createDirectoryIfMissing True (takeDirectory nixDiskImage)
  parentEnvironment <- getEnvironment <&> Map.fromList
  vdeCtlDir <- Vde.getVdeCtlDir ctx
  (_, _, _, ph) <-
    createProcess
      ( ( System.Process.proc
            vmExecutable
            [ "-device",
              "virtio-net-pci,netdev=vlan1,mac=52:54:00:12:01:03",
              "-netdev",
              "vde,id=vlan1,sock=" <> vdeCtlDir
            ]
        )
          { env =
              Just $
                Map.toList $
                  parentEnvironment
                    <> "NIX_DISK_IMAGE" ~> nixDiskImage
                    <> "NIXOS_COMPOSE_FLAKE_DIR" ~> (ctx ^. #workingDir),
            std_in = CreatePipe,
            std_out = UseHandle handle,
            std_err = UseHandle handle
          }
      )
  pure ph

sshIntoVmImpl :: (Cradle.Output o) => Context -> VmName -> Port -> Text -> IO o
sshIntoVmImpl ctx vmName port command = do
  vmKeyPath <- getVmFilePath ctx vmName "vmkey"
  Cradle.run $
    Cradle.cmd "ssh"
      & Cradle.setStdinHandle (ctx ^. #stdin)
      & Cradle.addArgs
        [ "-i",
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
          "-q",
          "localhost",
          "--",
          command
        ]

updateVmHostsEntryImpl :: Context -> VmName -> Port -> Hostname -> IPv4 -> IO ()
updateVmHostsEntryImpl ctx vmName port hostname ip = do
  sshIntoVmImpl ctx vmName port $ "update-vm-hosts-entry " <> hostnameToText hostname <> " " <> IPv4.encode ip
