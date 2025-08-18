module IntegrationSpec (spec) where

import Context
import Cradle qualified
import Data.ByteString qualified as B
import Data.Maybe (fromJust)
import Data.String.Conversions
import Data.String.Interpolate (i)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import NixVms qualified
import State (getPid, readVmState)
import StdLib
import System.Directory (copyFile, doesDirectoryExist, getCurrentDirectory, listDirectory)
import System.IO (SeekMode (..), hSeek)
import Test.Hspec
import Test.Hspec.Golden (defaultGolden)
import Test.Mockery.Directory (inTempDirectory)
import TestUtils

spec :: Spec
spec = do
  around_ inTempDirectory $ around (withContext NixVms.production) $ do
    it "lists vms" $ \ctx -> do
      writeFile
        (workingDir ctx </> "flake.nix")
        [i|
          {
            inputs.nixpkgs.url = "github:nixos/nixpkgs/#{nixpkgs2505Commit}";
            outputs = { nixpkgs, ... }: {
              nixosConfigurations.a = (nixpkgs.lib.nixosSystem {
                modules = [
                  {
                    networking.hostName = "a";
                    nixpkgs.hostPlatform = "x86_64-linux";
                    system.stateVersion = "25.05";
                  }
                ];
              });
              nixosConfigurations.b = (nixpkgs.lib.nixosSystem {
                modules = [
                  {
                    networking.hostName = "b";
                    nixpkgs.hostPlatform = "x86_64-linux";
                    system.stateVersion = "25.05";
                  }
                ];
              });
              nixosConfigurations.c = (nixpkgs.lib.nixosSystem {
                modules = [
                  {
                    networking.hostName = "c";
                    nixpkgs.hostPlatform = "x86_64-linux";
                    system.stateVersion = "25.05";
                  }
                ];
              });
            };
          }
        |]
      result <- assertSuccess $ test ctx ["list"]
      result ^. #stdout `shouldBe` "configured vms:\n  - a\n  - b\n  - c\n"

    it "lists vms when there's no `nixosConfigurations` field" $ \ctx -> do
      writeFile
        (workingDir ctx </> "flake.nix")
        [i|
        {
          inputs.nixpkgs.url = "github:nixos/nixpkgs/#{nixpkgs2505Commit}";
          outputs = { nixpkgs, ... }: { };
        }
      |]
      result <- assertSuccess $ test ctx ["list"]
      result ^. #stdout `shouldBe` "no vms configured\n"

    it "starts vms" $ \ctx -> do
      writeStandardFlake ctx Nothing
      _ <- assertSuccess $ test ctx ["up", "server"]
      (stdout <$> assertSuccess (test ctx ["ssh", "server", "hostname"])) `shouldReturn` "server\n"
      (stdout <$> assertSuccess (test ctx ["status", "server"])) `shouldReturn` "server: running\n"

    it "has nice output when starting vms" $ \ctx -> do
      writeStandardFlake ctx Nothing
      result <- assertSuccess $ test ctx ["up", "server"]
      stdout result `shouldBe` ""
      stderr result
        `shouldBe` T.unlines
          [ "server: building...",
            "server: done building",
            "server: starting...",
            "server: done starting"
          ]

    it "has nice output when the nix build fails" $ \ctx -> do
      writeStandardFlake ctx Nothing
      result <- test ctx ["up", "does-not-exist"]
      result ^. #exitCode `shouldBe` ExitFailure 1
      stdout result `shouldBe` ""
      cs (stderr result) `shouldContain` "does-not-exist: building...\nCommand exited with code 1"
      cs (stderr result) `shouldContain` "does not provide attribute 'packages.x86_64-linux.nixosConfigurations.\"does-not-exist\""

    it "starts vms with arbitrary hostnames" $ \ctx -> do
      writeStandardFlake ctx (Just "{ lib, ...} : { networking.hostName = lib.mkForce \"other-hostname\"; }")
      _ <- assertSuccess $ test ctx ["up", "server"]
      pure ()

    it "starts a shell by default" $ \ctx -> do
      writeStandardFlake ctx Nothing
      _ <- assertSuccess $ test ctx ["up", "server"]
      B.hPutStr (ctx ^. #stdin) "echo foo\nexit\n"
      hSeek (ctx ^. #stdin) AbsoluteSeek 0
      (stdout <$> assertSuccess (test ctx ["ssh", "server"])) `shouldReturn` "foo\n\ESC]0;\a"

    it "can start multiple vms" $ \ctx -> do
      writeFile
        (workingDir ctx </> "flake.nix")
        [i|
          {
            inputs.nixpkgs.url = "github:nixos/nixpkgs/#{nixpkgs2505Commit}";
            outputs = { nixpkgs, ... }: {
              nixosConfigurations.a = (nixpkgs.lib.nixosSystem {
                modules = [
                  {
                    networking.hostName = "a";
                    nixpkgs.hostPlatform = "x86_64-linux";
                    system.stateVersion = "25.05";
                  }
                ];
              });
              nixosConfigurations.b = (nixpkgs.lib.nixosSystem {
                modules = [
                  {
                    networking.hostName = "b";
                    nixpkgs.hostPlatform = "x86_64-linux";
                    system.stateVersion = "25.05";
                  }
                ];
              });
            };
          }
        |]
      _ <- assertSuccess $ test ctx ["up", "a", "b"]
      (stdout <$> assertSuccess (test ctx ["ssh", "a", "hostname"])) `shouldReturn` "a\n"
      (stdout <$> assertSuccess (test ctx ["ssh", "b", "hostname"])) `shouldReturn` "b\n"

    it "can stop vms" $ \ctx -> do
      writeStandardFlake ctx Nothing
      _ <- assertSuccess $ test ctx ["up", "server"]
      (stdout <$> assertSuccess (test ctx ["status", "server"])) `shouldReturn` "server: running\n"
      state <- readVmState ctx "server"
      _ <- assertSuccess $ test ctx ["down", "server"]
      (stdout <$> assertSuccess (test ctx ["status", "server"])) `shouldReturn` "server: not running\n"
      exist <- doesDirectoryExist ("/proc" </> show (fromJust $ getPid state))
      when exist $ do
        status <- do
          contents <- T.readFile ("/proc" </> show (fromJust $ getPid state) </> "status")
          pure $
            contents
              & T.lines
              & mapMaybe (T.stripPrefix "State:")
              & fmap T.strip
        status `shouldBe` ["Z (zombie)"]

    it "doesn't complain when starting a vm twice" $ \ctx -> do
      writeStandardFlake ctx Nothing
      _ <- assertSuccess $ test ctx ["up", "server"]
      result <- assertSuccess $ test ctx ["up", "server"]
      result ^. #stdout `shouldBe` "server: already running\n"

    it "allows mounting the repo root using the NIXOS_COMPOSE_FLAKE_DIR environment variable" $ \ctx -> do
      writeFile (workingDir ctx </> "foo") "bar"
      writeFile
        (workingDir ctx </> "flake.nix")
        [i|
          {
            inputs.nixpkgs.url = "github:nixos/nixpkgs/#{nixpkgs2505Commit}";
            outputs = { nixpkgs, ... }: {
              nixosConfigurations.server = (nixpkgs.lib.nixosSystem {
                modules = [
                  {
                    networking.hostName = "server";
                    nixpkgs.hostPlatform = "x86_64-linux";
                    system.stateVersion = "25.05";
                    virtualisation.vmVariant.virtualisation.sharedDirectories.test = {
                      source = "$NIXOS_COMPOSE_FLAKE_DIR";
                      target = "/mnt";
                    };
                  }
                ];
              });
            };
          }
        |]
      _ <- assertSuccess $ test ctx ["up", "server"]
      (stdout <$> assertSuccess (test ctx ["ssh", "server", "sudo cat /mnt/foo"])) `shouldReturn` "bar"

    it "does not output device status report ansi sequences" $ \ctx -> do
      writeStandardFlake ctx Nothing
      result <- assertSuccess $ test ctx ["up", "server", "-v"]
      result ^. #stdout `shouldNotSatisfy` ("\ESC[6n" `T.isInfixOf`)

    describe "networking" $ do
      repoRoot <- runIO getCurrentDirectory
      it "allows to talk from one vm to the other by static ip" $ \ctx -> do
        copyFile (repoRoot </> "spec/static-ips/flake.nix") (workingDir ctx </> "flake.nix")
        _ <- assertSuccess $ test ctx ["up", "a", "b"]
        aIp <- T.strip . stdout <$> assertSuccess (test ctx ["ip", "a"])
        bIp <- T.strip . stdout <$> assertSuccess (test ctx ["ip", "b"])
        result <- assertSuccess $ test ctx ["ssh", "a", "ping -c 1 " <> bIp]
        result ^. #stdout `shouldSatisfy` ("1 received" `T.isInfixOf`)
        result <- assertSuccess $ test ctx ["ssh", "b", "ping -c 1 " <> aIp]
        result ^. #stdout `shouldSatisfy` ("1 received" `T.isInfixOf`)

      it "allows connecting to VMs by their name" $ \ctx -> do
        copyFile (repoRoot </> "spec/domains/flake.nix") (workingDir ctx </> "flake.nix")
        _ <- assertSuccess $ test ctx ["up", "server", "client"]
        result <- assertSuccess $ test ctx ["ssh", "client", "fetch-from-server"]
        result ^. #stdout `shouldBe` "hello from nginx"

  context "not inside a temporary working dir (for hspec-golden)" $ do
    it "stores the qcow2 image and other files in the storage dir" $ do
      stdout <- inTempDirectory $ do
        TestUtils.withContext NixVms.production $ \ctx -> do
          writeStandardFlake ctx Nothing
          _ <- assertSuccess $ test ctx ["up", "server"]
          files <- listDirectory "."
          files `shouldBe` []
          Cradle.StdoutRaw stdout <- Cradle.run $ Cradle.cmd "tree" & Cradle.setWorkingDir (ctx ^. #storageDir)
          pure stdout
      pure $ defaultGolden "storage-tree" (cs stdout)

writeStandardFlake :: Context -> Maybe Text -> IO ()
writeStandardFlake ctx addedModule = do
  let emptyModule = "{}"
  let flake =
        cs
          [i|
            {
              inputs.nixpkgs.url = "github:nixos/nixpkgs/#{nixpkgs2505Commit}";
              outputs = { nixpkgs, ... }: {
                nixosConfigurations.server = (nixpkgs.lib.nixosSystem {
                  modules = [
                    {
                      networking.hostName = "server";
                      nixpkgs.hostPlatform = "x86_64-linux";
                      system.stateVersion = "25.05";
                    }
                    (#{fromMaybe emptyModule addedModule})
                  ];
                });
              };
            }
          |]
  T.writeFile (ctx ^. #workingDir </> "flake.nix") flake

nixpkgs2505Commit :: Text
nixpkgs2505Commit = "3ff0e34b1383648053bba8ed03f201d3466f90c9"
