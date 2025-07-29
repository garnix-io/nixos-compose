{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

module IntegrationSpec where

import Context
import Cradle qualified
import Data.ByteString qualified as B
import Data.String.Conversions
import Data.String.Interpolate (i)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import NixVms qualified
import Options (VmName (..))
import State (readVmState)
import StdLib
import System.Directory (copyFile, doesDirectoryExist, getCurrentDirectory, listDirectory)
import System.FilePath ((</>))
import System.IO (SeekMode (..), hSeek)
import Test.Hspec
import Test.Hspec.Golden (defaultGolden)
import Test.Mockery.Directory (inTempDirectory)
import TestUtils hiding (withContext)
import TestUtils qualified

withContext :: (Context -> IO a) -> IO a
withContext = TestUtils.withContext NixVms.production

spec :: Spec
spec = do
  around_ inTempDirectory $ do
    it "lists vms" $ do
      withContext $ \ctx -> do
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
        result ^. #stdout `shouldBe` "configured vms: a, b, c\n"

    it "starts vms" $ do
      withContext $ \ctx -> do
        writeStandardFlake ctx Nothing
        _ <- assertSuccess $ test ctx ["start", "server"]
        (stdout <$> assertSuccess (test ctx ["ssh", "server", "hostname"])) `shouldReturn` "server\n"
        (stdout <$> assertSuccess (test ctx ["status", "server"])) `shouldReturn` "server: running\n"

    it "has nice output when starting vms" $ do
      withContext $ \ctx -> do
        writeStandardFlake ctx Nothing
        result <- assertSuccess $ test ctx ["start", "server"]
        stdout result `shouldBe` ""
        stderr result
          `shouldBe` T.unlines
            [ "Building NixOS config...",
              "Done",
              "Starting VM...",
              "Done"
            ]

    it "has nice output when the nix build fails" $ do
      withContext $ \ctx -> do
        writeStandardFlake ctx Nothing
        result <- test ctx ["start", "does-not-exist"]
        result ^. #exitCode `shouldBe` ExitFailure 1
        stdout result `shouldBe` ""
        cs (stderr result) `shouldContain` "Building NixOS config...\nCommand exited with code 1"
        cs (stderr result) `shouldContain` "does not provide attribute 'packages.x86_64-linux.nixosConfigurations.\"does-not-exist\""

    it "starts vms with arbitrary hostnames" $ do
      withContext $ \ctx -> do
        writeStandardFlake ctx (Just "{ lib, ...} : { networking.hostName = lib.mkForce \"other-hostname\"; }")
        _ <- assertSuccess $ test ctx ["start", "server"]
        pure ()

    it "starts a shell by default" $ do
      withContext $ \ctx -> do
        writeStandardFlake ctx Nothing
        _ <- assertSuccess $ test ctx ["start", "server"]
        B.hPutStr ctx.stdin "echo foo\nexit\n"
        hSeek ctx.stdin AbsoluteSeek 0
        (stdout <$> assertSuccess (test ctx ["ssh", "server"])) `shouldReturn` "foo\n\ESC]0;\a"

    it "can start multiple vms" $ do
      withContext $ \ctx -> do
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
        _ <- assertSuccess $ test ctx ["start", "a", "b"]
        (stdout <$> assertSuccess (test ctx ["ssh", "a", "hostname"])) `shouldReturn` "a\n"
        (stdout <$> assertSuccess (test ctx ["ssh", "b", "hostname"])) `shouldReturn` "b\n"

    it "can stop vms" $ do
      withContext $ \ctx -> do
        writeStandardFlake ctx Nothing
        _ <- assertSuccess $ test ctx ["start", "server"]
        (stdout <$> assertSuccess (test ctx ["status", "server"])) `shouldReturn` "server: running\n"
        state <- readVmState ctx (VmName "server")
        _ <- assertSuccess $ test ctx ["stop", "server"]
        (stdout <$> assertSuccess (test ctx ["status", "server"])) `shouldReturn` "server: not running\n"
        exist <- doesDirectoryExist ("/proc" </> show (state ^. #pid))
        when exist $ do
          status <- do
            contents <- T.readFile ("/proc" </> show (state ^. #pid) </> "status")
            pure $
              contents
                & T.lines
                & mapMaybe (T.stripPrefix "State:")
                & fmap T.strip
          status `shouldBe` ["Z (zombie)"]

    it "doesn't complain when starting a vm twice" $ do
      withContext $ \ctx -> do
        writeStandardFlake ctx Nothing
        _ <- assertSuccess $ test ctx ["start", "server"]
        result <- assertSuccess $ test ctx ["start", "server"]
        result ^. #stdout `shouldBe` "server: already running\n"

    describe "networking" $ do
      repoRoot <- runIO getCurrentDirectory
      it "allows to talk from one vm to the other by static ip" $ do
        withContext $ \ctx -> do
          copyFile (repoRoot </> "spec/static-ips/flake.nix") (workingDir ctx </> "flake.nix")
          _ <- assertSuccess $ test ctx ["start", "a", "b"]
          result <- assertSuccess (test ctx ["ssh", "a", "ping -c 1 10.0.0.6"])
          result ^. #stdout `shouldSatisfy` ("1 received" `T.isInfixOf`)
          result <- assertSuccess (test ctx ["ssh", "b", "ping -c 1 10.0.0.5"])
          result ^. #stdout `shouldSatisfy` ("1 received" `T.isInfixOf`)

  context "not inside a temporary working dir (for hspec-golden)" $ do
    it "stores the qcow2 image and other files in the storage dir" $ do
      stdout <- inTempDirectory $ do
        withContext $ \ctx -> do
          writeStandardFlake ctx Nothing
          _ <- assertSuccess $ test ctx ["start", "server"]
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
  T.writeFile (ctx.workingDir </> "flake.nix") flake

nixpkgs2505Commit :: Text
nixpkgs2505Commit = "3ff0e34b1383648053bba8ed03f201d3466f90c9"
