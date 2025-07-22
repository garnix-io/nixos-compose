{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

module RunSpec where

import Context
import Control.Concurrent (modifyMVar_, newMVar, readMVar)
import Control.Concurrent.MVar (newEmptyMVar, putMVar)
import Control.Exception (finally)
import Control.Monad
import Data.ByteString qualified as B
import Data.Maybe (fromMaybe)
import Data.String.Conversions
import Data.String.Interpolate (i)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Run (run)
import State (VmName (VmName), readState)
import StdLib
import System.Directory (doesDirectoryExist, listDirectory)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO (SeekMode (..), hSeek)
import System.IO qualified
import System.IO.Silently
import System.IO.Temp (withSystemTempDirectory, withSystemTempFile)
import System.Process
import Test.Hspec
import Test.Mockery.Directory (inTempDirectory)

spec :: Spec
spec = around_ inTempDirectory $ do
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

  it "does not leave a qcow2 image lying around" $ do
    withContext $ \ctx -> do
      writeStandardFlake ctx Nothing
      _ <- assertSuccess $ test ctx ["start", "server"]
      files <- listDirectory "."
      files `shouldBe` []

  it "does not reuse an outdated qcow2 image" $ do
    pending

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
      (stdout <$> assertSuccess (test ctx ["ssh", "server"])) `shouldReturn` "foo\n"

  it "can start multiple vms" $ do
    withContext $ \ctx -> do
      writeFile
        (workingDir ctx </> "flake.nix")
        [i|
          {
            inputs.nixpkgs.url = "github:nixos/nixpkgs/2f913f37ac91d3dda25c9259f17dbedcf908a157";
            outputs = { nixpkgs, ... }: {
              nixosConfigurations.a = (nixpkgs.lib.nixosSystem {
                modules = [
                  {
                    networking.hostName = "a";
                    nixpkgs.hostPlatform = "x86_64-linux";
                    system.stateVersion = "24.11";
                  }
                ];
              });
              nixosConfigurations.b = (nixpkgs.lib.nixosSystem {
                modules = [
                  {
                    networking.hostName = "b";
                    nixpkgs.hostPlatform = "x86_64-linux";
                    system.stateVersion = "24.11";
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
      state <- readState ctx (VmName "server")
      _ <- assertSuccess $ test ctx ["stop", "server"]
      (stdout <$> assertSuccess (test ctx ["status", "server"])) `shouldReturn` "no vms running\n"
      doesDirectoryExist ("/proc" </> show (state ^. #pid)) `shouldReturn` False

  it "doesn't complain when starting a vm twice" $ do
    withContext $ \ctx -> do
      writeStandardFlake ctx Nothing
      _ <- assertSuccess $ test ctx ["start", "server"]
      result <- assertSuccess $ test ctx ["start", "server"]
      result ^. #stdout `shouldBe` "server: already running\n"

  it "removes the state directory when the vm process is not running anymore" $ do
    withContext $ \ctx -> do
      mvar <- newEmptyMVar
      ctx <-
        pure $
          ctx
            { registerProcess = \handle -> registerProcess ctx handle >> putMVar mvar handle
            }
      writeStandardFlake ctx Nothing
      _ <- assertSuccess $ test ctx ["start", "server"]
      handle <- readMVar mvar
      _ <- endProcess handle
      result <- assertSuccess $ test ctx ["status", "server"]
      result ^. #stdout `shouldBe` "WARN: cannot find process for vm: server\nno vms running\n"
      listDirectory (ctx ^. #storageDir) `shouldReturn` []

  it "`stop` cleans up in the storageDir" $ do
    pending

  it "`status` without argument lists the status of all vms" $ do
    pending

  it "prints a help text with all commands when running with no arguments" $ do
    pending

data TestResult = TestResult
  { stdout :: Text,
    stderr :: Text,
    exitCode :: ExitCode
  }
  deriving stock (Generic, Show)

assertSuccess :: (HasCallStack) => IO TestResult -> IO TestResult
assertSuccess action = do
  result <- action
  when (result ^. #exitCode /= ExitSuccess) $ do
    expectationFailure $ "command exited with " <> show result
  pure result

test :: Context -> [Text] -> IO TestResult
test ctx args = do
  (stderr, (stdout, exitCode)) <- hCapture [System.IO.stderr] $ capture $ run ctx args
  pure $ TestResult (cs stdout) (cs stderr) exitCode

writeStandardFlake :: Context -> Maybe Text -> IO ()
writeStandardFlake ctx addedModule = do
  let emptyModule = "{}"
  let flake =
        cs
          [i|
            {
              inputs.nixpkgs.url = "github:nixos/nixpkgs/2f913f37ac91d3dda25c9259f17dbedcf908a157";
              outputs = { nixpkgs, ... }: {
                nixosConfigurations.server = (nixpkgs.lib.nixosSystem {
                  modules = [
                    {
                      networking.hostName = "server";
                      nixpkgs.hostPlatform = "x86_64-linux";
                      system.stateVersion = "24.11";
                    }
                    (#{fromMaybe emptyModule addedModule})
                  ];
                });
              };
            }
          |]
  T.writeFile (ctx.workingDir </> "flake.nix") flake

withContext :: (Context -> IO a) -> IO a
withContext action = do
  withSystemTempFile "test-stdin" $ \_stdinFile stdinHandle -> do
    withSystemTempDirectory "test-working-dir" $ \workingDir -> do
      withSystemTempDirectory "test-storage-dir" $ \storageDir -> do
        processHandles <- newMVar []
        let ctx =
              Context
                { registerProcess = \handle -> modifyMVar_ processHandles $ \h -> pure $ h <> [handle],
                  stdin = stdinHandle,
                  workingDir = workingDir,
                  storageDir = storageDir </> "vmcli"
                }
        action ctx
          `finally` (readMVar processHandles >>= mapM_ endProcess)

endProcess :: ProcessHandle -> IO ExitCode
endProcess handle = do
  terminateProcess handle
  waitForProcess handle
