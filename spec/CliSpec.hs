module CliSpec where

import Context
import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import Cradle
import Ki qualified
import Net.IPv4 qualified as IPv4
import State (VmState (..), readVmState)
import StdLib
import System.Directory (listDirectory)
import Test.Hspec
import Test.Hspec.Golden (defaultGolden)
import TestUtils

spec :: Spec
spec = do
  describe "help output" $ do
    it "outputs all commands when invoked without arguments" $ do
      withMockContext ["a"] $ \ctx -> do
        result <- test ctx []
        pure $ defaultGolden "stderr" (cs (result ^. #stderr))

  describe "status" $ do
    context "without vm arguments" $ do
      it "lists the status of all vms" $ do
        withMockContext ["a", "b"] $ \ctx -> do
          _ <- assertSuccess $ test ctx ["start", "a"]
          _ <- assertSuccess $ test ctx ["start", "b"]
          result <- assertSuccess $ test ctx ["status"]
          result ^. #stdout `shouldBe` "a: running\nb: running\n"

      it "lists all vms when some are running" $ do
        withMockContext ["a", "b"] $ \ctx -> do
          _ <- assertSuccess $ test ctx ["start", "a"]
          result <- assertSuccess $ test ctx ["status"]
          result ^. #stdout `shouldBe` "a: running\nb: not running\n"

      it "lists all vms when none are running" $ do
        withMockContext ["a", "b"] $ \ctx -> do
          result <- assertSuccess $ test ctx ["status"]
          result ^. #stdout `shouldBe` "a: not running\nb: not running\n"

      it "prints a nice message when no vms are configured" $ do
        withMockContext [] $ \ctx -> do
          result <- assertSuccess $ test ctx ["status"]
          result ^. #stdout `shouldBe` "no vms configured\n"

    it "accepts multiple vm names" $ do
      withMockContext ["a", "b", "c"] $ \ctx -> do
        _ <- assertSuccess $ test ctx ["start", "a"]
        _ <- assertSuccess $ test ctx ["start", "b"]
        _ <- assertSuccess $ test ctx ["start", "c"]
        result <- assertSuccess $ test ctx ["status", "a", "c"]
        result ^. #stdout `shouldBe` "a: running\nc: running\n"
        result <- assertSuccess $ test ctx ["status", "c", "b"]
        result ^. #stdout `shouldBe` "c: running\nb: running\n"
        result <- assertSuccess $ test ctx ["status", "b", "d"]
        result ^. #stdout `shouldBe` "b: running\nd: not running\n"

    it "removes the state directory when the vm process is not running anymore" $ do
      withMockContext ["a"] $ \ctx -> do
        _ <- assertSuccess $ test ctx ["start", "a"]
        stopProcess ctx (Vm "a")
        result <- assertSuccess $ test ctx ["status", "a"]
        result ^. #stdout `shouldBe` "WARN: cannot find process for vm: a\na: not running\n"
        listDirectory (ctx ^. #storageDir) `shouldReturn` ["state.json"]

  describe "list" $ do
    it "lists all configured vms" $ do
      withMockContext ["a", "b", "c"] $ \ctx -> do
        result <- assertSuccess $ test ctx ["list"]
        result ^. #stdout `shouldBe` "configured vms: a, b, c\n"

    it "has a nice message when no vms are configured" $ do
      withMockContext [] $ \ctx -> do
        result <- assertSuccess $ test ctx ["list"]
        result ^. #stdout `shouldBe` "no vms configured\n"

  describe "start" $ do
    context "when `--all` is given" $ do
      it "starts all vms" $ do
        withMockContext ["a", "b", "c"] $ \ctx -> do
          _ <- assertSuccess $ test ctx ["start", "--all"]
          result <- assertSuccess $ test ctx ["status"]
          result ^. #stdout `shouldBe` "a: running\nb: running\nc: running\n"

      it "gives a nice message when no vms are defined" $ do
        withMockContext [] $ \ctx -> do
          result <- test ctx ["start", "--all"]
          result `shouldBe` TestResult "" "No vms are defined. Nothing to do.\n" (ExitFailure 1)

    describe "vm building state" $ do
      context "when the nix build blocks" $ do
        let blockingBuildVmScript :: Context -> Context
            blockingBuildVmScript =
              (#nixVms . #buildVmScript)
                %~ ( \_buildVmScript _ctx _vmName _ip -> do
                       forever $ threadDelay 1_000_000
                   )
        it "locks the state of a vm when building the nixos config" $ do
          withMockContext ["a"] $ \(blockingBuildVmScript -> ctx) -> do
            Ki.scoped $ \scope -> do
              _ <- Ki.fork scope $ do
                test ctx ["start", "a"]
              waitFor $ do
                vmState <- readVmState ctx "a"
                vmState `shouldBe` Starting {ip = IPv4.fromOctets 10 0 0 2}
                test ctx ["start", "a"] `shouldReturn` TestResult "a: already starting\n" "" ExitSuccess
                test ctx ["status", "a"] `shouldReturn` TestResult "a: starting\n" "" ExitSuccess
                test ctx ["ip", "a"] `shouldReturn` TestResult "10.0.0.2\n" "" ExitSuccess

        it "handles attempts to stop building vms gracefully" $ do
          withMockContext ["a"] $ \(blockingBuildVmScript -> ctx) -> do
            Ki.scoped $ \scope -> do
              _ <- Ki.fork scope $ do
                test ctx ["start", "a"]
              waitFor $ do
                test ctx ["stop", "a"]
                  `shouldReturn` TestResult
                    { stdout = "",
                      stderr = "a: building, cannot stop a building vm\n",
                      exitCode = ExitFailure 1
                    }

      it "locks the state of a vm when booting" $ do
        let blockingRunVm :: Context -> Context
            blockingRunVm =
              (#nixVms . #runVm)
                %~ ( \_runVm _ctx _verbosity _vmName _vmScript -> do
                       forever $ threadDelay 1_000_000
                   )
        withMockContext ["a"] $ \(blockingRunVm -> ctx) -> do
          Ki.scoped $ \scope -> do
            _ <- Ki.fork scope $ do
              test ctx ["start", "a"]
            waitFor $ do
              vmState <- readVmState ctx "a"
              vmState `shouldBe` Starting {ip = IPv4.fromOctets 10 0 0 2}
              test ctx ["start", "a"] `shouldReturn` TestResult "a: already starting\n" "" ExitSuccess
              test ctx ["status", "a"] `shouldReturn` TestResult "a: starting\n" "" ExitSuccess
              test ctx ["ip", "a"] `shouldReturn` TestResult "10.0.0.2\n" "" ExitSuccess

  describe "ssh" $ do
    let cases =
          [ (["true"], ExitSuccess),
            (["false"], ExitFailure 1),
            (["--", "bash", "-c", "'exit 42'"], ExitFailure 42)
          ]
    forM_ cases $ \(command, exitCode) ->
      it ("relays the exit code " <> show exitCode) $ do
        withMockContext ["a"] $ \ctx -> do
          _ <- assertSuccess $ test ctx ["start", "a"]
          result <- test ctx (["ssh", "a"] <> command)
          result
            `shouldBe` TestResult
              { exitCode,
                stdout = "",
                stderr = ""
              }
