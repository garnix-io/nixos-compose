module CliSpec where

import Context
import Control.Concurrent (myThreadId, newEmptyMVar, putMVar, readMVar, threadDelay, throwTo)
import Control.Exception (AsyncException (..))
import Control.Monad (forever)
import Cradle
import Data.Maybe (fromJust)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Ki qualified
import Net.IPv4 qualified as IPv4
import State (VmState (..), getPid, readState, readVmState)
import StdLib
import System.Directory (doesDirectoryExist, listDirectory)
import System.IO qualified
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
          _ <- assertSuccess $ test ctx ["up", "a"]
          _ <- assertSuccess $ test ctx ["up", "b"]
          result <- assertSuccess $ test ctx ["status"]
          result ^. #stdout `shouldBe` "a: running\nb: running\n"

      it "lists all vms when some are running" $ do
        withMockContext ["a", "b"] $ \ctx -> do
          _ <- assertSuccess $ test ctx ["up", "a"]
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
        _ <- assertSuccess $ test ctx ["up", "a"]
        _ <- assertSuccess $ test ctx ["up", "b"]
        _ <- assertSuccess $ test ctx ["up", "c"]
        result <- assertSuccess $ test ctx ["status", "a", "c"]
        result ^. #stdout `shouldBe` "a: running\nc: running\n"
        result <- assertSuccess $ test ctx ["status", "c", "b"]
        result ^. #stdout `shouldBe` "c: running\nb: running\n"
        result <- assertSuccess $ test ctx ["status", "b", "d"]
        result ^. #stdout `shouldBe` "b: running\nd: not running\n"

    it "removes the state directory when the vm process is not running anymore" $ do
      withMockContext ["a"] $ \ctx -> do
        _ <- assertSuccess $ test ctx ["up", "a"]
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

  describe "up" $ do
    context "when no vm names are given" $ do
      it "starts all vms" $ do
        withMockContext ["a", "b", "c"] $ \ctx -> do
          _ <- assertSuccess $ test ctx ["up"]
          result <- assertSuccess $ test ctx ["status"]
          result ^. #stdout `shouldBe` "a: running\nb: running\nc: running\n"

      it "gives a nice message when no vms are defined" $ do
        withMockContext [] $ \ctx -> do
          result <- test ctx ["up"]
          result `shouldBe` TestResult "" "No vms are defined. Nothing to do.\n" (ExitFailure 1)

    describe "vm building state" $ do
      context "when the nix build blocks" $ do
        let blockingBuildVmScript :: Context -> Context
            blockingBuildVmScript =
              (#nixVms . #buildVmScript)
                .~ ( \_ctx _vmName _ip -> do
                       forever $ threadDelay 1_000_000
                   )
        it "locks the state of a vm when building the nixos config" $ do
          withMockContext ["a"] $ \(blockingBuildVmScript -> ctx) -> do
            Ki.scoped $ \scope -> do
              _ <- Ki.fork scope $ do
                test ctx ["up", "a"]
              waitFor $ do
                vmState <- readVmState ctx "a"
                vmState `shouldBe` Starting {ip = IPv4.fromOctets 10 0 0 2}
                test ctx ["up", "a"] `shouldReturn` TestResult "a: already starting\n" "" ExitSuccess
                test ctx ["status", "a"] `shouldReturn` TestResult "a: starting\n" "" ExitSuccess
                test ctx ["ip", "a"] `shouldReturn` TestResult "10.0.0.2\n" "" ExitSuccess

        it "handles attempts to stop building vms gracefully" $ do
          withMockContext ["a"] $ \(blockingBuildVmScript -> ctx) -> do
            Ki.scoped $ \scope -> do
              _ <- Ki.fork scope $ do
                test ctx ["up", "a"]
              waitFor $ do
                test ctx ["down", "a"]
                  `shouldReturn` TestResult
                    { stdout = "",
                      stderr = "a: building, cannot stop a building vm\n",
                      exitCode = ExitFailure 1
                    }

      it "locks the state of a vm when booting" $ do
        let blockingRunVm :: Context -> Context
            blockingRunVm =
              (#nixVms . #runVm)
                .~ ( \_ctx _verbosity _vmName _vmScript -> do
                       forever $ threadDelay 1_000_000
                   )
        withMockContext ["a"] $ \(blockingRunVm -> ctx) -> do
          Ki.scoped $ \scope -> do
            _ <- Ki.fork scope $ do
              test ctx ["up", "a"]
            waitFor $ do
              vmState <- readVmState ctx "a"
              vmState `shouldBe` Starting {ip = IPv4.fromOctets 10 0 0 2}
              test ctx ["up", "a"] `shouldReturn` TestResult "a: already starting\n" "" ExitSuccess
              test ctx ["status", "a"] `shouldReturn` TestResult "a: starting\n" "" ExitSuccess
              test ctx ["ip", "a"] `shouldReturn` TestResult "10.0.0.2\n" "" ExitSuccess

    describe "when nix evaluation fails" $ do
      let failingBuildVmScript :: Context -> Context
          failingBuildVmScript =
            #nixVms
              . #buildVmScript
              .~ ( \_ctx _vmName _ip -> do
                     T.hPutStrLn System.IO.stderr "test output"
                     throwIO $ ExitFailure 42
                 )
      it "prints out the error message" $ do
        withMockContext ["a"] $ \(failingBuildVmScript -> ctx) -> do
          test ctx ["up", "a"] `shouldReturn` TestResult "" "Building NixOS config...\ntest output\n" (ExitFailure 42)

      it "doesn't add a vm to the state" $ do
        withMockContext ["a"] $ \(failingBuildVmScript -> ctx) -> do
          test ctx ["up", "a"] `shouldReturn` TestResult "" "Building NixOS config...\ntest output\n" (ExitFailure 42)
          (^. #vms) <$> readState ctx `shouldReturn` mempty
          (^. #vde) <$> readState ctx `shouldReturn` Nothing

      it "doesn't add a vm to the state when interrupted by an async exception, e.g. Ctrl-C" $ do
        blockingOnBuild <- newEmptyMVar
        let blockingBuildVmScript :: Context -> Context
            blockingBuildVmScript =
              (#nixVms . #buildVmScript)
                .~ ( \_ctx _vmName _ip -> do
                       putMVar blockingOnBuild ()
                       forever $ threadDelay 1_000_000
                   )
        withMockContext ["a"] $ \(blockingBuildVmScript -> ctx) -> do
          Ki.scoped $ \scope -> do
            threadId <- newEmptyMVar
            _ <- Ki.fork scope $ do
              myThreadId >>= putMVar threadId
              test ctx ["up", "a"]
            readMVar blockingOnBuild
            threadId <- readMVar threadId
            throwTo threadId UserInterrupt
            waitFor $ do
              (^. #vms) <$> readState ctx `shouldReturn` mempty
              (^. #vde) <$> readState ctx `shouldReturn` Nothing

  describe "down" $ do
    it "stops vms" $ do
      withMockContext ["a"] $ \ctx -> do
        _ <- assertSuccess $ test ctx ["up", "a"]
        (stdout <$> assertSuccess (test ctx ["status", "a"])) `shouldReturn` "a: running\n"
        state <- readVmState ctx "a"
        test ctx ["down", "a"] `shouldReturn` TestResult "stopping a\n" "" ExitSuccess
        (stdout <$> assertSuccess (test ctx ["status", "a"])) `shouldReturn` "a: not running\n"
        exist <- doesDirectoryExist ("/proc" </> show (fromJust $ State.getPid state))
        when exist $ do
          status <- do
            contents <- T.readFile ("/proc" </> show (fromJust $ State.getPid state) </> "status")
            pure $
              contents
                & T.lines
                & mapMaybe (T.stripPrefix "State:")
                & fmap T.strip
          status `shouldBe` ["Z (zombie)"]

    it "stops multiple specified vms" $ do
      withMockContext ["a", "b", "c"] $ \ctx -> do
        _ <- assertSuccess $ test ctx ["up", "a", "b", "c"]
        (stdout <$> assertSuccess (test ctx ["status"])) `shouldReturn` "a: running\nb: running\nc: running\n"
        _ <- assertSuccess $ test ctx ["down", "a", "c"]
        (stdout <$> assertSuccess (test ctx ["status"])) `shouldReturn` "a: not running\nb: running\nc: not running\n"

    it "stops all vms when no vm name given" $ do
      withMockContext ["a", "b"] $ \ctx -> do
        _ <- assertSuccess $ test ctx ["up", "a", "b"]
        (stdout <$> assertSuccess (test ctx ["status"])) `shouldReturn` "a: running\nb: running\n"
        _ <- assertSuccess $ test ctx ["down"]
        (stdout <$> assertSuccess (test ctx ["status"])) `shouldReturn` "a: not running\nb: not running\n"

    it "prints a nice message when no vm names are given and no vms are running" $ do
      withMockContext ["a", "b"] $ \ctx -> do
        (stdout <$> assertSuccess (test ctx ["down"])) `shouldReturn` "no vms running, nothing to do\n"

    it "prints a nice message when the specified vms are not running" $ do
      withMockContext ["a", "b"] $ \ctx -> do
        _ <- assertSuccess $ test ctx ["up", "a"]
        (stdout <$> assertSuccess (test ctx ["down", "b"])) `shouldReturn` "b is not running, nothing to do\n"

  describe "ssh" $ do
    let cases =
          [ (["true"], ExitSuccess),
            (["false"], ExitFailure 1),
            (["--", "bash", "-c", "'exit 42'"], ExitFailure 42)
          ]
    forM_ cases $ \(command, exitCode) ->
      it ("relays the exit code " <> show exitCode) $ do
        withMockContext ["a"] $ \ctx -> do
          _ <- assertSuccess $ test ctx ["up", "a"]
          result <- test ctx (["ssh", "a"] <> command)
          result
            `shouldBe` TestResult
              { exitCode,
                stdout = "",
                stderr = ""
              }
