module CliSpec where

import Context
import Control.Concurrent (readMVar)
import Cradle
import Data.Maybe (fromJust)
import State (readState, readVmState)
import StdLib
import System.Directory (getSymbolicLinkTarget, listDirectory)
import System.FilePath
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
        listDirectory (ctx ^. #storageDir) `shouldReturn` []

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

  describe "`vde_switch`" $ do
    let assertVdeIsRunning ctx = do
          state <- readState ctx
          case state of
            Nothing -> expectationFailure "assertVdeIsRunning: no state file"
            Just state -> do
              exe <- getSymbolicLinkTarget $ "/proc" </> show (state ^. #vde . #pid :: Int64) </> "exe"
              takeFileName exe `shouldBe` "vde_switch"

    let assertVmIsRunning ctx vmName = do
          state <- readVmState ctx vmName
          let pid :: Int = state ^. #pid
          comm <- readFile $ "/proc" </> show (pid :: Int) </> "comm"
          comm `shouldBe` "sleep\n"

    it "starts the switch when starting a vm" $ do
      withMockContext ["a"] $ \ctx -> do
        _ <- assertSuccess $ test ctx ["start", "a"]
        assertVdeIsRunning ctx

    it "stops the switch when a vm is stopped" $ do
      withMockContext ["a"] $ \ctx -> do
        _ <- assertSuccess $ test ctx ["start", "a"]
        state <- fromJust <$> readState ctx
        _ <- assertSuccess $ test ctx ["stop", "a"]
        readState ctx `shouldReturn` Nothing
        (StdoutRaw stdout) <- Cradle.run $ cmd "ps" & addArgs ["-p", show (state ^. #vde . #pid), "-o", "stat", "--no-headers"]
        -- It's stopped, but still a zombie, since it's a child of the test-suite.
        stdout `shouldSatisfy` (`elem` ["Z\n", "Z+\n"])

    it "cleans up the files after stopping" $ do
      withMockContext ["a"] $ \ctx -> do
        _ <- assertSuccess $ test ctx ["start", "a"]
        _ <- assertSuccess $ test ctx ["stop", "a"]
        listDirectory (ctx ^. #storageDir) `shouldReturn` []

    it "keeps the switch running for multiple vms" $ do
      withMockContext ["a", "b"] $ \ctx -> do
        _ <- assertSuccess $ test ctx ["start", "a"]
        _ <- assertSuccess $ test ctx ["start", "b"]
        assertVdeIsRunning ctx
        _ <- assertSuccess $ test ctx ["stop", "b"]
        assertVdeIsRunning ctx

    it "stops the switch after all vms are stopped" $ do
      withMockContext ["a", "b"] $ \ctx -> do
        _ <- assertSuccess $ test ctx ["start", "a"]
        _ <- assertSuccess $ test ctx ["start", "b"]
        assertVdeIsRunning ctx
        _ <- assertSuccess $ test ctx ["stop", "b"]
        _ <- assertSuccess $ test ctx ["stop", "a"]
        readState ctx `shouldReturn` Nothing

    it "restarts the switch after e.g. a reboot" $ do
      withMockContext ["a", "b"] $ \ctx -> do
        _ <- assertSuccess $ test ctx ["start", "a"]
        readMVar (fromJust (ctx ^. #registeredProcesses)) >>= mapM_ endProcess
        _ <- assertSuccess $ test ctx ["start", "a"]
        assertVdeIsRunning ctx
        assertVmIsRunning ctx "a"
