module CliSpec where

import Context
import Control.Concurrent (newEmptyMVar, putMVar, readMVar)
import Cradle
import Data.IORef (modifyIORef, newIORef, readIORef)
import Data.Maybe (fromJust)
import Data.String.Conversions (cs)
import State (readState, readVdeState)
import StdLib
import System.Directory (getSymbolicLinkTarget, listDirectory)
import System.FilePath
import System.Posix (readSymbolicLink)
import System.Process (Pid, getPid)
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
    it "without argument lists the status of all vms" $ do
      withMockContext ["a", "b"] $ \ctx -> do
        _ <- assertSuccess $ test ctx ["start", "a"]
        _ <- assertSuccess $ test ctx ["start", "b"]
        result <- assertSuccess $ test ctx ["status"]
        result ^. #stdout `shouldBe` "a: running\nb: running\n"

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
        mvar <- newEmptyMVar
        ctx <-
          pure $
            ctx
              { registerProcess = \handle -> do
                  registerProcess ctx handle
                  Just pid <- getPid handle
                  exe <- readSymbolicLink $ "/proc" </> show (pid :: Pid) </> "exe"
                  when (takeFileName exe /= "vde_switch") $ do
                    putMVar mvar handle
              }
        _ <- assertSuccess $ test ctx ["start", "a"]
        handle <- readMVar mvar
        _ <- endProcess handle
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
          vdeState <- readVdeState ctx
          case vdeState of
            Nothing -> expectationFailure "assertVdeIsRunning: no vde state"
            Just vdeState -> do
              exe <- getSymbolicLinkTarget $ "/proc" </> show (vdeState ^. #pid :: Int64) </> "exe"
              takeFileName exe `shouldBe` "vde_switch"

    let assertVmIsRunning ctx vmName = do
          state <- readState ctx vmName
          let pid :: Int = fromJust $ state ^. #pid
          comm <- readFile $ "/proc" </> show (pid :: Int) </> "comm"
          comm `shouldBe` "sleep\n"

    it "starts the switch when starting a vm" $ do
      withMockContext ["a"] $ \ctx -> do
        _ <- assertSuccess $ test ctx ["start", "a"]
        assertVdeIsRunning ctx

    it "stops the switch when a vm is stopped" $ do
      withMockContext ["a"] $ \ctx -> do
        _ <- assertSuccess $ test ctx ["start", "a"]
        vdeState <- fromJust <$> readVdeState ctx
        _ <- assertSuccess $ test ctx ["stop", "a"]
        readVdeState ctx `shouldReturn` Nothing
        (StdoutRaw stdout) <- Cradle.run $ cmd "ps" & addArgs ["-p", show (vdeState ^. #pid), "-o", "stat", "--no-headers"]
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
        readVdeState ctx `shouldReturn` Nothing

    it "restarts the switch after e.g. a reboot" $ do
      withMockContext ["a", "b"] $ \ctx -> do
        processes <- newIORef []
        ctx <-
          pure $
            ctx
              { registerProcess = \handle -> do
                  registerProcess ctx handle
                  modifyIORef processes (handle :)
              }
        _ <- assertSuccess $ test ctx ["start", "a"]
        readIORef processes >>= mapM_ endProcess
        _ <- assertSuccess $ test ctx ["start", "a"]
        assertVdeIsRunning ctx
        assertVmIsRunning ctx "a"
