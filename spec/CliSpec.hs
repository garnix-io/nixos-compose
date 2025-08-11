module CliSpec where

import Context
import Data.Maybe (fromJust)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import State (getPid, readVmState)
import StdLib
import System.Directory (doesDirectoryExist, listDirectory)
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

    it "has a --version flag" $ do
      withMockContext [] $ \ctx -> do
        test ctx ["--version"]
          `shouldReturn` TestResult "unknown\n" "" ExitSuccess

    it "ignores other options and flags when --version is given" $ do
      withMockContext [] $ \ctx -> do
        test ctx ["--version", "list"]
          `shouldReturn` TestResult "unknown\n" "" ExitSuccess

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
        (stderr <$> assertSuccess (test ctx ["down"])) `shouldReturn` "no vms running, nothing to do\n"

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
