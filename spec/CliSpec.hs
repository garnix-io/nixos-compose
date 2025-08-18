module CliSpec where

import Context
import Data.Map qualified as Map
import Data.Maybe (fromJust)
import Data.String.Interpolate (i)
import Data.String.Interpolate.Util (unindent)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Net.IPv4 qualified as IPv4
import State (getPid, readVmState)
import State qualified
import StdLib
import System.Directory (doesDirectoryExist, listDirectory)
import Table (renderTable)
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
          result ^. #stdout
            `shouldBe` renderTable
              False
              [ [("name", "a"), ("status", "running")],
                [("name", "b"), ("status", "running")]
              ]

      it "lists all vms when some are running" $ do
        withMockContext ["a", "b"] $ \ctx -> do
          _ <- assertSuccess $ test ctx ["up", "a"]
          result <- assertSuccess $ test ctx ["status"]
          result ^. #stdout
            `shouldBe` renderTable
              False
              [ [("name", "a"), ("status", "running")],
                [("name", "b"), ("status", "not running")]
              ]

      it "lists all vms when none are running" $ do
        withMockContext ["a", "b"] $ \ctx -> do
          result <- assertSuccess $ test ctx ["status"]
          result ^. #stdout
            `shouldBe` renderTable
              False
              [ [("name", "a"), ("status", "not running")],
                [("name", "b"), ("status", "not running")]
              ]

      it "prints a nice message when no vms are configured" $ do
        withMockContext [] $ \ctx -> do
          result <- assertSuccess $ test ctx ["status"]
          result ^. #stdout `shouldBe` "no vms configured, no vms running\n"

    it "accepts multiple vm names" $ do
      withMockContext ["a", "b", "c"] $ \ctx -> do
        _ <- assertSuccess $ test ctx ["up", "a"]
        _ <- assertSuccess $ test ctx ["up", "b"]
        _ <- assertSuccess $ test ctx ["up", "c"]
        result <- assertSuccess $ test ctx ["status", "a", "c"]
        result ^. #stdout
          `shouldBe` renderTable
            False
            [ [("name", "a"), ("status", "running")],
              [("name", "c"), ("status", "running")]
            ]
        result <- assertSuccess $ test ctx ["status", "c", "b"]
        result ^. #stdout
          `shouldBe` renderTable
            False
            [ [("name", "c"), ("status", "running")],
              [("name", "b"), ("status", "running")]
            ]
        result <- assertSuccess $ test ctx ["status", "b", "d"]
        result ^. #stdout
          `shouldBe` renderTable
            False
            [ [("name", "b"), ("status", "running")],
              [("name", "d"), ("status", "not running")]
            ]

    it "removes the state directory when the vm process is not running anymore" $ do
      withMockContext ["a"] $ \ctx -> do
        _ <- assertSuccess $ test ctx ["up", "a"]
        stopProcess ctx (Vm "a")
        test ctx ["status", "a"]
          `shouldReturn` TestResult
            (renderTable False [[("name", "a"), ("status", "not running")]])
            "WARN: cannot find process for vm: a\n"
            ExitSuccess
        listDirectory (ctx ^. #storageDir) `shouldReturn` ["state.json"]

    describe "running vms from other flake files" $ do
      let fakeVmState =
            State.Running
              { port = 8080,
                pid = 42,
                ip = IPv4.fromOctets 10 0 0 42
              }
      it "prints running vms when there's no configured vms in the local flake file" $ do
        withMockContext [] $ \ctx -> do
          State.modifyState_ ctx (pure . (#vms %~ Map.insert "other" fakeVmState))
          result <- assertSuccess $ test ctx ["status"]
          result ^. #stdout `shouldBe` renderTable False [[("name", "other"), ("status", "running")]]

      it "prints running vms from other directories with configured vms" $ do
        withMockContext ["a"] $ \ctx -> do
          State.modifyState_ ctx (pure . (#vms %~ Map.insert "other" fakeVmState))
          result <- assertSuccess $ test ctx ["status"]
          result ^. #stdout
            `shouldBe` renderTable
              False
              [ [("name", "a"), ("status", "not running")],
                [("name", "other"), ("status", "running")]
              ]

  describe "list" $ do
    it "lists all configured vms" $ do
      withMockContext ["a", "b", "c"] $ \ctx -> do
        result <- assertSuccess $ test ctx ["list"]
        result ^. #stdout
          `shouldBe` cs
            ( unindent
                [i|
                  configured vms:
                    - a
                    - b
                    - c
                |]
            )

    it "has a nice message when no vms are configured" $ do
      withMockContext [] $ \ctx -> do
        result <- assertSuccess $ test ctx ["list"]
        result ^. #stdout `shouldBe` "no vms configured\n"

  describe "down" $ do
    it "stops vms" $ do
      withMockContext ["a"] $ \ctx -> do
        _ <- assertSuccess $ test ctx ["up", "a"]
        runningVms ctx `shouldReturn` ["a"]
        state <- readVmState ctx "a"
        test ctx ["down", "a"] `shouldReturn` TestResult "stopping a\n" "" ExitSuccess
        runningVms ctx `shouldReturn` []
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
        runningVms ctx `shouldReturn` ["a", "b", "c"]
        _ <- assertSuccess $ test ctx ["down", "a", "c"]
        runningVms ctx `shouldReturn` ["b"]

    it "stops all vms when no vm name given" $ do
      withMockContext ["a", "b"] $ \ctx -> do
        _ <- assertSuccess $ test ctx ["up", "a", "b"]
        runningVms ctx `shouldReturn` ["a", "b"]
        _ <- assertSuccess $ test ctx ["down"]
        runningVms ctx `shouldReturn` []

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
