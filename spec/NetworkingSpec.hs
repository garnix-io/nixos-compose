module NetworkingSpec where

import Control.Concurrent (readMVar)
import Cradle
import Data.Maybe (fromJust)
import State (readState, readVmState)
import StdLib
import System.Directory (getSymbolicLinkTarget, listDirectory)
import System.FilePath
import Test.Hspec
import TestUtils

spec :: Spec
spec = do
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
