module NetworkingSpec where

import Context
import Control.Concurrent (readMVar)
import Control.Monad (replicateM)
import Cradle
import Data.Maybe (fromJust)
import Net.IPv4 qualified as IPv4
import State (getNextIp, readState, readVmState)
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
          exe <- getSymbolicLinkTarget $ "/proc" </> show (state ^. #vde . to fromJust . #pid :: Int64) </> "exe"
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
        state <- readState ctx
        _ <- assertSuccess $ test ctx ["stop", "a"]
        (^. #vde) <$> readState ctx `shouldReturn` Nothing
        (StdoutRaw stdout) <- Cradle.run $ cmd "ps" & addArgs ["-p", show (state ^. #vde . to fromJust . #pid), "-o", "stat", "--no-headers"]
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
        (^. #vde) <$> readState ctx `shouldReturn` Nothing

    it "restarts the switch after e.g. a reboot" $ do
      withMockContext ["a", "b"] $ \ctx -> do
        _ <- assertSuccess $ test ctx ["start", "a"]
        readMVar (fromJust (ctx ^. #registeredProcesses)) >>= mapM_ endProcess
        _ <- assertSuccess $ test ctx ["start", "a"]
        assertVdeIsRunning ctx
        assertVmIsRunning ctx "a"

  describe "ip assignments" $ do
    it "assigns an ip to vms" $ do
      withMockContext ["a"] $ \ctx -> do
        _ <- assertSuccess $ test ctx ["start", "a"]
        output <- assertSuccess $ test ctx ["ip", "a"]
        output ^. #stdout `shouldBe` "10.0.0.2\n"

    it "handles non-running vms gracefully" $ do
      withMockContext ["a"] $ \ctx -> do
        test ctx ["ip", "a"]
          `shouldReturn` TestResult "" "vm not running: a\n" (ExitFailure 1)
        _ <- assertSuccess $ test ctx ["start", "a"]
        stopProcess ctx (Vm "a")
        test ctx ["ip", "a"]
          `shouldReturn` TestResult
            { stdout = "WARN: cannot find process for vm: a\n",
              stderr = "vm not running: a\n",
              exitCode = ExitFailure 1
            }

    it "assigns different ips to different vms" $ do
      withMockContext ["a", "b", "c"] $ \ctx -> do
        _ <- assertSuccess $ test ctx ["start", "a"]
        _ <- assertSuccess $ test ctx ["start", "b"]
        _ <- assertSuccess $ test ctx ["start", "c"]
        output <- assertSuccess $ test ctx ["ip", "a"]
        output ^. #stdout `shouldBe` "10.0.0.2\n"
        output <- assertSuccess $ test ctx ["ip", "b"]
        output ^. #stdout `shouldBe` "10.0.0.3\n"
        output <- assertSuccess $ test ctx ["ip", "c"]
        output ^. #stdout `shouldBe` "10.0.0.4\n"

    it "tries not to re-assign vms" $ do
      withMockContext ["a", "b", "c"] $ \ctx -> do
        _ <- assertSuccess $ test ctx ["start", "a"]
        stdout <$> assertSuccess (test ctx ["ip", "a"]) `shouldReturn` "10.0.0.2\n"
        _ <- assertSuccess $ test ctx ["start", "b"]
        stdout <$> assertSuccess (test ctx ["ip", "b"]) `shouldReturn` "10.0.0.3\n"
        _ <- assertSuccess $ test ctx ["stop", "b"]
        _ <- assertSuccess $ test ctx ["start", "b"]
        stdout <$> assertSuccess (test ctx ["ip", "b"]) `shouldReturn` "10.0.0.4\n"
        _ <- assertSuccess $ test ctx ["stop", "b"]
        _ <- assertSuccess $ test ctx ["start", "c"]
        stdout <$> assertSuccess (test ctx ["ip", "c"]) `shouldReturn` "10.0.0.5\n"

    it "wraps around and doesn't assign ips that are in use" $ do
      withMockContext ["a"] $ \ctx -> do
        _ <- assertSuccess $ test ctx ["start", "a"]
        ips <- replicateM 251 $ do
          getNextIp ctx
        ips `shouldBe` [IPv4.fromOctets 10 0 0 3 .. IPv4.fromOctets 10 0 0 253]
        ips <- replicateM 3 $ do
          IPv4.encode <$> getNextIp ctx
        ips `shouldBe` ["10.0.0.254", "10.0.0.3", "10.0.0.4"]
