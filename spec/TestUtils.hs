module TestUtils where

import Context
import Control.Concurrent (modifyMVar_, newMVar, readMVar)
import Control.Exception (finally)
import Cradle qualified
import Data.String.Conversions
import Network.Socket.Free (getFreePort)
import Run (run)
import State
import StdLib
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO qualified
import System.IO.Silently
import System.IO.Temp (withSystemTempDirectory, withSystemTempFile)
import System.Process
import Test.Hspec

data TestResult = TestResult
  { stdout :: Text,
    stderr :: Text,
    exitCode :: ExitCode
  }
  deriving stock (Generic, Show)

assertSuccess :: (HasCallStack) => IO TestResult -> IO TestResult
assertSuccess action = do
  result <- action
  case result ^. #exitCode of
    ExitFailure code ->
      expectationFailure $
        cs $
          "command exited with "
            <> cs (show code)
            <> "\nStdout: \n"
            <> stdout result
            <> "\nStderr: \n"
            <> stderr result
    ExitSuccess -> pure ()
  pure result

test :: Context -> [Text] -> IO TestResult
test ctx args = do
  (stderr, (stdout, exitCode)) <- hCapture [System.IO.stderr] $ capture $ run ctx args
  pure $ TestResult (cs stdout) (cs stderr) exitCode

withContext :: NixVms -> (Context -> IO a) -> IO a
withContext nixVms action = do
  withSystemTempFile "test-stdin" $ \_stdinFile stdinHandle -> do
    withSystemTempDirectory "test-working-dir" $ \workingDir -> do
      withSystemTempDirectory "test-storage-dir" $ \storageDir -> do
        processHandles <- newMVar []
        let ctx =
              Context
                { registerProcess = \handle -> modifyMVar_ processHandles $ \h -> pure $ h <> [handle],
                  stdin = stdinHandle,
                  workingDir = workingDir,
                  storageDir = storageDir </> "vmcli",
                  nixVms
                }
        action ctx
          `finally` (readMVar processHandles >>= mapM_ endProcess)

endProcess :: ProcessHandle -> IO ExitCode
endProcess handle = do
  terminateProcess handle
  waitForProcess handle

withMockContext :: (Context -> IO a) -> IO a
withMockContext action = do
  let mockNixVms =
        NixVms
          { buildAndRun =
              \ctx vmName -> do
                (_, _, _, ph) <- do
                  createProcess
                    (proc "sleep" ["inf"])
                      { std_in = NoStream,
                        std_out = NoStream,
                        std_err = NoStream
                      }
                port <- getFreePort
                State.writeState ctx vmName (VmState {pid = Nothing, port})
                pure ph,
            sshIntoHost = \ctx vmName args -> do
              state <- State.readState ctx vmName
              case state ^. #pid of
                Nothing -> error "sshIntoHost': mock vm not running"
                Just _pid -> case args of
                  [] -> error "sshIntoHost': no args given"
                  command : args ->
                    withSystemTempDirectory "fake-ssh" $ \tempDir -> do
                      Cradle.run $
                        Cradle.cmd (cs command)
                          & Cradle.addArgs args
                          & Cradle.setWorkingDir tempDir
          }
  withContext mockNixVms action
