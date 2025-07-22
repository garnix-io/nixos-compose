module TestUtils where

import Context
import Control.Concurrent (modifyMVar_, newMVar, readMVar)
import Control.Exception (finally)
import Control.Monad
import Data.String.Conversions
import NixVms qualified
import Run (run)
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
  when (result ^. #exitCode /= ExitSuccess) $ do
    expectationFailure $ "command exited with " <> show result
  pure result

test :: Context -> [Text] -> IO TestResult
test ctx args = do
  (stderr, (stdout, exitCode)) <- hCapture [System.IO.stderr] $ capture $ run ctx args
  pure $ TestResult (cs stdout) (cs stderr) exitCode

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
                  storageDir = storageDir </> "vmcli",
                  nixVms = NixVms.production
                }
        action ctx
          `finally` (readMVar processHandles >>= mapM_ endProcess)

endProcess :: ProcessHandle -> IO ExitCode
endProcess handle = do
  terminateProcess handle
  waitForProcess handle
