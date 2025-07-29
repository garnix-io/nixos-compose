{-# OPTIONS_GHC -Wno-orphans #-}

module TestUtils where

import Context
import Control.Concurrent (modifyMVar_, newMVar, readMVar)
import Control.Exception (finally)
import Cradle qualified
import Data.Maybe (fromJust, isJust)
import Data.String (IsString)
import Data.String.Conversions
import Data.Text qualified as T
import GHC.Exts (IsString (..))
import Network.Socket.Free (getFreePort)
import Options (VmName (..))
import Run (run)
import State
import StdLib
import System.Environment (lookupEnv)
import System.FilePath ((</>))
import System.IO (hPutStr)
import System.IO qualified
import System.IO.Silently
import System.IO.Temp (withSystemTempDirectory, withSystemTempFile)
import System.Process
import Test.Hspec
import Utils

data TestResult = TestResult
  { stdout :: Text,
    stderr :: Text,
    exitCode :: ExitCode
  }
  deriving stock (Generic, Show, Eq)

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
  debugEnvVar <- lookupEnv "DEBUG"
  when (isJust debugEnvVar) $ do
    putStr stdout
    hPutStr System.IO.stderr stderr
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

instance IsString VmName where
  fromString :: String -> VmName
  fromString = VmName . cs

withMockContext :: [VmName] -> (Context -> IO a) -> IO a
withMockContext vmNames action = do
  let mockNixVms =
        NixVms
          { listVms = \_ctx -> pure vmNames,
            buildAndRun =
              \ctx _verbosity vmName -> do
                unless (vmName `elem` vmNames) $ do
                  error $ cs $ "nix vm mock: vm not found: " <> vmNameToText vmName
                (_, _, _, ph) <- do
                  createProcess
                    (proc "sleep" ["inf"])
                      { std_in = NoStream,
                        std_out = NoStream,
                        std_err = NoStream
                      }
                port <- getFreePort
                pure (ph, port),
            sshIntoHost = \ctx vmName args -> do
              unless (vmName `elem` vmNames) $ do
                error $ cs $ "nix vm mock: vm not found: " <> vmNameToText vmName
              _state <- State.readVmState ctx vmName
              case args of
                [] -> error "nix vm mock: sshIntoHost: no args given"
                command : args ->
                  withSystemTempDirectory "fake-ssh" $ \tempDir -> do
                    -- this emulates ssh's behavior
                    Cradle.run $
                      Cradle.cmd "bash"
                        & Cradle.addArgs ["-c", T.unwords (command : args)]
                        & Cradle.setWorkingDir tempDir
          }
  withContext mockNixVms action
