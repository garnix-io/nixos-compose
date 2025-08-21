{-# OPTIONS_GHC -Wno-orphans #-}

module TestUtils where

import Context
import Control.Concurrent (newMVar, readMVar, threadDelay)
import Control.Exception.Safe (SomeException, finally, throwIO, try)
import Cradle qualified
import Data.Map qualified as Map
import Data.Maybe (fromJust, isJust)
import Data.String (IsString)
import Data.String.Conversions
import Data.Text qualified as T
import GHC.Clock (getMonotonicTime)
import GHC.Exts (IsString (..))
import Logger (mkSimpleLogger)
import Network.Socket.Free (getFreePort)
import Options (VmName (..))
import Run (run)
import State
import StdLib
import System.Environment (lookupEnv)
import System.IO (hPutStr)
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
        testState <- newMVar $ TestState mempty mempty
        logger <- mkSimpleLogger
        let ctx =
              Context
                { testState = Just testState,
                  stdin = stdinHandle,
                  workingDir = workingDir,
                  storageDir = storageDir </> "nixos-compose",
                  nixVms,
                  logger
                }
        action ctx `finally` endAllRegisteredProcesses ctx

readTestState :: Context -> IO TestState
readTestState ctx = readMVar (fromJust $ ctx ^. #testState)

endAllRegisteredProcesses :: Context -> IO ()
endAllRegisteredProcesses ctx = do
  testState <- readTestState ctx
  mapM_ endProcess (testState ^. #registeredProcesses)

endProcess :: ProcessHandle -> IO ()
endProcess handle = do
  terminateProcess handle
  _ <- waitForProcess handle
  pure ()

stopProcess :: Context -> ProcessType -> IO ()
stopProcess ctx typ = updateTestState ctx $ \testState -> do
  let map = testState ^. #registeredProcesses
  case Map.lookup typ map of
    Nothing ->
      error $
        cs $
          "cannot find executable: "
            <> cs (show typ)
            <> " in "
            <> T.intercalate ", " (fmap (cs . show) (Map.keys map))
    Just handle -> do
      endProcess handle
      pure $ testState & #registeredProcesses .~ Map.delete typ map

instance IsString VmName where
  fromString :: String -> VmName
  fromString = VmName . cs

withMockContext :: [VmName] -> (Context -> IO a) -> IO a
withMockContext vmNames action = do
  let mockNixVms =
        NixVms
          { listVms = \_ctx -> pure vmNames,
            buildVmScript = \_ctx _vmName _ip -> do
              port <- getFreePort
              pure ("/fake-vm-script", port),
            runVm =
              \_ctx _logLine vmName _vmScript -> do
                unless (vmName `elem` vmNames) $ do
                  error $ cs $ "nix vm mock: vm not found: " <> vmNameToText vmName
                (_, _, _, ph) <- do
                  createProcess
                    (proc "sleep" ["inf"])
                      { std_in = NoStream,
                        std_out = NoStream,
                        std_err = NoStream
                      }
                pure ph,
            sshIntoVm = SshIntoVm $ \ctx vmName _port command -> do
              unless (vmName `elem` vmNames) $ do
                error $ cs $ "nix vm mock: vm not found: " <> vmNameToText vmName
              _state <- State.readVmState ctx vmName
              withSystemTempDirectory "fake-ssh" $ \tempDir -> do
                Cradle.run $
                  Cradle.cmd "bash"
                    & Cradle.addArgs ["-c", command]
                    & Cradle.setWorkingDir tempDir,
            updateVmHostsEntry = \ctx vmName _port hostName ip -> do
              updateTestState ctx $ pure . (#vmHostEntries %~ Map.insert (vmName, hostName) ip)
          }
  withContext mockNixVms action

runningVms :: Context -> IO [VmName]
runningVms ctx = Map.keys <$> State.listRunningVms ctx

waitFor :: IO a -> IO a
waitFor action = do
  startTime <- getMonotonicTime
  inner startTime
  where
    inner startTime = do
      result :: Either SomeException a <- try action
      case result of
        Left e -> do
          now <- getMonotonicTime
          if now - startTime < 2
            then do
              threadDelay 50_000
              inner startTime
            else throwIO e
        Right a -> pure a
