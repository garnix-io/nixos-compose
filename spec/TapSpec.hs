module TapSpec where

import Context
import Cradle qualified
import Data.Maybe (isJust)
import Data.String.Interpolate (i)
import Data.String.Interpolate.Util (unindent)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import State (readState)
import StdLib
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist, removeFile)
import System.Environment (getEnv)
import System.IO.Temp (withSystemTempDirectory)
import System.Posix (sigKILL, signalProcess)
import System.Process
import Test.Hspec
import Test.Mockery.Environment (withModifiedEnvironment)
import TestUtils
import Utils (which)
import Vde qualified

spec :: Spec
spec = do
  describe "tap" $ do
    it "allows connecting with vms from the host by ip address" $ do
      withMockContext ["server"] $ \ctx -> do
        withMockSudo $ \getMockSudoCalls -> do
          _ <- assertSuccess $ test ctx ["up", "server"]
          _ <- assertSuccess $ test ctx ["tap"]
          expected <- expectedCommands ctx
          getMockSudoCalls `shouldReturn` unlines (fmap unwords expected)

    it "prints a nice message when `sudo` is not in the `$PATH`" $ do
      sudo <- which "sudo"
      case sudo of
        Just _ -> do
          pendingWith "this test relies on `sudo` *not* being in the path"
        Nothing -> pure ()
      withMockContext ["server"] $ \ctx -> do
        _ <- assertSuccess $ test ctx ["up", "server"]
        expectedStdout <- expectedCommands ctx
        test ctx ["tap"]
          `shouldReturn` TestResult
            (cs $ unlines $ fmap unwords expectedStdout)
            "`sudo` not found in the $PATH, cannot create `tap` device.\nYou can run the following commands with elevated privileges to create it manually:\n\n"
            (ExitFailure 1)

    it "allows starting tap before starting vms" $ do
      withMockContext ["server"] $ \ctx -> do
        withMockSudo $ \getMockSudoCalls -> do
          _ <- assertSuccess $ test ctx ["tap"]
          expected <- expectedCommands ctx
          getMockSudoCalls `shouldReturn` unlines (fmap unwords expected)
          state <- readState ctx
          state ^. #vde `shouldSatisfy` isJust
          _ <- assertSuccess $ test ctx ["up", "server"]
          pure ()

    it "handles double starting the tap device gracefully" $ do
      withMockContext [] $ \ctx -> do
        withMockSudo $ \getMockSudoCalls -> do
          _ <- assertSuccess $ test ctx ["tap"]
          _ <- getMockSudoCalls
          test ctx ["tap"] `shouldReturn` TestResult "tap device already running\n" "" ExitSuccess
          getMockSudoCalls `shouldReturn` ""

    it "allows removing the tap device with vms running" $ do
      withMockContext ["server"] $ \ctx -> do
        withMockSudo $ \getMockSudoCalls -> do
          _ <- assertSuccess $ test ctx ["tap"]
          tapPid <- getTapPid ctx
          _ <- getMockSudoCalls
          _ <- assertSuccess $ test ctx ["up", "server"]
          _ <- assertSuccess $ test ctx ["tap", "--remove"]
          getMockSudoCalls `shouldReturn` ("kill -15 " <> show tapPid <> "\n")

    it "allows removing the tap device with no vms running" $ do
      withMockContext ["server"] $ \ctx -> do
        withMockSudo $ \getMockSudoCalls -> do
          _ <- assertSuccess $ test ctx ["tap"]
          tapPid <- getTapPid ctx
          _ <- getMockSudoCalls
          _ <- assertSuccess $ test ctx ["tap", "--remove"]
          getMockSudoCalls `shouldReturn` ("kill -15 " <> show tapPid <> "\n")

    it "allows re-starting the tap device after e.g. a reboot" $ do
      withMockContext [] $ \ctx -> do
        withMockSudo $ \getMockSudoCalls -> do
          _ <- assertSuccess $ test ctx ["tap"]
          _ <- getMockSudoCalls
          tapPid <- getTapPid ctx
          signalProcess sigKILL tapPid
          waitFor $ do
            doesDirectoryExist ("/proc/" <> show (tapPid :: ProcessID)) `shouldReturn` False
          _ <- assertSuccess $ test ctx ["tap"]
          expected <- expectedCommands ctx
          getMockSudoCalls `shouldReturn` unlines (fmap unwords expected)

    context "--dry-run" $ do
      it "has a --dry-run mode" $ do
        withMockContext ["server"] $ \ctx -> do
          withMockSudo $ \getMockSudoCalls -> do
            _ <- assertSuccess $ test ctx ["up", "server"]
            expectedStderr <- expectedCommands ctx
            result <- test ctx ["tap", "--dry-run"]
            getMockSudoCalls `shouldReturn` []
            result
              `shouldBe` TestResult
                (cs $ unlines $ fmap unwords expectedStderr)
                "Would run the following commands:\n\n"
                ExitSuccess

      it "allows showing removal commands with --dry-run" $ do
        withMockContext ["server"] $ \ctx -> do
          withMockSudo $ \getMockSudoCalls -> do
            _ <- assertSuccess $ test ctx ["tap"]
            _ <- getMockSudoCalls
            tapPid <- getTapPid ctx
            result <- test ctx ["tap", "--remove", "--dry-run"]
            getMockSudoCalls `shouldReturn` []
            result
              `shouldBe` TestResult
                ("kill -15 " <> cs (show tapPid) <> "\n")
                "Would run the following commands:\n\n"
                ExitSuccess

withMockSudo :: (IO String -> IO a) -> IO a
withMockSudo action = do
  withSystemTempDirectory "mock-sudo" $ \mockSudoDir -> do
    let mockSudoBinDir = mockSudoDir </> "bin"
    let mockSudoPath = mockSudoBinDir </> "sudo"
    createDirectoryIfMissing True mockSudoBinDir
    T.writeFile
      mockSudoPath
      ( T.strip $
          cs $
            unindent
              [i|
                #!/usr/bin/env python3

                import subprocess
                import sys

                args = " ".join(sys.argv[1:])
                with open("#{mockSudoDir}/calls", "a") as calls:
                  calls.write(args + "\\n")
                try:
                  pidFileFlagIndex = sys.argv.index("--pidfile")
                except ValueError:
                  pidFileFlagIndex = None
                if pidFileFlagIndex is not None:
                  pidFile = sys.argv[pidFileFlagIndex + 1]
                  process = subprocess.Popen(["sleep", "inf"])
                  with open(pidFile, "w") as file:
                    file.write(str(process.pid))
              |]
      )
    Cradle.run_ $ Cradle.cmd "chmod" & Cradle.addArgs ["+x", mockSudoPath]
    pathWithMockSudo <-
      getEnv "PATH"
        <&> ((mockSudoBinDir <> ":") <>)
    withModifiedEnvironment [("PATH", pathWithMockSudo)] $ do
      let getCalls = do
            exists <- doesFileExist (mockSudoDir </> "calls")
            if exists
              then do
                calls <- cs <$> T.readFile (mockSudoDir </> "calls")
                removeFile (mockSudoDir </> "calls")
                pure calls
              else pure ""
      action getCalls

expectedCommands :: Context -> IO [[String]]
expectedCommands ctx = do
  Paths {vde_plug2tap, ip, vdeCtlDir, pidFile} <- getPaths ctx
  pure
    [ [vde_plug2tap, "--daemon", "--pidfile", pidFile, "--sock", vdeCtlDir, "nixos-compose0"],
      [ip, "addr", "add", "10.0.0.1/24", "dev", "nixos-compose0"],
      [ip, "link", "set", "nixos-compose0", "up"]
    ]

getTapPid :: Context -> IO Pid
getTapPid ctx = do
  Paths {pidFile} <- getPaths ctx
  content <- readFile pidFile
  pure $ read content

data Paths = Paths
  { vde_plug2tap :: FilePath,
    ip :: FilePath,
    vdeCtlDir :: FilePath,
    pidFile :: FilePath
  }

getPaths :: Context -> IO Paths
getPaths ctx = do
  vde_plug2tap <-
    which "vde_plug2tap"
      <&> fromMaybe (error "vde_plug2tap not found in PATH")
  ip <-
    which "ip"
      <&> fromMaybe (error "ip not found in PATH")
  vdeCtlDir <- Vde.getVdeCtlDir ctx
  let pidFile = storageDir ctx </> "vde_plug2tap.pid"
  pure $ Paths {vde_plug2tap, ip, vdeCtlDir, pidFile}
