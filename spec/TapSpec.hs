module TapSpec where

import Context
import Cradle qualified
import Data.String.Interpolate (i)
import Data.String.Interpolate.Util (unindent)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import StdLib
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.Environment (getEnv)
import System.IO.Temp (withSystemTempDirectory)
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
        withMockSudo $ \(sudo, getMockSudoCalls) -> do
          _ <- assertSuccess $ test ctx ["up", "server"]
          _ <- assertSuccess $ test ctx ["tap"]
          Paths {vde_plug2tap, ip, vdeCtlDir} <- getPaths ctx
          let expected =
                [ [sudo, vde_plug2tap, "--daemon", "--sock", vdeCtlDir, "nixos-compose0"],
                  [sudo, ip, "addr", "add", "10.0.0.1/24", "dev", "nixos-compose0"],
                  [sudo, ip, "link", "set", "nixos-compose0", "up"]
                ]
          getMockSudoCalls `shouldReturn` unlines (fmap unwords expected)

    it "prints a nice message when `sudo` is not in the `$PATH`" $ do
      sudo <- which "sudo"
      case sudo of
        Just _ -> do
          pendingWith "this test relies on `sudo` *not* being in the path"
        Nothing -> pure ()
      withMockContext ["server"] $ \ctx -> do
        _ <- assertSuccess $ test ctx ["up", "server"]
        Paths {vde_plug2tap, ip, vdeCtlDir} <- getPaths ctx
        let expectedStdout =
              [ [vde_plug2tap, "--daemon", "--sock", vdeCtlDir, "nixos-compose0"],
                [ip, "addr", "add", "10.0.0.1/24", "dev", "nixos-compose0"],
                [ip, "link", "set", "nixos-compose0", "up"]
              ]
        test ctx ["tap"]
          `shouldReturn` TestResult
            (cs $ unlines $ fmap unwords expectedStdout)
            "`sudo` not found in the $PATH, cannot create `tap` device.\nYou can run the following commands with elevated privileges to create it manually:\n\n"
            (ExitFailure 1)

    it "errors when the vde switch is not running" $ do
      withMockContext ["server"] $ \ctx -> do
        withMockSudo $ \(_, getMockSudoCalls) -> do
          test ctx ["tap"]
            `shouldReturn` TestResult
              ""
              "Cannot start `tap` device with no VMs running\n"
              (ExitFailure 1)
          getMockSudoCalls `shouldReturn` []

    it "has a --dry-run mode" $ do
      withMockContext ["server"] $ \ctx -> do
        withMockSudo $ \(_, getMockSudoCalls) -> do
          _ <- assertSuccess $ test ctx ["up", "server"]
          Paths {vde_plug2tap, ip, vdeCtlDir} <- getPaths ctx
          let expectedStderr =
                [ [vde_plug2tap, "--daemon", "--sock", vdeCtlDir, "nixos-compose0"],
                  [ip, "addr", "add", "10.0.0.1/24", "dev", "nixos-compose0"],
                  [ip, "link", "set", "nixos-compose0", "up"]
                ]
          result <- test ctx ["tap", "--dry-run"]
          getMockSudoCalls `shouldReturn` []
          result
            `shouldBe` TestResult
              (cs $ unlines $ fmap unwords expectedStderr)
              "Would run the following commands:\n\n"
              ExitSuccess

    it "bug" $ do
      withMockContext ["server"] $ \ctx -> do
        withMockSudo $ \_ -> do
          _ <- assertSuccess $ test ctx ["up", "server"]
          stopProcess ctx (Vm "server")
          test ctx ["tap", "--dry-run"]
            `shouldReturn` TestResult "" "WARN: cannot find process for vm: server\nCannot start `tap` device with no VMs running\n" (ExitFailure 1)

withMockSudo :: ((FilePath, IO String) -> IO a) -> IO a
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

                import sys

                args = " ".join(sys.argv)
                with open("#{mockSudoDir}/calls", "a") as calls:
                  calls.write(args + "\\n")
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
                cs <$> T.readFile (mockSudoDir </> "calls")
              else pure ""
      action (mockSudoPath, getCalls)

data Paths = Paths
  { vde_plug2tap :: FilePath,
    ip :: FilePath,
    vdeCtlDir :: FilePath
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
  pure $ Paths {vde_plug2tap, ip, vdeCtlDir}
