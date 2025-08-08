module Vde where

import Context
import Control.Exception.Safe (SomeException, try)
import Cradle qualified
import Data.Aeson (FromJSON, ToJSON)
import Data.Text qualified as T
import Logging
import Net.IPv4 (IPv4)
import Net.IPv4 qualified as IPv4
import Options
import StdLib
import System.Directory (createDirectoryIfMissing, removeDirectoryRecursive)
import System.Posix (sigKILL, signalProcess)
import System.Process
import Utils (which)

newtype VdeState = VdeState
  { pid :: ProcessID
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (ToJSON, FromJSON)

start :: Context -> IO VdeState
start ctx = do
  ctlDir <- getVdeCtlDir ctx
  (stdinPipe, _) <- createPipe
  (_, _, _, handle) <-
    createProcess
      (System.Process.proc "vde_switch" ["--sock", ctlDir, "--dirmode", "0700", "--hub"])
        { std_in = UseHandle stdinPipe -- `CreatePipe :: StdStream` doesn't work reliably
        }
  registerProcess ctx VdeSwitch handle
  pid <- System.Process.getPid handle <&> fromMaybe (error "no pid")
  pure $ VdeState {pid}

stop :: Context -> VdeState -> IO ()
stop ctx state = do
  _ :: Either SomeException () <- try $ signalProcess sigKILL $ state ^. #pid
  removeDirectoryRecursive =<< getVdeCtlDir ctx

getVdeCtlDir :: Context -> IO FilePath
getVdeCtlDir ctx = do
  let ctlDir = storageDir ctx </> "vde_switch.ctl"
  createDirectoryIfMissing True ctlDir
  pure ctlDir

-- We have some golden tests for the issued commands (in `TapSpec`), but when
-- changing the sudo process invokations, you should manually make sure that
-- setting up a `tap` device still works. With e.g.:
--
-- - nix shell .#default
-- - nixos-compose up test-vm
-- - nixos-compose tap
-- - curl $(nixos-compose ip test-vm)
-- - nixos-compose down test-vm
setupTapDevice :: Context -> DryRunFlag -> IPv4 -> IO ()
setupTapDevice ctx dryRunFlag ipAddress = do
  commands <- do
    vdeCtlDir <- getVdeCtlDir ctx
    vde_plug2tapPath <-
      which "vde_plug2tap"
        <&> fromMaybe (error "impossible: vde_plug2tap not in path")
    ipPath <-
      which "ip"
        <&> fromMaybe (error "impossible: ip not in path")
    pure
      [ [cs vde_plug2tapPath, "--daemon", "--sock", cs vdeCtlDir, "nixos-compose0"],
        [cs ipPath, "addr", "add", IPv4.encode ipAddress <> "/24", "dev", "nixos-compose0" :: Text],
        [cs ipPath, "link", "set", "nixos-compose0", "up" :: Text]
      ]
  sudo <- which "sudo"
  case sudo of
    Nothing -> do
      exitWith
        [ ToStderr
            ( T.unlines
                [ "`sudo` not found in the $PATH, cannot create `tap` device.",
                  "You can run the following commands with elevated privileges to create it manually:",
                  ""
                ]
            ),
          ToStdout (T.unlines $ fmap T.unwords commands)
        ]
        (ExitFailure 1)
    Just _ -> case dryRunFlag of
      DryRun -> do
        exitWith
          [ ToStderr
              ( T.unlines
                  [ "Would run the following commands:",
                    ""
                  ]
              ),
            ToStdout (T.unlines $ fmap T.unwords commands)
          ]
          ExitSuccess
      NoDryRun -> forM_ commands runWithSudo

runWithSudo :: [Text] -> IO ()
runWithSudo args =
  Cradle.run_ $
    Cradle.cmd "sudo"
      & Cradle.addArgs args
