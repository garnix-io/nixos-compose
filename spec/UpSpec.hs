module UpSpec where

import Context
import Control.Concurrent (MVar, modifyMVar_, myThreadId, newEmptyMVar, newMVar, putMVar, readMVar, threadDelay, throwTo)
import Control.Exception (AsyncException (..))
import Control.Monad (forever)
import Cradle
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Ki qualified
import Net.IPv4 qualified as IPv4
import State (VmState (..), getVmFilePath, readState, readVmState)
import StdLib
import System.IO qualified
import System.Process (CreateProcess (..), StdStream (..), createProcess, proc)
import Table (renderTable)
import Test.Hspec
import TestUtils

spec :: Spec
spec = do
  it "starts a vm" $ do
    withMockContext ["a"] $ \ctx -> do
      result <- assertSuccess $ test ctx ["up", "a"]
      result ^. #stderr `shouldBe` "a: building...\na: done building\na: booting...\na: done booting\n"

  it "starts vms in parallel" $ do
    let vmNames = ["a", "b", "c"]
    buildVmBarrier <- newBarrier (length vmNames)
    sshVmBarrier <- newBarrier (length vmNames)
    let mockNixVms =
          NixVms
            { listVms = \_ctx -> pure vmNames,
              buildVmScript = \_ctx _vmName _ip -> do
                waitBarrier buildVmBarrier
                pure ("/fake-vm-script", 1234),
              runVm =
                \_ctx _verbosity _vmName _vmScript -> do
                  (_, _, _, ph) <- do
                    createProcess
                      (proc "sleep" ["inf"])
                        { std_in = NoStream,
                          std_out = NoStream,
                          std_err = NoStream
                        }
                  pure ph,
              sshIntoVm = SshIntoVm $ \_ctx _vmName _port _command -> do
                waitBarrier sshVmBarrier
                Cradle.run $ Cradle.cmd "true",
              updateVmHostsEntry = \_ctx _vmName _port _hostName _ip -> pure ()
            }
    withContext mockNixVms $ \ctx -> do
      result <- assertSuccess $ test ctx ["up"]
      groupIntoSets [3, 6, 3] (T.lines $ result ^. #stderr)
        `shouldBe` map
          Set.fromList
          [ ["a: building...", "b: building...", "c: building..."],
            [ "a: done building",
              "a: booting...",
              "b: done building",
              "b: booting...",
              "c: done building",
              "c: booting..."
            ],
            ["a: done booting", "b: done booting", "c: done booting"]
          ]

  context "when no vm names are given" $ do
    it "starts all vms" $ do
      withMockContext ["a", "b", "c"] $ \ctx -> do
        _ <- assertSuccess $ test ctx ["up"]
        runningVms ctx `shouldReturn` ["a", "b", "c"]

    it "gives a nice message when no vms are defined" $ do
      withMockContext [] $ \ctx -> do
        result <- test ctx ["up"]
        result `shouldBe` TestResult "" "No vms are defined. Nothing to do.\n" (ExitFailure 1)

  describe "vm building state" $ do
    context "when the nix build blocks" $ do
      let blockingBuildVmScript :: Context -> Context
          blockingBuildVmScript =
            (#nixVms . #buildVmScript)
              .~ ( \_ctx _vmName _ip -> do
                     forever $ threadDelay 1_000_000
                 )
      it "locks the state of a vm when building the nixos config" $ do
        withMockContext ["a"] $ \(blockingBuildVmScript -> ctx) -> do
          Ki.scoped $ \scope -> do
            _ <- Ki.fork scope $ do
              test ctx ["up", "a"]
            waitFor $ do
              vmState <- readVmState ctx "a"
              vmState `shouldBe` Building {ip = IPv4.fromOctets 10 0 0 2}
              test ctx ["up", "a"] `shouldReturn` TestResult "a: already building\n" "" ExitSuccess
              test ctx ["status", "a"] `shouldReturn` TestResult (renderTable False [[("name", "a"), ("status", "building")]]) "" ExitSuccess
              test ctx ["ip", "a"] `shouldReturn` TestResult "10.0.0.2\n" "" ExitSuccess

      it "handles attempts to stop building vms gracefully" $ do
        withMockContext ["a"] $ \(blockingBuildVmScript -> ctx) -> do
          Ki.scoped $ \scope -> do
            _ <- Ki.fork scope $ do
              test ctx ["up", "a"]
            waitFor $ do
              test ctx ["down", "a"]
                `shouldReturn` TestResult
                  { stdout = "",
                    stderr = "a: building, cannot stop a building vm\n",
                    exitCode = ExitFailure 1
                  }

    it "locks the state of a vm when booting" $ do
      let blockingRunVm :: Context -> Context
          blockingRunVm =
            (#nixVms . #runVm)
              .~ ( \_ctx _verbosity _vmName _vmScript -> do
                     forever $ threadDelay 1_000_000
                 )
      withMockContext ["a"] $ \(blockingRunVm -> ctx) -> do
        Ki.scoped $ \scope -> do
          _ <- Ki.fork scope $ do
            test ctx ["up", "a"]
          waitFor $ do
            vmState <- readVmState ctx "a"
            vmState `shouldBe` Booting {ip = IPv4.fromOctets 10 0 0 2}
            test ctx ["up", "a"] `shouldReturn` TestResult "a: already booting\n" "" ExitSuccess
            test ctx ["status", "a"] `shouldReturn` TestResult (renderTable False [[("name", "a"), ("status", "booting")]]) "" ExitSuccess
            test ctx ["ip", "a"] `shouldReturn` TestResult "10.0.0.2\n" "" ExitSuccess

  describe "when nix evaluation fails" $ do
    let failingBuildVmScript :: Context -> Context
        failingBuildVmScript =
          #nixVms
            . #buildVmScript
            .~ ( \_ctx _vmName _ip -> do
                   T.hPutStrLn System.IO.stderr "test output"
                   exitWith $ ExitFailure 42
               )
    it "prints out the error message" $ do
      withMockContext ["a"] $ \(failingBuildVmScript -> ctx) -> do
        test ctx ["up", "a"] `shouldReturn` TestResult "" "a: building...\ntest output\n" (ExitFailure 42)

    it "doesn't add a vm to the state" $ do
      withMockContext ["a"] $ \(failingBuildVmScript -> ctx) -> do
        test ctx ["up", "a"] `shouldReturn` TestResult "" "a: building...\ntest output\n" (ExitFailure 42)
        (^. #vms) <$> readState ctx `shouldReturn` mempty
        (^. #vde) <$> readState ctx `shouldReturn` Nothing

    it "doesn't add a vm to the state when interrupted by an async exception, e.g. Ctrl-C" $ do
      blockingOnBuild <- newEmptyMVar
      let blockingBuildVmScript :: Context -> Context
          blockingBuildVmScript =
            (#nixVms . #buildVmScript)
              .~ ( \_ctx _vmName _ip -> do
                     putMVar blockingOnBuild ()
                     forever $ threadDelay 1_000_000
                 )
      withMockContext ["a"] $ \(blockingBuildVmScript -> ctx) -> do
        Ki.scoped $ \scope -> do
          threadId <- newEmptyMVar
          _ <- Ki.fork scope $ do
            myThreadId >>= putMVar threadId
            test ctx ["up", "a"]
          readMVar blockingOnBuild
          threadId <- readMVar threadId
          throwTo threadId UserInterrupt
          waitFor $ do
            (^. #vms) <$> readState ctx `shouldReturn` mempty
            (^. #vde) <$> readState ctx `shouldReturn` Nothing

  describe "when the vm script terminates unexpectedly" $ do
    let failingRunVm :: Int -> Context -> Context
        failingRunVm exitCode context =
          context
            & (#nixVms . #runVm)
            .~ ( \ctx _verbosity vmName _vmScript -> do
                   stdoutLog <- getVmFilePath ctx vmName "stdout.log"
                   T.writeFile stdoutLog "test stdout"
                   stderrLog <- getVmFilePath ctx vmName "stderr.log"
                   T.writeFile stderrLog "test stderr"
                   (_, _, _, ph) <- do
                     createProcess
                       (proc "bash" ["-c", "sleep 0.01; exit " <> show exitCode])
                         { std_in = NoStream,
                           std_out = NoStream,
                           std_err = NoStream
                         }
                   pure ph
               )
            & (#nixVms . #sshIntoVm)
            .~ SshIntoVm
              ( \_ctx _vmName _port _command -> do
                  threadDelay 10_000
                  Cradle.run $
                    Cradle.cmd "bash"
                      & Cradle.addArgs ["-c", "exit 255" :: Text]
              )

    it "shows the script output, when the script fails" $ do
      withMockContext ["a"] $ \(failingRunVm 42 -> ctx) -> do
        test ctx ["up", "a"]
          `shouldReturn` TestResult
            ""
            ( T.unlines
                [ "a: building...",
                  "a: done building",
                  "a: booting...",
                  "VM failed to start:",
                  "",
                  "test stdout",
                  "test stderr"
                ]
            )
            (ExitFailure 42)

    it "shows the script output, when the script exits with exit code 0" $ do
      withMockContext ["a"] $ \(failingRunVm 0 -> ctx) -> do
        test ctx ["up", "a"]
          `shouldReturn` TestResult
            ""
            ( T.unlines
                [ "a: building...",
                  "a: done building",
                  "a: booting...",
                  "VM failed to start:",
                  "",
                  "test stdout",
                  "test stderr"
                ]
            )
            (ExitFailure 1)

    it "cleans up the state.json file" $ do
      withMockContext ["a"] $ \(failingRunVm 0 -> ctx) -> do
        _ <- test ctx ["up", "a"]
        state <- readState ctx
        state ^. #vms `shouldBe` mempty
        state ^. #vde `shouldBe` Nothing

data Barrier = Barrier
  { target :: Int,
    signal :: MVar (),
    count :: MVar Int
  }

newBarrier :: Int -> IO Barrier
newBarrier target = Barrier target <$> newEmptyMVar <*> newMVar 0

waitBarrier :: Barrier -> IO ()
waitBarrier Barrier {target, signal, count} = do
  modifyMVar_ count $ \c -> do
    let new = c + 1
    when (new == target) $ putMVar signal ()
    pure new
  readMVar signal

groupIntoSets :: (Ord a) => [Int] -> [a] -> [Set a]
groupIntoSets ns list = case (ns, list) of
  (n : ns, list) ->
    let (group, rest) = splitAt n list
     in Set.fromList group : groupIntoSets ns rest
  ([], []) -> []
  _ -> error "groupIntoSets: didn't get right amount of elements"
