module LoggerSpec where

import Control.Concurrent (threadDelay)
import Control.Monad (replicateM_)
import Data.Text.IO qualified as T
import Logger
import System.IO qualified
import System.IO.Silently (hCapture)
import Test.Hspec
import Test.Hspec.Golden (defaultGolden)
import TestUtils ()

spec :: Spec
spec = do
  describe "ANSILogger" $ do
    it "allows pushing log lines to stdout" $ do
      -- If the golden test fails, verify manually that the output looks good
      -- by changing this to True and running this test *outside* of ghcid
      let manualTesting = False
      let runTest :: IO () -> IO ()
          runTest delay = do
            withANSILogger $ \logger -> do
              mapM_
                (>> delay)
                [ setPhase logger "a" "building",
                  setPhase logger "b" "building",
                  setPhase logger "c" "building",
                  pushLog logger System.IO.stderr "log line",
                  setPhase logger "b" "starting",
                  pushLog logger System.IO.stderr "log line",
                  setPhase logger "a" "starting",
                  pushLog logger System.IO.stderr "this is a very long log line",
                  clearPhase logger "a",
                  setPhase logger "b" "starting",
                  pushLog logger System.IO.stderr "log line",
                  clearPhase logger "b",
                  pushLog logger System.IO.stderr "log line"
                ]
      if manualTesting
        then do
          replicateM_ 3 $ T.putStrLn ""
          runTest $ threadDelay 1_000_000
          replicateM_ 3 $ T.putStrLn ""
          pendingWith "Running ANSI logger tests manually"
          error "unreachable"
        else do
          (output, _) <- hCapture [System.IO.stdout, System.IO.stderr] $ do
            runTest $ pure ()
          pure $
            defaultGolden "ansi-logger" $
              unlines
                [ "This asserts stdout and stderr of the ansi logger",
                  "If these change it's best that you manually confirm that the logger still works well in a real terminal",
                  "",
                  output
                ]
