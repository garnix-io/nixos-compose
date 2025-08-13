module LoggingSpec where

import Control.Concurrent (threadDelay)
import Control.Exception.Safe (try)
import Control.Monad (replicateM_)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Logging
import StdLib
import System.IO qualified
import System.IO.Silently (hCapture)
import Test.Hspec
import Test.Hspec.Golden (defaultGolden)
import TestUtils ()

spec :: Spec
spec = do
  describe "impossible" $ do
    it "throws" $ do
      impossible "foo" `shouldThrow` (== ExitFailure 1)

    it "produces a nice error message" $ do
      (output, _) :: (String, Either ExitCode ()) <- hCapture [System.IO.stderr] $ try $ impossible "test message"
      cs output
        `shouldBe` T.unlines
          [ "nixos-compose encountered an unexpected error: test message",
            "Please, consider reporting this as a bug here: https://github.com/garnix-io/nixos-compose/issues",
            "",
            "callstack:",
            "CallStack (from HasCallStack):",
            "  impossible, called at spec/LoggingSpec.hs:19:80 in nixos-compose-0.0.0-inplace-spec:LoggingSpec"
          ]

  describe "ANSILogger" $ do
    fit "allows pushing log lines to stdout" $ do
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
