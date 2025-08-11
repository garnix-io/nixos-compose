module LoggingSpec where

import Control.Exception.Safe (try)
import Data.Text qualified as T
import Logging
import StdLib
import System.IO (stderr)
import System.IO.Silently (hCapture)
import Test.Hspec

spec :: Spec
spec = do
  describe "impossible" $ do
    it "throws" $ do
      impossible "foo" `shouldThrow` (== ExitFailure 1)

    it "produces a nice error message" $ do
      (output, _) :: (String, Either ExitCode ()) <- hCapture [stderr] $ try $ impossible "test message"
      cs output
        `shouldBe` T.unlines
          [ "nixos-compose encountered an unexpected error: test message",
            "Please, consider reporting this as a bug here: https://github.com/garnix-io/nixos-compose/issues",
            "",
            "callstack:",
            "CallStack (from HasCallStack):",
            "  impossible, called at spec/LoggingSpec.hs:18:80 in nixos-compose-0.0.0-inplace-spec:LoggingSpec"
          ]
