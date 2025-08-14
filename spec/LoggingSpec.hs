module LoggingSpec where

import Control.Exception.Safe (try)
import Data.Text qualified as T
import Logging
import StdLib
import System.IO qualified
import System.IO.Silently (hCapture)
import Test.Hspec
import TestUtils (withMockContext)

spec :: Spec
spec = do
  describe "impossible" $ around (withMockContext []) $ do
    it "throws" $ \ctx -> do
      impossible ctx "foo" `shouldThrow` (== ExitFailure 1)

    it "produces a nice error message" $ \ctx -> do
      (output, _) :: (String, Either ExitCode ()) <- hCapture [System.IO.stderr] $ try $ impossible ctx "test message"
      cs output
        `shouldBe` T.unlines
          [ "nixos-compose encountered an unexpected error: test message",
            "Please, consider reporting this as a bug here: https://github.com/garnix-io/nixos-compose/issues",
            "",
            "callstack:",
            "CallStack (from HasCallStack):",
            "  impossible, called at spec/LoggingSpec.hs:19:90 in nixos-compose-0.0.0-inplace-spec:LoggingSpec"
          ]
