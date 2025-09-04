module LoggingSpec where

import Control.Exception.Safe (try)
import Logging
import StdLib
import System.IO qualified
import System.IO.Silently (hCapture, hSilence)
import Test.Hspec
import Test.Hspec.Golden (defaultGolden)
import TestUtils (withMockContext)

spec :: Spec
spec = do
  describe "impossible" $ around (withMockContext []) $ do
    it "throws" $ \ctx -> do
      hSilence [System.IO.stderr] $ do
        impossible ctx "foo" `shouldThrow` (== ExitFailure 1)

    it "produces a nice error message" $ \ctx -> do
      (output, _) :: (String, Either ExitCode ()) <- hCapture [System.IO.stderr] $ try $ impossible ctx "test message"
      pure $ defaultGolden "impossible-message" output
