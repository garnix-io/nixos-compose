module CliSpec where

import Data.String.Conversions (cs)
import StdLib
import Test.Hspec
import Test.Hspec.Golden (defaultGolden)
import TestUtils (test, withMockContext)

spec :: Spec
spec = do
  describe "help output" $ do
    it "outputs all commands when invoked without arguments" $ do
      withMockContext $ \ctx -> do
        result <- test ctx []
        pure $ defaultGolden "stderr" (cs (result ^. #stderr))
