module CliSpec where

import Data.String.Conversions (cs)
import RunSpec (test, withContext)
import StdLib
import Test.Hspec
import Test.Hspec.Golden (defaultGolden)

spec :: Spec
spec = do
  describe "help output" $ do
    it "outputs all commands when invoked without arguments" $ do
      withContext $ \ctx -> do
        result <- test ctx []
        pure $ defaultGolden "stderr" (cs (result ^. #stderr))
