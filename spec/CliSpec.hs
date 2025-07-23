module CliSpec where

import Data.String.Conversions (cs)
import StdLib
import Test.Hspec
import Test.Hspec.Golden (defaultGolden)
import TestUtils

spec :: Spec
spec = do
  describe "help output" $ do
    it "outputs all commands when invoked without arguments" $ do
      withMockContext $ \ctx -> do
        result <- test ctx []
        pure $ defaultGolden "stderr" (cs (result ^. #stderr))

  it "`status` without argument lists the status of all vms" $ do
    withMockContext $ \ctx -> do
      _ <- assertSuccess $ test ctx ["start", "a"]
      _ <- assertSuccess $ test ctx ["start", "b"]
      result <- assertSuccess $ test ctx ["status"]
      result ^. #stdout `shouldBe` "a: running\nb: running\n"

  it "`status` accepts multiple vm names" $ do
    withMockContext $ \ctx -> do
      _ <- assertSuccess $ test ctx ["start", "a"]
      _ <- assertSuccess $ test ctx ["start", "b"]
      _ <- assertSuccess $ test ctx ["start", "c"]
      result <- assertSuccess $ test ctx ["status", "a", "c"]
      result ^. #stdout `shouldBe` "a: running\nc: running\n"
      result <- assertSuccess $ test ctx ["status", "c", "b"]
      result ^. #stdout `shouldBe` "c: running\nb: running\n"
      result <- assertSuccess $ test ctx ["status", "b", "d"]
      result ^. #stdout `shouldBe` "b: running\nd: not running\n"
