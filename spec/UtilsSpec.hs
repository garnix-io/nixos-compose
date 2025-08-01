module UtilsSpec where

import StdLib
import Test.Hspec
import Utils (isValidHostname)

spec :: Spec
spec = do
  describe "isValidHostname" $ do
    let validHostnames =
          [ "valid",
            "WITH-CAPS",
            "ending-wtih-digits-123",
            "with-123-digits",
            "with-hyphen",
            "with.dot"
          ]

    let invalidHostnames =
          [ "",
            "123-starting-with-digits",
            "-starting-with-hyphen",
            ".starting-with-dot",
            "ending-with-dot.",
            "ending-with-hyphen-",
            "with spaces",
            "with--double-hyphen",
            "with?symbols",
            "with_underscore"
          ]

    describe "returns true for valid hostnames" $ forM_ validHostnames $ \valid ->
      it ("\"" <> cs valid <> "\"") $ isValidHostname valid `shouldBe` True

    describe "returns false for invalid hostnames" $ forM_ invalidHostnames $ \invalid ->
      it ("\"" <> cs invalid <> "\"") $ isValidHostname invalid `shouldBe` False
