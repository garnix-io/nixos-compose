module UtilsSpec where

import StdLib
import Test.Hspec
import Utils (hostnameToText, parseHostname)

spec :: Spec
spec = do
  describe "parseHostname" $ do
    let validHostnames =
          [ "valid",
            "WITH-CAPS",
            "ending-with-digits-123",
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
            "with'symbols",
            "with?symbols",
            "with_underscore"
          ]

    describe "returns Just Hostname for valid hostnames" $ forM_ validHostnames $ \valid ->
      it ("\"" <> cs valid <> "\"") $ do
        case parseHostname valid of
          Nothing -> error "expected Just Hostname"
          Just hostname -> hostnameToText hostname `shouldBe` valid

    describe "returns Nothing for invalid hostnames" $ forM_ invalidHostnames $ \invalid ->
      it ("\"" <> cs invalid <> "\"") $ parseHostname invalid `shouldBe` Nothing
