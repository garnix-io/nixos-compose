module TableSpec where

import StdLib
import System.Console.ANSI (Color (..))
import Table
import Test.Hspec
import Test.Hspec.Golden (defaultGolden)

spec :: Spec
spec = do
  let testCases =
        [ [ [("h", "a"), ("g", "foo")],
            [("h", "b"), ("g", "bar")]
          ],
          [ [("longer-1", "a"), ("longer-2", "foo")],
            [("longer-1", "b"), ("longer-2", "bar")]
          ],
          [ [("h", "longer-value"), ("g", "a")],
            [("h", "b"), ("g", "also-very-long")]
          ],
          [],
          [ [("h", "h1"), ("g", "g1")],
            [("h", "h2")]
          ],
          [ [("h", "h1"), ("g", "g1")],
            [("g", "g2")]
          ],
          [ [("h", "h1"), ("g", "g1"), ("i", "i1")],
            [("h", "h2"), ("i", "i2")],
            [("g", "g3"), ("h", "h3"), ("i", "i3")]
          ],
          [ [("h", "h1")],
            [("h", "h2"), ("g", "g2")]
          ]
        ]
  forM_ (zip [1 :: Int ..] testCases) $ \(i, testCase) -> do
    it ("renders table-" <> show i) $ do
      defaultGolden ("table-" <> show i) (cs $ renderTable False testCase)

  it "uses colors" $ do
    defaultGolden
      "table-colors"
      ( cs $
          renderTable
            True
            [ [("h", "a"), ("g", withColor Green "foo")],
              [("h", "b"), ("g", withColor Yellow "longer")]
            ]
      )
