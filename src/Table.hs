{-# LANGUAGE MultiParamTypeClasses #-}

module Table (StyledText (..), withColor, renderTable) where

import Data.String (IsString (..))
import Data.String.Conversions (ConvertibleStrings (..))
import Data.Text qualified as T
import StdLib
import System.Console.ANSI

data StyledText = StyledText
  { color :: Maybe Color,
    unstyledText :: Text
  }
  deriving stock (Generic)

instance ConvertibleStrings Text StyledText where
  convertString = StyledText Nothing

instance IsString StyledText where
  fromString = StyledText Nothing . cs

withColor :: Color -> Text -> StyledText
withColor color = StyledText (Just color)

renderStyledText :: Bool -> StyledText -> Text
renderStyledText useColor t = case (useColor, color t) of
  (True, Just color) ->
    cs (setSGRCode [SetColor Foreground Vivid color])
      <> unstyledText t
      <> cs (setSGRCode [Reset])
  _ -> unstyledText t

renderTable :: Bool -> [[(Text, StyledText)]] -> Text
renderTable useColors = \case
  [] -> ""
  rows@(first : _) ->
    let ansi codes t = if useColors then codes <> t <> reset else t
        headers = fmap fst first
        columnWidths = flip map headers $ \header ->
          max (T.length header) (maximum (map (maybe 0 (T.length . unstyledText) . lookup header) rows))
        renderLines start char sep end =
          ansi
            lineColor
            ( start
                <> T.intercalate sep (map (\n -> T.replicate (n + 2) char) columnWidths)
                <> end
            )
        renderRow (zip columnWidths -> row) =
          ansi lineColor "│"
            <> " "
            <> T.intercalate
              (" " <> ansi lineColor "│" <> " ")
              ( map
                  ( \(width, t) ->
                      renderStyledText useColors (t & #unstyledText %~ pad width)
                  )
                  row
              )
            <> ansi lineColor " │"
        topLine = [renderLines "┌" "─" "┬" "┐"]
        header = [renderRow $ map cs headers]
        divider = [renderLines "├" "─" "┼" "┤"]
        body = map (renderRow . map snd) rows
        bottomLine = [renderLines "└" "─" "┴" "┘"]
     in T.unlines $
          topLine
            <> header
            <> divider
            <> body
            <> bottomLine

lineColor :: Text
lineColor = cs (setSGRCode [SetColor Foreground Dull Blue])

reset :: Text
reset = cs $ setSGRCode [Reset]

pad :: Int -> Text -> Text
pad n t = t <> T.replicate (n - T.length t) " "
