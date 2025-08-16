module Table (renderTable) where

import Data.Text qualified as T
import StdLib
import System.Console.ANSI

renderTable :: Bool -> [[(Text, Text)]] -> Text
renderTable useColors = \case
  [] -> ""
  rows@(first : _) ->
    let ansi codes t = if useColors then codes <> t <> reset else t
        headers = fmap fst first
        columnWidths = flip map headers $ \header ->
          max (T.length header) (maximum (map (maybe 0 T.length . lookup header) rows))
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
              (map (uncurry pad) row)
            <> ansi lineColor " │"
        topLine = [renderLines "┌" "─" "┬" "┐"]
        header = [renderRow headers]
        divider = [renderLines "├" "─" "┼" "┤"]
        body = map (renderRow . map snd) rows
        bottomLine = [renderLines "└" "─" "┴" "┘"]
     in T.unlines $
          map
            T.stripEnd
            ( topLine
                <> header
                <> divider
                <> body
                <> bottomLine
            )

lineColor :: Text
lineColor = cs (setSGRCode [SetColor Foreground Dull Blue])

reset :: Text
reset = cs $ setSGRCode [Reset]

pad :: Int -> Text -> Text
pad n t = t <> T.replicate (n - T.length t) " "
