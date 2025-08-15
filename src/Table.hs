module Table (renderTable) where

import Data.Text qualified as T
import StdLib

renderTable :: [[(Text, Text)]] -> Text
renderTable = \case
  [] -> ""
  rows@(first : _) ->
    let headers = fmap fst first
        columnWidths = flip map headers $ \header ->
          max (T.length header) (maximum (map (maybe 0 T.length . lookup header) rows))
        renderLines start char sep end =
          start
            <> T.intercalate sep (map (\n -> T.replicate (n + 2) char) columnWidths)
            <> end
        renderRow (zip columnWidths -> row) =
          "│ " <> T.intercalate " │ " (map (uncurry pad) row) <> " │"
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

-- ─│┌┐└┘├┤┬┴┼

pad :: Int -> Text -> Text
pad n t = t <> T.replicate (n - T.length t) " "
