module Utils where

import Debug.Trace qualified
import System.IO (hPrint, stderr)

dbg :: (Show a) => a -> IO ()
dbg = hPrint stderr

trace :: (Show a) => a -> a
trace = Debug.Trace.traceShowId
