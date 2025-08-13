module Logger where

import Control.Concurrent
import Control.Exception (bracket)
import Control.Exception.Safe (finally)
import Data.Map qualified as Map
import Data.Text qualified as T
import Data.Text.IO qualified as T
import GHC.IO.Handle
import Options
import StdLib
import System.Console.ANSI qualified as ANSI
import System.IO qualified

data Logger = Logger
  { pushLog :: Handle -> Text -> IO (),
    setPhase :: VmName -> Text -> IO (),
    clearPhase :: VmName -> IO ()
  }
  deriving stock (Generic)

withAutoLogger :: (Logger -> IO ()) -> IO ()
withAutoLogger action = do
  ansiSupport <- and <$> mapM ANSI.hNowSupportsANSI [System.IO.stdout, System.IO.stderr]
  if ansiSupport then withANSILogger action else action =<< mkSimpleLogger

-- * ANSI logger

newtype ANSILogger = ANSILogger {statusLine :: MVar (Maybe Text)}
  deriving stock (Generic)

withNoBuffering :: Handle -> IO a -> IO a
withNoBuffering handle action =
  bracket
    (hGetBuffering handle <* hSetBuffering handle NoBuffering)
    (hSetBuffering handle)
    $ \_ -> do
      action

withANSILogger :: (Logger -> IO a) -> IO a
withANSILogger action =
  withNoBuffering System.IO.stdout $ do
    withNoBuffering System.IO.stderr $ do
      phases <- newMVar mempty
      let renderPhaseLine :: VmName -> Text -> Text
          renderPhaseLine vmName phase =
            cs (ANSI.setSGRCode [ANSI.SetConsoleIntensity ANSI.BoldIntensity])
              <> vmNameToText vmName
              <> cs (ANSI.setSGRCode [ANSI.SetConsoleIntensity ANSI.NormalIntensity])
              <> ": "
              <> phase
              <> cs ANSI.clearFromCursorToLineEndCode
      let renderPhases :: Map.Map VmName Text -> Text
          renderPhases phases = do
            phases
              & Map.toList
              & fmap (uncurry renderPhaseLine)
              & T.unlines
      let updatePhases :: (Map.Map VmName Text -> Map.Map VmName Text) -> IO ()
          updatePhases updater = do
            modifyMVar_ phases $ \previousPhases -> do
              let newPhases = updater previousPhases
              let prevCount = length previousPhases
              let newCount = length newPhases
              if prevCount > newCount
                then do
                  T.hPutStr System.IO.stderr $
                    cs (ANSI.scrollPageDownCode (prevCount - newCount))
                      <> cs (ANSI.cursorUpLineCode newCount)
                      <> renderPhases newPhases
                      <> cs ANSI.clearFromCursorToLineEndCode
                else do
                  T.hPutStr System.IO.stderr $
                    cs (ANSI.cursorUpLineCode prevCount)
                      <> renderPhases newPhases
              pure newPhases
      let logger =
            Logger
              { pushLog = \handle text -> do
                  modifyMVar_ phases $ \p -> do
                    ANSI.hCursorUpLine System.IO.stderr $ length p
                    T.hPutStr handle text
                    ANSI.hClearFromCursorToLineEnd System.IO.stderr
                    T.hPutStrLn handle ""
                    T.hPutStr System.IO.stderr $ renderPhases p
                    pure p,
                setPhase = \vmName phase -> do
                  updatePhases $ Map.insert vmName phase,
                clearPhase = \vmName -> do
                  updatePhases $ Map.delete vmName
              }
      let clearAllPhases = updatePhases $ const mempty
      action logger `finally` clearAllPhases

-- * Simple logger

mkSimpleLogger :: IO Logger
mkSimpleLogger = do
  currentPhases <- newMVar mempty
  let logDone :: VmName -> Map.Map VmName Text -> IO ()
      logDone vmName phases = do
        case Map.lookup vmName phases of
          Nothing -> pure ()
          Just phase -> T.hPutStrLn System.IO.stderr $ vmNameToText vmName <> ": done " <> phase
  pure $
    Logger
      { setPhase = \vmName phase -> do
          modifyMVar_ currentPhases $ \phases -> do
            logDone vmName phases
            T.hPutStrLn System.IO.stderr $ vmNameToText vmName <> ": " <> phase <> "..."
            pure $ Map.insert vmName phase phases,
        clearPhase = \vmName -> do
          modifyMVar_ currentPhases $ \phases -> do
            logDone vmName phases
            pure $ Map.delete vmName phases,
        pushLog = T.hPutStrLn
      }
