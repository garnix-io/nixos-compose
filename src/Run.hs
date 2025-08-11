module Run where

import Commands
import Context (Context)
import Context.Production qualified
import Control.Exception qualified
import Control.Exception.Safe (SomeException, fromException)
import Data.Text hiding (elem)
import Options
import Options.Applicative
import StdLib
import System.Environment (getArgs)
import System.IO (hPrint, stderr)

runInProduction :: IO ()
runInProduction = do
  args <- getArgs <&> fmap cs
  ctx <- Context.Production.mkContext
  run ctx args >>= exitWith

run :: Context -> [Text] -> IO ExitCode
run ctx args =
  handleExceptions $ do
    (Options opts) <- handleParseResult $ execParserPure (prefs showHelpOnError) parserInfo (cs <$> args)
    case opts of
      List -> list ctx
      Up verbosity vmNames -> up ctx verbosity vmNames
      Down vmNames -> down ctx vmNames
      Ssh vmName command -> ssh ctx vmName (Data.Text.unwords command)
      Status vmNames -> status ctx vmNames
      Ip vmName -> ip ctx vmName
      Tap dryRunFlag -> tap ctx dryRunFlag
    pure ExitSuccess

handleExceptions :: IO ExitCode -> IO ExitCode
handleExceptions action = do
  -- handle all -- including async -- exceptions
  result <- Control.Exception.try action
  case result of
    Right exitCode -> pure exitCode
    Left (e :: SomeException) -> do
      case fromException e of
        Just e -> pure e
        Nothing -> do
          hPrint stderr e
          pure (ExitFailure 33)
