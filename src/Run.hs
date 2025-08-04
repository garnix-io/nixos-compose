module Run where

import Commands
import Context (Context)
import Context.Production qualified
import Control.Exception (SomeException, fromException)
import Control.Exception.Safe (try)
import Data.Text hiding (elem)
import Options
import Options.Applicative
import StdLib
import System.Environment (getArgs)
import System.Exit (exitWith)
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
      Down vmName -> down ctx vmName
      Ssh vmName command -> ssh ctx vmName (Data.Text.unwords command)
      Status vmNames -> status ctx vmNames
      Ip vmName -> ip ctx vmName
    pure ExitSuccess

handleExceptions :: IO ExitCode -> IO ExitCode
handleExceptions action = do
  result <- try action
  case result of
    Right exitCode -> pure exitCode
    Left (e :: SomeException) -> do
      case fromException e of
        Just e -> pure e
        Nothing -> do
          hPrint stderr e
          pure (ExitFailure 33)
