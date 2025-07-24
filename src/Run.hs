module Run where

import Commands
import Context
import Context.Production qualified
import Control.Exception (SomeException, fromException)
import Control.Exception.Safe (try)
import Data.String.Conversions (cs)
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
    (Options opts) <- handleParseResult $ execParserPure (prefs showHelpOnError) parser (cs <$> args)
    case opts of
      List -> list ctx
      Start vmNames -> start ctx vmNames
      Stop vmName -> stop ctx vmName
      Ssh vmName command -> ssh ctx vmName command
      Status vmNames -> status ctx vmNames
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
