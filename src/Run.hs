module Run where

import Commands
import Commands.Up (up)
import Context (Context)
import Context.Production qualified
import Context.Utils (info)
import Control.Exception qualified
import Control.Exception.Safe (SomeException, fromException)
import Data.Text qualified
import Options
import Options.Applicative (execParserPure, handleParseResult, prefs, showHelpOnError)
import StdLib
import System.Environment (getArgs)

runInProduction :: IO ()
runInProduction = do
  args <- getArgs <&> fmap cs
  Context.Production.withContext $ \ctx -> do
    run ctx args >>= exitWith

run :: Context -> [Text] -> IO ExitCode
run ctx args =
  handleExceptions ctx $ do
    (Options opts) <- handleParseResult $ execParserPure (prefs showHelpOnError) parserInfo (cs <$> args)
    case opts of
      List -> list ctx
      Up verbosity vmNames -> up ctx verbosity vmNames
      Down vmNames -> down ctx vmNames
      Ssh vmName command -> ssh ctx vmName (Data.Text.unwords command)
      Status vmNames -> status ctx vmNames
      Ip vmName -> ip ctx vmName
      Tap removeFlag dryRunFlag -> tap ctx removeFlag dryRunFlag
    pure ExitSuccess

handleExceptions :: Context -> IO ExitCode -> IO ExitCode
handleExceptions ctx action = do
  -- handle all -- including async -- exceptions
  result <- Control.Exception.try action
  case result of
    Right exitCode -> pure exitCode
    Left (e :: SomeException) -> do
      case fromException e of
        Just (e :: ExitCode) -> pure e
        Nothing -> do
          info ctx (cs $ show e)
          pure (ExitFailure 1)
