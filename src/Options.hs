module Options
  ( parser,
    Options (..),
    Verbosity (..),
    Command (..),
    StartOptions (..),
    VmName (..),
  )
where

import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Text
import Options.Applicative
import StdLib

newtype Options = Options Command
  deriving stock (Show)

data Command
  = List
  | Start {verbosity :: Verbosity, options :: StartOptions}
  | Ssh {vmName :: VmName, sshCommand :: [Text]}
  | Status {vmNames :: [VmName]}
  | Stop {vmName :: VmName}
  deriving stock (Show, Generic)

data Verbosity
  = DefaultVerbosity
  | Verbose
  deriving stock (Show)

parseVerbosity :: Parser Verbosity
parseVerbosity =
  flag
    DefaultVerbosity
    Verbose
    ( long "verbose"
        <> short 'v'
        <> help "increase verbosity"
    )

data StartOptions
  = StartAll
  | StartSome (NonEmpty VmName)
  deriving stock (Show, Generic)

parseStartOptions :: Parser StartOptions
parseStartOptions =
  flag' StartAll (long "all")
    <|> (StartSome . NonEmpty.fromList <$> some parseVmName)

parser :: ParserInfo Options
parser =
  info p mempty
  where
    p =
      Options
        <$> hsubparser
          ( command
              "list"
              ( info
                  (pure List)
                  (fullDesc <> progDesc "List all configured vms")
              )
              <> command
                "start"
                ( info
                    (Start <$> parseVerbosity <*> parseStartOptions)
                    (fullDesc <> progDesc "Start a development vm")
                )
              <> command
                "ssh"
                ( info
                    (Ssh <$> parseVmName <*> many (argument str (metavar "SSH_COMMAND")))
                    (fullDesc <> progDesc "`ssh` into a running vm")
                )
              <> command
                "status"
                ( info
                    (Status <$> many parseVmName)
                    (fullDesc <> progDesc "Show the status of running vms")
                )
              <> command
                "stop"
                ( info
                    (Stop <$> parseVmName)
                    (progDesc "Stop a running vm")
                )
          )

newtype VmName = VmName {vmNameToText :: Text}
  deriving stock (Eq, Show, Ord)

parseVmName :: Parser VmName
parseVmName = VmName <$> argument str (metavar "VM_NAME")
