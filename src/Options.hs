module Options
  ( parserInfo,
    Options (..),
    Verbosity (..),
    Command (..),
    StartOptions (..),
    VmName (..),
  )
where

import Data.Aeson (FromJSONKey, ToJSONKey)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Text
import Options.Applicative
import StdLib

parserInfo :: ParserInfo Options
parserInfo =
  info parser mempty

class Parseable a where
  parser :: Parser a

newtype Options = Options Command
  deriving stock (Show)

instance Parseable Options where
  parser = Options <$> parser

data Command
  = List
  | Start {verbosity :: Verbosity, options :: StartOptions}
  | Ssh {vmName :: VmName, sshCommand :: [Text]}
  | Status {vmNames :: [VmName]}
  | Stop {vmName :: VmName}
  | Ip {vmName :: VmName}
  deriving stock (Show, Generic)

instance Parseable Command where
  parser =
    hsubparser
      ( command
          "list"
          ( info
              (pure List)
              (fullDesc <> progDesc "List all configured vms")
          )
          <> command
            "start"
            ( info
                (Start <$> parser <*> parser)
                (fullDesc <> progDesc "Start a development vm")
            )
          <> command
            "ssh"
            ( info
                (Ssh <$> parser <*> many (argument str (metavar "SSH_COMMAND")))
                (fullDesc <> progDesc "`ssh` into a running vm")
            )
          <> command
            "status"
            ( info
                (Status <$> many parser)
                (fullDesc <> progDesc "Show the status of running vms")
            )
          <> command
            "stop"
            ( info
                (Stop <$> parser)
                (progDesc "Stop a running vm")
            )
          <> command
            "ip"
            ( info
                (Ip <$> parser)
                (progDesc "Print the ip address of a vm (in the virtual network)")
            )
      )

data Verbosity
  = DefaultVerbosity
  | Verbose
  deriving stock (Show)

instance Parseable Verbosity where
  parser =
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

instance Parseable StartOptions where
  parser =
    flag' StartAll (long "all")
      <|> (StartSome . NonEmpty.fromList <$> some parser)

newtype VmName = VmName {vmNameToText :: Text}
  deriving stock (Eq, Show, Ord)
  deriving newtype (ToJSONKey, FromJSONKey)

instance Parseable VmName where
  parser = VmName <$> argument str (metavar "VM_NAME")
