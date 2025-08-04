module Options
  ( parserInfo,
    Options (..),
    Verbosity (..),
    Command (..),
    UpOptions (..),
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
  | Up {verbosity :: Verbosity, options :: UpOptions}
  | Ssh {vmName :: VmName, sshCommand :: [Text]}
  | Status {vmNames :: [VmName]}
  | Down {vmName :: VmName}
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
            "up"
            ( info
                (Up <$> parser <*> parser)
                (fullDesc <> progDesc "Start development vms")
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
            "down"
            ( info
                (Down <$> parser)
                (progDesc "Stop running vms")
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

data UpOptions
  = UpAll
  | UpSome (NonEmpty VmName)
  deriving stock (Show, Generic)

instance Parseable UpOptions where
  parser =
    flag' UpAll (long "all")
      <|> (UpSome . NonEmpty.fromList <$> some parser)

newtype VmName = VmName {vmNameToText :: Text}
  deriving stock (Eq, Show, Ord)
  deriving newtype (ToJSONKey, FromJSONKey)

instance Parseable VmName where
  parser = VmName <$> argument str (metavar "VM_NAME")
