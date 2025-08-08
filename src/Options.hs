module Options
  ( parserInfo,
    Options (..),
    Verbosity (..),
    Command (..),
    AllOrSomeVms (..),
    VmName (..),
  )
where

import Data.Aeson (FromJSONKey, ToJSONKey)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text
import Options.Applicative
import StdLib
import Version qualified

parserInfo :: ParserInfo Options
parserInfo =
  info parser mempty

class Parseable a where
  parser :: Parser a

newtype Options = Options Command
  deriving stock (Show)

instance Parseable Options where
  parser =
    version
      <*> (Options <$> parser)

version :: Parser (a -> a)
version =
  infoOption (cs Version.version) (long "version" <> help ("Show version (" <> cs Version.version <> ") and exit"))

data Command
  = List
  | Up {verbosity :: Verbosity, vms :: AllOrSomeVms}
  | Ssh {vmName :: VmName, sshCommand :: [Text]}
  | Status {vmNames :: [VmName]}
  | Down {vms :: AllOrSomeVms}
  | Ip {vmName :: VmName}
  deriving stock (Show, Generic)

instance Parseable Command where
  parser =
    hsubparser
      ( command
          "up"
          ( info
              (Up <$> parser <*> parser)
              (fullDesc <> progDesc "Start development vms")
          )
          <> command
            "down"
            ( info
                (Down <$> parser)
                (progDesc "Stop running vms")
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
            "list"
            ( info
                (pure List)
                (fullDesc <> progDesc "List all configured vms")
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

data AllOrSomeVms
  = All
  | Some (NonEmpty VmName)
  deriving stock (Show, Generic)

instance Parseable AllOrSomeVms where
  parser =
    many (parser :: Parser VmName)
      <&> ( \case
              [] -> All
              a : r -> Some (a :| r)
          )

newtype VmName = VmName {vmNameToText :: Text}
  deriving stock (Eq, Show, Ord)
  deriving newtype (ToJSONKey, FromJSONKey)

instance Parseable VmName where
  parser = VmName <$> argument str (metavar "VM_NAME")
