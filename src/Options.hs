module Options
  ( Options (..),
    Command (..),
    parser,
  )
where

import Context
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Text
import Options.Applicative
import StdLib

newtype Options = Options Command
  deriving stock (Show)

data Command
  = Start {vmNamesNonEmpty :: NonEmpty VmName}
  | Ssh {vmName :: VmName, sshCommand :: [Text]}
  | Status {vmNames :: [VmName]}
  | Stop {vmName :: VmName}
  deriving stock (Show, Generic)

parseVmName :: Parser VmName
parseVmName = VmName <$> argument str (metavar "VM_NAME")

parser :: ParserInfo Options
parser =
  info p mempty
  where
    p =
      Options
        <$> hsubparser
          ( command "start" (info (Start . NonEmpty.fromList <$> some parseVmName) (fullDesc <> progDesc "Start a development vm"))
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
