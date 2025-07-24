module StdLib
  ( (&),
    (<&>),
    (^.),
    ExitCode (..),
    Generic,
    sort,
    Text,
    throwIO,
    unless,
    when,
  )
where

import Control.Exception.Safe (throwIO)
import Control.Lens ((&), (<&>), (^.))
import Control.Monad (unless, when)
import Data.Generics.Labels ()
import Data.List (sort)
import Data.Text (Text)
import GHC.Generics (Generic)
import System.Exit (ExitCode (..))
