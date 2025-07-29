module StdLib
  ( (&),
    (<&>),
    (?~),
    (^.),
    ExitCode (..),
    forM,
    forM_,
    fromMaybe,
    Generic,
    Int64,
    sort,
    Text,
    throwIO,
    unless,
    when,
  )
where

import Control.Exception.Safe (throwIO)
import Control.Lens ((&), (<&>), (?~), (^.))
import Control.Monad (forM, forM_, unless, when)
import Data.Generics.Labels ()
import Data.Int (Int64)
import Data.List (sort)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import GHC.Generics (Generic)
import System.Exit (ExitCode (..))
