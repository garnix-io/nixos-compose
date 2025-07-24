module StdLib
  ( (&),
    (<&>),
    (^.),
    ExitCode (..),
    forM,
    forM_,
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
import Control.Monad (forM, forM_, unless, when)
import Data.Generics.Labels ()
import Data.List (sort)
import Data.Text (Text)
import GHC.Generics (Generic)
import System.Exit (ExitCode (..))
