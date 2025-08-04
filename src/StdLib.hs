module StdLib
  ( (%~),
    (&),
    (.~),
    (<&>),
    (</>),
    (?~),
    (^.),
    (^?),
    cs,
    ExitCode (..),
    forM,
    forM_,
    fromMaybe,
    Generic,
    Int64,
    mapMaybe,
    sort,
    Text,
    throwIO,
    to,
    unless,
    when,
  )
where

import Control.Exception.Safe (throwIO)
import Control.Lens (to, (%~), (&), (.~), (<&>), (?~), (^.), (^?))
import Control.Monad (forM, forM_, unless, when)
import Data.Generics.Labels ()
import Data.Int (Int64)
import Data.List (sort)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.String.Conversions (cs)
import Data.Text (Text)
import GHC.Generics (Generic)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
