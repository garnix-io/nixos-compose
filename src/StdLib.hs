{-# OPTIONS_GHC -Wno-orphans #-}

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
    exitSuccess,
    exitWith,
    forM,
    forM_,
    fromMaybe,
    Generic,
    mapMaybe,
    ProcessID,
    sort,
    Text,
    to,
    unless,
    when,
  )
where

import Control.Lens (to, (%~), (&), (.~), (<&>), (?~), (^.), (^?))
import Control.Monad (forM, forM_, unless, when)
import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Generics.Labels ()
import Data.Int (Int64)
import Data.List (sort)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.String.Conversions (cs)
import Data.Text (Text)
import GHC.Generics (Generic)
import System.Exit (ExitCode (..), exitSuccess, exitWith)
import System.FilePath ((</>))
import System.Posix (ProcessID)

instance ToJSON ProcessID where
  toJSON pid = toJSON (fromIntegral pid :: Int64)

instance FromJSON ProcessID where
  parseJSON value = do
    pid :: Int64 <- parseJSON value
    pure $ fromIntegral pid
