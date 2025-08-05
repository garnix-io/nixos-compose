{-# LANGUAGE CPP #-}

module Version (version) where

import Data.String.Interpolate (i)
import StdLib

{-# NOINLINE version #-}
version :: Text
version = case cs [i|__NIXOS_COMPOSE_VERSION__|] of
  "__NIXOS_COMPOSE_VERSION__" -> "unknown"
  version -> version
