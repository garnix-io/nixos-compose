module StdLib
  ( Generic,
    (&),
    (<&>),
    (^.),
  )
where

import Control.Lens ((&), (<&>), (^.))
import Data.Generics.Labels ()
import GHC.Generics (Generic)
