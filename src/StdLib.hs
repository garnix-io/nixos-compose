module StdLib
  ( Generic,
    (&),
    (<&>),
    (^.),
    Text,
  )
where

import Control.Lens ((&), (<&>), (^.))
import Data.Generics.Labels ()
import Data.Text (Text)
import GHC.Generics (Generic)
