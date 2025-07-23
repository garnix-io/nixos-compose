module StdLib
  ( Generic,
    (&),
    (<&>),
    (^.),
    Text,
    sort,
  )
where

import Control.Lens ((&), (<&>), (^.))
import Data.Generics.Labels ()
import Data.List (sort)
import Data.Text (Text)
import GHC.Generics (Generic)
