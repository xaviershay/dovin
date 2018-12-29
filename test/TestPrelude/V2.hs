module TestPrelude.V2
  ( module TestPrelude
  , module Dovin.V2
  ) where

import Dovin.V2
import TestPrelude hiding
  ( activate
  , trigger
  , validate
  , validateLife
  , withLocation
  )
