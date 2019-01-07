{-|
V2 makes the following changes from V1:

* withLocation now only takes a Location, using the current actor for
  player.
* Flips argument order for `validate` functions to be consistent with rest
  of API.
* `activate` and `trigger` use the stack.
-}
module Dovin.V2
  ( module Dovin.Dump
  , module Dovin.Actions
  , module Dovin.Attributes
  , module Dovin.Builder
  , module Dovin.Formatting
  , module Dovin.Helpers
  , module Dovin.Types
  )
  where

import Dovin.Actions
import qualified Dovin.V1
import Dovin.Prelude
import Dovin.Dump hiding (activate, trigger)
import Dovin.Attributes
import Dovin.Builder
import Dovin.Formatting
import Dovin.Helpers
import Dovin.Monad
import Dovin.Types
import Control.Monad.Reader (local)
