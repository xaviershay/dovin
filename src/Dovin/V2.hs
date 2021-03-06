{-|
V2 makes the following changes from V1:

* withLocation now only takes a Location, using the current actor for
  player.
* Flips argument order for `validate` functions to be consistent with rest
  of API.
* `activate` and `trigger` use the stack.
* Fork has a saner API and reports properly.
* `formatter` now takes a 'Step' rather than an 'Int'. 'view' is re-exported
  since it will virtually always be required for normal formatters.
-}
module Dovin.V2
  ( module Dovin.Runner
  , module Dovin.Actions
  , module Dovin.Attributes
  , module Dovin.Builder
  , module Dovin.Formatting
  , module Dovin.Helpers
  , module Dovin.Types
  , module Dovin.Matchers
  , module Dovin.V1
  , module Control.Lens
  )
  where

import Dovin.Runner
import Dovin.Actions
import Dovin.V1 (withEffect, targetInLocation)
import Dovin.Attributes
import Dovin.Builder hiding (withEffect)
import Dovin.Formatting
import Dovin.Helpers
import Dovin.Matchers
import Dovin.Types hiding
  ( cards
  , stack
  , deck
  , life
  , manaPool
  , phase
  , currentStep
  , resolvedCards
  )
import Control.Lens (view)
