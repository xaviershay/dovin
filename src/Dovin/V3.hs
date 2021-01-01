{-|
V3 introduces a new effects system that is incompatible with V2 effects. V3
effects are more flexible and better align with the layer system. It also adds
some new card properties.

* `withEffect` has a new, more flexible API.
* `withEffectWhen` added as a version of `withEffect` that allows customization
   of then the effect should apply (by default they apply when card is in
   play).
* added `withCMC` to set converted mana cost on cards.
-}
module Dovin.V3
  ( module Dovin.V2
  , module Dovin.Builder
  )
  where

import Dovin.V2 hiding (withEffect)
import Dovin.Builder (withEffect)
