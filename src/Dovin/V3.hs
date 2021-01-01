{-|
V3 introduces a new effects system that is incompatible with V2 effects. V3
effects are more flexible and better align with the layer system. It also adds
some new card properties.

* `withEffect` has a new, more flexible API.
* `withEffectWhen` added as a version of `withEffect` that allows customization
   of then the effect should apply (by default they apply when card is in
   play).
* `withCMC` to set converted mana cost on cards.
* `addEffect` to apply an effect to a card. In combination with
  `effectPTAdjust` this replaces altering `cardStrengthModifier` directly,
  which is now removed.
* Added a suite of `effect*` builders for effects.
* Added `cardOwner` property to cards, and `withOwner` fluid builder to set it.
-}
module Dovin.V3
  ( module Dovin.V2
  , module Dovin.Builder
  , module Dovin.Effects
  )
  where

import Dovin.V2 hiding (withEffect, cardStrengthModifier)
import Dovin.Builder (withEffect)
import Dovin.Effects
  ( effectPTSet
  , effectPTSetF
  , effectPTAdjust
  , effectPTAdjustF
  , effectAddAbility
  , effectNoAbilities
  , effectAddType
  , enabledInPlay
  , viewSelf
  , askSelf
  )

