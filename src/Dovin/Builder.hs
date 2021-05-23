{-|
Functions for adding new cards to the board.

> withLocation Hand $ do
>   withAttributes ["angel", token] $ addCreature (4, 4) "Angel"
-}
module Dovin.Builder (
  -- * Builders
  -- | Each of these terminates a build chain, and will add a card with the
  -- specified type to the board.
    addCard
  , addAura
  , addArtifact
  , addCreature
  , addEnchantment
  , addInstant
  , addLand
  , addLands
  , addPlaneswalker
  , addSorcery
  -- * Fluid interface
  -- | These methods can be chained together to specify different properties of
  -- the card to be created.
  , as
  , withAttribute
  , withAttributes
  , withCMC
  , withEffect
  , withEffectWhen
  , withLocation
  , withZone
  , withPlusOneCounters
  , withMinusOneCounters
  , withCardTarget
  , withColors
  ) where

import Dovin.Attributes
import Dovin.Prelude
import Dovin.Types
import Dovin.Helpers (getTimestamp)
import Dovin.Matchers (matchNone)
import Dovin.Effects (resolveEffects, enabledInPlay)

import Control.Monad.Reader (local)
import qualified Data.HashMap.Strict as M
import qualified Data.Set as S

addCard :: CardName -> GameMonad ()
addCard name = do
  card <- use $ cards . at name
  now <- getTimestamp

  case card of
    Just _ -> throwError $ "Card should be removed: " <> name
    Nothing -> do
      template <- view envTemplate
      let location = view cardLocation template
      modifying cards (M.insert name (BaseCard
        $ set cardName name
        . set cardTimestamp now
        $ template))

      case location of
        (player, Deck) ->
          modifying
            (deck . at (view cardController template) . non [])
            ((:) name)
        _ -> return ()

      resolveEffects

addAura :: CardName -> GameMonad ()
addAura name = withAttribute aura $ addEnchantment name

addArtifact :: CardName -> GameMonad ()
addArtifact name = withAttribute artifact $ addCard name

addCreature :: (Int, Int) -> CardName -> GameMonad ()
addCreature strength name = local (set (envTemplate . cardStrength) $ mkStrength strength)
  $ withAttribute creature
  $ addCard name

addPlaneswalker :: Int -> CardName -> GameMonad ()
addPlaneswalker loyalty name = local (set (envTemplate . cardLoyalty) loyalty)
  $ withAttribute planeswalker
  $ addCard name

addEnchantment :: CardName -> GameMonad ()
addEnchantment name = withAttribute enchantment $ addCard name

addInstant :: CardName -> GameMonad ()
addInstant name = withAttribute instant $ addCard name

addLand :: CardName -> GameMonad ()
addLand name = withAttribute land $ addCard name

addLands :: Int -> CardName -> GameMonad ()
addLands n name = withAttribute land $
  forM_ [1..n] $ \n -> addCard (numbered n name)

addSorcery :: CardName -> GameMonad ()
addSorcery name = withAttribute sorcery $ addCard name

-- | Perform action as the specified player.
as :: Player -> GameMonad () -> GameMonad ()
as p = local (
    set envActor p
    . set (envTemplate . cardController) p
  )

-- | Add an attribute to the created card, as identified by a string.
-- Attributes with that special meaning to Dovin built-ins (such as flying) are
-- defined in "Dovin.Attributes".
withAttribute :: String -> GameMonad () -> GameMonad ()
withAttribute attr = withAttributes [attr]

-- | Helper version of 'withAttribute' for adding multiple attributes at a
-- time.
withAttributes :: [String] -> GameMonad () -> GameMonad ()
withAttributes attrs =
  let f = S.union . S.fromList $ attrs in
  local (over (envTemplate . cardAttributes) f
       . over (envTemplate . cardDefaultAttributes) f)

-- | Add an effect to the created card. The effect will only apply when the
-- card is in play.
withEffect ::
    EffectMonad CardMatcher -- ^ The set of cards to apply the effect to
 -> [LayeredEffectPart]     -- ^ The effect to apply
 -> EffectName              -- ^ Human-readable description, cosmetic only.
 -> GameMonad ()
 -> GameMonad ()
withEffect = withEffectWhen enabledInPlay

-- | A more flexible version of 'withEffect' that allows customization of then
-- the effect should apply.
withEffectWhen ::
    EffectMonad Bool        -- ^ Effect only applies when this returns true
 -> EffectMonad CardMatcher -- ^ The set of cards to apply the effect to
 -> [LayeredEffectPart]     -- ^ The effect to apply
 -> EffectName              -- ^ Human-readable description, cosmetic only.
 -> GameMonad ()
 -> GameMonad ()
withEffectWhen enabled appliesTo effect name =
  local (over (envTemplate . cardPassiveEffects)
  (mkLayeredEffectPart combinedMatcher effect name:))

  where
    combinedMatcher :: EffectMonad CardMatcher
    combinedMatcher = do
      isEnabled <- enabled

      if isEnabled then
        appliesTo
      else
        return matchNone

-- | Set the converted mana cost of the created card.
withCMC :: Int -> GameMonad () -> GameMonad ()
withCMC n =
  local (set (envTemplate . cardCmc) n)

-- | Place the created card into a specific location.
withLocation :: Location -> GameMonad () -> GameMonad ()
withLocation loc m = do
  p <- view envActor

  local (
        set (envTemplate . cardController) p
      . set (envTemplate . cardZone) loc
    ) m

-- | Place the created card into a specific zone.
withZone :: Zone -> GameMonad () -> GameMonad ()
withZone n =
  local (set (envTemplate . cardZone) n)

-- | Add a target to the created card.
withCardTarget :: CardName -> GameMonad () -> GameMonad ()
withCardTarget target = local (over (envTemplate . cardTargets) ((:) (TargetCard target)))

withColors :: [Color] -> GameMonad () -> GameMonad ()
withColors cs =
  local (set (envTemplate . cardColors) (S.fromList cs))

-- | Set the number of +1/+1 counters of the created card.
withPlusOneCounters :: Int -> GameMonad () -> GameMonad ()
withPlusOneCounters = local . set (envTemplate . cardPlusOneCounters)

-- | Set the number of -1/-1 counters of the created card.
withMinusOneCounters :: Int -> GameMonad () -> GameMonad ()
withMinusOneCounters = local . set (envTemplate . cardMinusOneCounters)
