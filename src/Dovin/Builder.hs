{-|
Functions for adding new cards to the board.

> withLocation (Active, Hand) $ do
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
  , withAttribute
  , withAttributes
  , withEffect
  , withLocation
  , withPlusOneCounters
  ) where

import Control.Monad.Reader (ask, local)
import qualified Data.HashMap.Strict as M
import qualified Data.Set as S

import Dovin.Attributes
import Dovin.Actions
import Dovin.Helpers
import Dovin.Prelude
import Dovin.Types

addCard :: CardName -> GameMonad ()
addCard name = do
  template <- ask
  validateRemoved name
  modifying cards (M.insert name (BaseCard $ set cardName name template))

addAura :: CardName -> GameMonad ()
addAura name = withAttribute aura $ addEnchantment name

addArtifact :: CardName -> GameMonad ()
addArtifact name = withAttribute artifact $ addEnchantment name

addCreature :: (Int, Int) -> CardName -> GameMonad ()
addCreature strength name = local (set cardStrength $ mkStrength strength)
  $ withAttribute creature
  $ addCard name

addPlaneswalker :: Int -> CardName -> GameMonad ()
addPlaneswalker loyalty name = local (set cardLoyalty loyalty)
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

-- | Add an attribute to the created card, as identified by a string.
-- Attributes with that special meaning to Dovin built-ins (such as flying) are
-- defined in "Dovin.Attributes".
withAttribute :: String -> GameMonad () -> GameMonad ()
withAttribute attr = local (over cardAttributes (S.insert attr))

-- | Helper version of 'withAttribute' for adding multiple attributes at a
-- time.
withAttributes :: [String] -> GameMonad () -> GameMonad ()
withAttributes attrs =
  local (over cardAttributes (S.union . S.fromList $ attrs))

-- | Set the location of the created card.
withLocation :: CardLocation -> GameMonad () -> GameMonad ()
withLocation loc = local (set location loc)

-- | Add an effect to the created card.
withEffect ::
  CardMatcher -- ^ A matcher that must apply to this card for this affect to
              -- apply. 'matchInPlay' is a typical value.
 -> (Card -> CardMatcher) -- ^ Given the current card, return a matcher that
                          -- matches cards that this affect applies to.
 -> (Card -> GameMonad Card) -- ^ Apply an effect to the given card.
 -> GameMonad ()
 -> GameMonad ()
withEffect applyCondition filter action =
  local (over cardEffects (mkEffect applyCondition filter action:))

-- | Set the number of +1/+1 counters of the created card.
withPlusOneCounters :: Int -> GameMonad () -> GameMonad ()
withPlusOneCounters = local . set cardPlusOneCounters
