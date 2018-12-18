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
  ) where

import Control.Lens
import Control.Monad (forM_)
import Control.Monad.Reader (ask, local)
import qualified Data.HashMap.Strict as M
import qualified Data.Set as S

import Dovin.Attributes
import Dovin.Helpers
import Dovin.Types

addCard :: CardName -> GameMonad ()
addCard name = do
  template <- ask
  -- TODO: Add this back in
  validateRemoved name
  modifying cards (M.insert name (set cardName name template))

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
addLands n name = withAttribute land $ do
  forM_ [1..n] $ \n -> addCard (numbered n name)

addSorcery :: CardName -> GameMonad ()
addSorcery name = withAttribute sorcery $ addCard name

-- | Add an attribute to the created card, as identified by a string.
-- Attributes with that special meaning to Dovin built-ins (such as flying) are
-- defined in "Dovin.Attributes".
withAttribute :: String -> GameMonad () -> GameMonad ()
withAttribute attr m = do
  local (over cardAttributes (S.insert attr)) m

-- | Helper version of 'withAttribute' for adding multiple attributes at a
-- time.
withAttributes :: [String] -> GameMonad () -> GameMonad ()
withAttributes attrs m = do
  local (over cardAttributes (S.union . S.fromList $ attrs)) m

-- | Set the location of the created card.
withLocation :: CardLocation -> GameMonad () -> GameMonad ()
withLocation loc m = do
  local (set location loc) m

withEffect :: CardMatcher -> (Card -> CardMatcher) -> (Card -> GameMonad Card) -> GameMonad () -> GameMonad ()
withEffect x y z =
  local (over cardEffects ((x, y, z):))
