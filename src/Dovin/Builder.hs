{-|
Functions for adding new cards to the board.

> withLocation (Active, Hand) $ do
>   withAttributes ["angel", token] $ addCreature (4, 4) "Angel"
-}
module Dovin.Builder (
  -- * Builders
  -- | Each of these terminates a build chain, and will add a card with the
  -- specified type to the board.
    addCard2
  , addAura
  , addArtifact
  , addCreature2
  , addEnchantment
  , addInstant
  , addLand
  , addPlaneswalker2
  , addSorcery
  -- * Fluid interface
  -- | These methods can be chained together to specify different properties of
  -- the card to be created.
  , withAttribute
  , withAttributes
  , withLocation
  ) where

import Control.Lens
import Control.Monad.Reader (ask, local)
import qualified Data.HashMap.Strict as M
import qualified Data.Set as S

import Dovin.Types
import Dovin.Attributes

addCard2 :: CardName -> GameMonad ()
addCard2 name = do
  template <- ask
  -- TODO: Add this back in
  --validateRemoved name
  modifying cards (M.insert name (set cardName name template))

addAura :: CardName -> GameMonad ()
addAura name = withAttribute aura $ addEnchantment name

addArtifact :: CardName -> GameMonad ()
addArtifact name = withAttribute artifact $ addEnchantment name

addCreature2 :: (Int, Int) -> CardName -> GameMonad ()
addCreature2 strength name = local (set cardStrength $ mkStrength strength)
  $ withAttribute creature
  $ addCard2 name

addPlaneswalker2 :: Int -> CardName -> GameMonad ()
addPlaneswalker2 loyalty name = local (set cardLoyalty loyalty)
  $ withAttribute planeswalker
  $ addCard2 name

addEnchantment :: CardName -> GameMonad ()
addEnchantment name = withAttribute enchantment $ addCard2 name

addInstant :: CardName -> GameMonad ()
addInstant name = withAttribute instant $ addCard2 name

addLand :: CardName -> GameMonad ()
addLand name = withAttribute land $ addCard2 name

addSorcery :: CardName -> GameMonad ()
addSorcery name = withAttribute sorcery $ addCard2 name

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

withLocation :: CardLocation -> GameMonad () -> GameMonad ()
withLocation loc m = do
  local (set location loc) m
