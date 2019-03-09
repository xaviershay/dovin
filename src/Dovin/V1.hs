module Dovin.V1
  ( module Dovin.Dump
  , module Dovin.Actions
  , module Dovin.Attributes
  , module Dovin.Builder
  , module Dovin.Formatting
  , module Dovin.Helpers
  , module Dovin.Types
  , fork
  , validate
  , validateLife
  , withLocation
  , activate
  , trigger
  ) where

import Dovin.Dump
import Dovin.Actions hiding (validate, validateLife, activate, trigger, fork)
import qualified Dovin.Actions
import Dovin.Attributes
import Dovin.Builder hiding (withLocation)
import Dovin.Formatting
import Dovin.Helpers
import Dovin.Monad
import Dovin.Types

import Control.Monad (forM_)
import Control.Monad.Reader (local)
import Control.Monad.State (get, put)
import Control.Lens (set, view)

-- | Validate that a card matches a matcher.
--
-- > validate "Angrath's Marauders" $ matchAttribute "pirate"
--
-- [Validates]
--
--   * Card matches matcher.
validate :: CardName -> CardMatcher -> GameMonad ()
validate = flip Dovin.Actions.validate

-- | Validates a player has a specific life total.
--
-- > validateLife Opponent 0
--
-- [Validates]
--
--     * Player life equals amount.
validateLife :: Player -> Int -> GameMonad ()
validateLife = flip Dovin.Actions.validateLife

-- | Set the location of the created card.
withLocation :: CardLocation -> GameMonad () -> GameMonad ()
withLocation loc = local (set (envTemplate . cardLocation) loc)

activate mana targetName = do
  card <- requireCard targetName mempty
  actor <- view envActor

  validate targetName $ matchController actor

  spendMana mana

  return ()

trigger targetName = do
  -- TODO: Technically some cards can trigger from other zones, figure out best
  -- way to represent.
  card <- requireCard targetName matchInPlay

  return ()

fork :: [GameMonad ()] -> GameMonad ()
fork options = do
  b <- get
  let cs = view currentStep b

  forM_ options $ \m -> do
    m
    put $ set currentStep cs b
