module Dovin.V1
  ( module Dovin.Runner
  , module Dovin.Actions
  , module Dovin.Attributes
  , module Dovin.Builder
  , module Dovin.Formatting
  , module Dovin.Helpers
  , module Dovin.Types
  , module Dovin.Matchers
  , validate
  , validateLife
  , withLocation
  , activate
  , trigger
  , fork
  , withEffect
  ) where

import Dovin.Runner
import Dovin.Actions hiding (validate, validateLife, activate, trigger, fork)
import qualified Dovin.Actions
import Dovin.Attributes
import Dovin.Builder hiding (withLocation, withEffect)
import Dovin.Formatting
import Dovin.Helpers
import Dovin.Monad
import Dovin.Types
import Dovin.Matchers

import Control.Monad (forM_)
import Control.Monad.State (put, get)
import Control.Monad.Reader (local)
import Control.Lens (set, view, over)
import Control.Monad.Identity (Identity)

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

-- | Branch off to an alternate line.
fork :: [GameMonad ()] -> GameMonad ()
fork options = do
  b <- get
  let cs = view currentStep b

  forM_ options $ \m -> do
    m
    put $ set currentStep cs b

-- | Add an effect to the created card.
withEffect ::
  CardMatcher -- ^ A matcher that must apply to this card for this affect to
              -- apply. 'matchInPlay' is a typical value.
 -> (Card -> CardMatcher) -- ^ Given the current card, return a matcher that
                          -- matches cards that this affect applies to.
 -> (Card -> Identity Card) -- ^ Apply an effect to the given card.
 -> GameMonad ()
 -> GameMonad ()
withEffect applyCondition filter action =
  local (over (envTemplate . cardEffects) (mkEffect applyCondition filter action:))

