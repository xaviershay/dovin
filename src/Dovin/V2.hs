-- V2 makes the following changes:
--
--   * withLocation now only takes a Location, using the current actor for
--     player.
--   * Flips argument order for `validate` functions to be consistent with rest
--     of API.
--   * `activate` and `trigger` use the stack.
module Dovin.V2
  ( activate
  , trigger
  , withLocation
  , validate
  , validateLife
  , module Dovin.V1
  )
  where

import Dovin.V1 hiding (withLocation, validate, validateLife, activate, trigger)
import qualified Dovin.V1
import Dovin.Prelude
import Control.Monad.Reader (local)

-- | Activate an ability of a permanent. See 'spendMana' for additional mana
-- validations and effects. Typically you will want to `resolve` after
-- activating.
--
-- > activate "Create Soldier" "3W" "Dawn of Hope" >> resolveTop
--
-- [Validates]
--
--   * Card is in play or graveyard.
--   * Card is cotrolled by actor.
--
-- [Effects]
--
--   * A card with 'activated' is added to stack.
activate :: CardName -> ManaPool -> CardName -> GameMonad ()
activate stackName mana targetName = do
  actor <- view envActor
  card <-
    requireCard
      targetName
      (  matchController actor
      <> labelMatch "in play or graveyard" (
           matchLocation (actor, Play)
           `matchOr`
           matchLocation (actor, Graveyard)
         )
      )

  spendMana mana

  withLocation Stack $ withAttribute activated $ addCard stackName

  modifying
    stack
    ((:) stackName)

-- | Triggers an effect of a permanent.  Typically you will want to `resolve`
-- after triggering.
--
-- > activate "Draw Card" "Dawn of Hope" >> resolveTop
--
-- [Validates]
--
--   * Card is in play or graveyard.
--   * Card is cotrolled by actor.
--
-- [Effects]
--
--   * A card with 'triggered' is added to stack.
trigger :: CardName -> CardName -> GameMonad ()
trigger triggerName sourceName = do
  actor <- view envActor
  card <-
    requireCard
      sourceName
      (  matchController actor
      <> labelMatch "in play or graveyard" (
           matchLocation (actor, Play)
           `matchOr`
           matchLocation (actor, Graveyard)
         )
      )

  withLocation Stack $ withAttribute triggered $ addCard triggerName

  modifying
    stack
    ((:) triggerName)

withLocation :: Location -> GameMonad () -> GameMonad ()
withLocation loc m = do
  p <- view envActor

  local (set (envTemplate . cardLocation) (p, loc)) m

-- | Validate that a card matches a matcher.
--
-- > validate (matchAttribute "pirate") "Angrath's Marauders"
--
-- [Validates]
--
--     * Card matches matcher.
validate :: CardMatcher -> CardName -> GameMonad ()
validate = flip Dovin.V1.validate

-- | Validates a player has a specific life total.
--
-- > validateLife 0 Opponent
--
-- [Validates]
--
--     * Player life equals amount.
validateLife :: Int -> Player -> GameMonad ()
validateLife = flip Dovin.V1.validateLife
