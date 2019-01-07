-- V2 makes the following changes from V1:
--
--   * withLocation now only takes a Location, using the current actor for
--     player.
--   * Flips argument order for `validate` functions to be consistent with rest
--     of API.
--   * `activate` and `trigger` use the stack.
module Dovin.V2
  ( activate
  , trigger
  , validate
  , module Dovin.Dump
  , module Dovin.Actions
  , module Dovin.Attributes
  , module Dovin.Builder
  , module Dovin.Formatting
  , module Dovin.Helpers
  , module Dovin.Types
  )
  where

import Dovin.Actions
import qualified Dovin.V1
import Dovin.Prelude
import Dovin.Dump hiding (activate, trigger)
import Dovin.Attributes
import Dovin.Builder
import Dovin.Formatting
import Dovin.Helpers
import Dovin.Monad
import Dovin.Types
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

