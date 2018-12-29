-- V2 makes the following changes:
--
--   * withLocation now only takes a Location, using the current actor for
--     player.
--   * Flips argument order for `validate` functions to be consistent with rest
--     of API.
--   * TODO: `activate` and `trigger` use the stack.
module Dovin.V2
  ( withLocation
  , validate
  , validateLife
  , module Dovin
  )
  where

import Dovin hiding (withLocation, validate, validateLife)
import qualified Dovin
import Dovin.Prelude
import Control.Monad.Reader (local)

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
validate = flip Dovin.validate

-- | Validates a player has a specific life total.
--
-- > validateLife 0 Opponent
--
-- [Validates]
--
--     * Player life equals amount.
validateLife :: Int -> Player -> GameMonad ()
validateLife = flip Dovin.validateLife
