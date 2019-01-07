module Dovin.V1
  ( module Dovin.Dump
  , module Dovin.Actions
  , module Dovin.Attributes
  , module Dovin.Builder
  , module Dovin.Formatting
  , module Dovin.Helpers
  , module Dovin.Types
  , validate
  , validateLife
  , withLocation
  ) where

import Dovin.Dump
import Dovin.Actions hiding (validate, validateLife)
import qualified Dovin.Actions
import Dovin.Attributes
import Dovin.Builder hiding (withLocation)
import Dovin.Formatting
import Dovin.Helpers
import Dovin.Monad
import Dovin.Types

import Control.Monad.Reader (local)
import Control.Lens (set)

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
