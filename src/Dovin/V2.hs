-- V2 makes the following changes:
--
--   * withLocation now only takes a Location, using the current actor for
--   player.
--   * TODO: Flips argument order for `validate` functions to be consistent
--   with rest of API.
--   * TODO: `activate` and `trigger` use the stack.
module Dovin.V2
  ( Dovin.V2.withLocation
  , module Dovin
  )
  where

import Dovin hiding (withLocation)
import Dovin.Prelude
import Control.Monad.Reader (local)

withLocation :: Location -> GameMonad () -> GameMonad ()
withLocation loc m = do
  p <- view envActor

  local (set (envTemplate . cardLocation) (p, loc)) m
