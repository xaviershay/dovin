-- Dumping ground for things that haven't been thought through or tested yet.
module Dovin.Dump where

import Control.Arrow (second)
import Control.Lens
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Writer
import qualified Data.HashMap.Strict as M
import qualified Data.Set as S
import System.Exit
import Data.List (groupBy, sort, sortBy)
import Data.Ord (comparing)
import Data.Function (on)
import Debug.Trace

import Dovin.Actions
import Dovin.Attributes
import Dovin.Builder
import Dovin.Formatting
import Dovin.Helpers
import Dovin.Monad
import Dovin.Types

-- ACTIONS
--
-- These correspond to things you can do in Magic. They progress the state
-- machine while verifying applicable properties. They all run inside the
-- library monad.



resetStrength :: CardName -> (Int, Int) -> GameMonad ()
resetStrength cn desired = do
  c <- requireCard cn (matchAttribute "creature")

  modifyCardDeprecated cn cardStrength (const $ mkStrength desired)

modifyStrength :: (Int, Int) -> CardName -> GameMonad ()
modifyStrength (x, y) cn = do
  _ <- requireCard cn (matchInPlay <> matchAttribute "creature")

  modifyCardDeprecated cn cardStrength (CardStrength x y <>)

-- TODO: Better name (resolveMentor?), check source has mentor attribute
triggerMentor sourceName targetName = do
  source <- requireCard sourceName $ matchAttribute attacking
  _      <- requireCard targetName $
                 matchAttribute attacking
              <> matchLesserPower (view cardPower source)

  modifyStrength (1, 1) targetName



gainLife :: Player -> Int -> GameMonad ()
gainLife player amount =
  modifying
    (life . at player . non 0)
    (+ amount)

loseLife :: Player -> Int -> GameMonad ()
loseLife player amount = gainLife player (-amount)

setLife :: Player -> Int -> GameMonad ()
setLife p n = assign (life . at p) (Just n)


-- HIGH LEVEL FUNCTIONS
--

