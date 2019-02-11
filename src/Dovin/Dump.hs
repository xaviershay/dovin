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

whenMatch :: CardName -> CardMatcher -> GameMonad () -> GameMonad ()
whenMatch name f action = do
  match <- requireCard name f >> pure True `catchError` const (pure False)

  when match action

-- ACTIONS
--
-- These correspond to things you can do in Magic. They progress the state
-- machine while verifying applicable properties. They all run inside the
-- library monad.

sacrifice cn = do
  actor <- view envActor

  validate (matchController actor) cn

  removeFromPlay cn

removeFromPlay cardName = do
  card <- requireCard cardName matchInPlay

  moveTo Graveyard cardName

exile cardName = do
  card <- requireCard cardName mempty

  if hasAttribute "token" card then
    remove cardName
  else
    let loc = view location card in
      move loc (second (const Exile) loc) cardName

copySpell targetName newName = do
  card <- requireCard targetName mempty

  let newCard = setAttribute copy . set cardName newName $ card

  modifying
    cards
    (M.insert newName $ BaseCard newCard)

  modifying
    stack
    ((:) newName)

triggerStorm :: (Int -> GameMonad ()) -> GameMonad ()
triggerStorm action = do
  maybeStorm <- use $ counters . at "storm"

  case maybeStorm of
    Nothing -> throwError "No counter in state: storm"
    Just c -> forM [1..c-1] $ \n -> action n

  return ()

resetStrength :: CardName -> (Int, Int) -> GameMonad ()
resetStrength cn desired = do
  c <- requireCard cn (matchAttribute "creature")

  modifyCard cn cardStrength (const $ mkStrength desired)

modifyStrength :: (Int, Int) -> CardName -> GameMonad ()
modifyStrength (x, y) cn = do
  _ <- requireCard cn (matchInPlay <> matchAttribute "creature")

  modifyCard cn cardStrength (CardStrength x y <>)

  -- Fetch card again to get new strength
  c <- requireCard cn mempty

  when (view cardToughness c <= 0) $ removeFromPlay cn

-- TODO: Better name (resolveMentor?), check source has mentor attribute
triggerMentor sourceName targetName = do
  source <- requireCard sourceName $ matchAttribute attacking
  _      <- requireCard targetName $
                 matchAttribute attacking
              <> matchLesserPower (view cardPower source)

  modifyStrength (1, 1) targetName


fight :: CardName -> CardName -> GameMonad ()
fight x y = do
  cx <- requireCard x (matchInPlay <> matchAttribute creature)
  cy <- requireCard y (matchInPlay <> matchAttribute creature)

  target x
  target y

  fight' cx cy
  unless (cx == cy) $ fight' cy cx

  where
    fight' cx cy = do

      let xdmg = max 0 $ view cardPower cx
      modifyCard (view cardName cy) cardDamage (+ xdmg)
      cy' <- requireCard (view cardName cy) mempty

      when (hasAttribute "lifelink" cx) $
        do
          let owner = fst . view location $ cx
          modifying (life . at owner . non 0) (+ xdmg)

      when (view cardDamage cy' >= view cardToughness cy' || (xdmg > 0 && hasAttribute "deathtouch" cx )) $
        destroy (view cardName cy)

gainLife :: Player -> Int -> GameMonad ()
gainLife player amount =
  modifying
    (life . at player . non 0)
    (+ amount)

loseLife :: Player -> Int -> GameMonad ()
loseLife player amount = gainLife player (-amount)

setLife :: Player -> Int -> GameMonad ()
setLife p n = assign (life . at p) (Just n)

returnToHand cn = do
  actor <- view envActor
  move (actor, Graveyard) (actor, Hand) cn

returnToPlay cn = do
  actor <- view envActor
  move (actor, Graveyard) (actor, Play) cn

activatePlaneswalker :: Int -> CardName -> GameMonad ()
activatePlaneswalker loyalty cn = do
  c <- requireCard cn matchInPlay
  actor <- view envActor

  validate (matchController actor) cn

  if view cardLoyalty c - loyalty < 0 then
    throwError $ cn <> " does not have enough loyalty"
  else
    modifyCard cn cardLoyalty (+ loyalty)


-- HIGH LEVEL FUNCTIONS
--

fork :: [GameMonad ()] -> GameMonad ()
fork options = do
  b <- get
  let cs = view currentStep b

  forM_ options $ \m -> do
    m
    put $ set currentStep cs b

with x f = f x

run :: (Step -> Formatter) -> GameMonad () -> IO ()
run formatter solution = do
  let (e, _, log) = runMonad emptyBoard solution

  let groupedSteps =
        groupBy ((==) `on` view stepFork) . sortBy (comparing $ view stepId ) $ log

  forM_ groupedSteps $ \steps -> do
    case view stepFork $ head steps of
      Just l -> do
        putStrLn ""
        putStrLn $ "=== ALTERNATIVE: " <> l
        putStrLn ""
      Nothing -> return ()

    forM_ steps $ \step -> do
      putStr $ show (view stepNumber step) <> ". "
      putStr $ view stepLabel step
      putStrLn . formatter step . view stepState $ step

  putStrLn ""
  case e of
    Left x -> do
      putStrLn "ERROR:"
      putStrLn x
      putStrLn ""
      System.Exit.exitFailure
    Right _ -> return ()

runVerbose :: GameMonad () -> IO ()
runVerbose = run (const boardFormatter)
