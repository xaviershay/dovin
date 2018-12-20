{-# LANGAUGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}

module Dovin
  ( module Dovin
  , module Dovin.Actions
  , module Dovin.Attributes
  , module Dovin.Builder
  , module Dovin.Formatting
  , module Dovin.Helpers
  , module Dovin.Types
  ) where

import Data.Char (isDigit)
import Control.Lens
import qualified Data.HashMap.Strict as M
import qualified Data.Set as S
import Data.Hashable
import GHC.Generics hiding (to)
import           Control.Monad.Except
import           Control.Monad.Writer
import           Control.Monad.State hiding (state)
import Control.Arrow ((&&&), second)
import Data.List
import Data.Function
import System.Exit

import Dovin.Actions
import Dovin.Attributes
import Dovin.Builder
import Dovin.Formatting
import Dovin.Helpers
import Dovin.Monad
import Dovin.Types

-- CORE TYPES
--
emptyBoard = Board
               { _cards = mempty
               , _counters = mempty
               , _stack = mempty
               , _life = mempty
               , _effects = mempty
               , _manaPool = mempty
               , _phase = FirstMain
               }


setAttribute :: CardAttribute -> Card -> Card
setAttribute attr = over cardAttributes (S.insert attr)

removeAttribute :: CardAttribute -> Card -> Card
removeAttribute attr = over cardAttributes (S.delete attr)

whenMatch :: CardName -> CardMatcher -> GameMonad () -> GameMonad ()
whenMatch name f action = do
  match <- requireCard name f >> pure True `catchError` (const $ pure False)

  when match action

-- EFFECTS
--
-- An effect is a reversible function that can be applied to a card. They can
-- be used to simplify keeping track of effects as cards enter and leave play.

instance Show Effect where
  show _ = "<effect>"

instance Semigroup Effect where
  (Effect f1 g1) <> (Effect f2 g2) = Effect (f1 . f2) (g1 . g2)

instance Monoid Effect where
  mempty = Effect id id

addEffect :: EffectName -> CardMatcher -> Effect -> GameMonad ()
addEffect cn f effect =
  modifying effects (M.insert cn (f, effect))

requireEffect :: EffectName -> GameMonad (CardMatcher, Effect)
requireEffect effectName = do
  board <- get
  case view (effects . at effectName) board of
    Nothing -> throwError $ "No effect named: " <> effectName
    Just x -> return x

applyEffect effectName = do
  (matcher, Effect f _) <- requireEffect effectName

  forCards matcher $ \name -> do
    modifying
      (cards . at name . _Just)
      (\(BaseCard c) -> BaseCard $ f c)

removeEffect effectName = do
  (matcher, Effect _ f) <- requireEffect effectName

  forCards matcher $ \name -> do
    modifying
      (cards . at name . _Just)
      (\(BaseCard c) -> BaseCard $ f c)

attributeEffect attr = Effect (setAttribute attr) (removeAttribute attr)
strengthEffect (x, y) = Effect
  (over cardStrength (mkStrength (x, y) <>))
  (over cardStrength (mkStrength (-x, -y) <>))

-- ACTIONS
--
-- These correspond to things you can do in Magic. They progress the state
-- machine while verifying applicable properties. They all run inside the
-- library monad.

jumpstart mana discardName castName = do
  spendMana mana
  discard discardName
  castFromLocation (Active, Graveyard) "" castName
  gainAttribute "exile-when-leave-stack" castName

discard = move (Active, Hand) (Active, Graveyard)

resolve :: CardName -> GameMonad ()
resolve expectedName = do
  s <- use stack

  case s of
    [] -> throwError $ "No spell on stack to resolve for: " <> expectedName
    (name:ss) ->

      if name /= expectedName then
        throwError $ "Top spell on stack does not match: expected "
                       <> name
                       <> ", got "
                       <> expectedName
      else do
        c <- requireCard name mempty

        assign stack ss

        when (hasAttribute "creature" c) $
          gainAttribute "summoned" name

        if hasAttribute "sorcery" c || hasAttribute "instant" c then
          if hasAttribute "copy" c then
            remove name
          else if hasAttribute "exile-when-leave-stack" c then
            do
              loseAttribute "exile-when-leave-stack" name
              move (Active, Stack) (Active, Exile) name
          else
            move (Active, Stack) (Active, Graveyard) name
        else
          move (Active, Stack) (Active, Play) name

target targetName = do
  card <- requireCard targetName (matchInPlay <> missingAttribute "hexproof")

  return ()

targetInLocation zone targetName = do
  card <- requireCard targetName (matchLocation zone)

  return ()

trigger targetName = do
  -- TODO: Technically some cards can trigger from other zones, figure out best
  -- way to represent.
  card <- requireCard targetName matchInPlay

  return ()

activate mana targetName = do
  card <- requireCard targetName mempty

  spendMana mana

  return ()

validate :: CardName -> CardMatcher -> GameMonad ()
validate targetName reqs = do
  _ <- requireCard targetName reqs
  return ()

validateLife :: Player -> Int -> GameMonad ()
validateLife player n = do
  current <- use (life . at player . non 0)

  when (current /= n) $
    throwError $ show player
      <> " life was "
      <> show current
      <> ", expected "
      <> show n

validatePhase :: Phase -> GameMonad ()
validatePhase expected = do
  actual <- use phase

  when (actual /= expected) $
    throwError $ "phase was "
      <> show actual
      <> ", expected "
      <> show expected

destroy targetName = do
  _ <- requireCard targetName (matchInPlay <> missingAttribute indestructible)

  removeFromPlay targetName

sacrifice targetName = removeFromPlay targetName

remove :: CardName -> GameMonad ()
remove cn = do
  modifying cards (M.delete cn)
  modifying stack (filter (/= cn))

removeFromPlay cardName = do
  card <- requireCard cardName matchInPlay

  if hasAttribute "token" card then
    remove cardName
  else
    let loc = view location card in
      move loc (second (const Graveyard) loc) cardName

exile cardName = do
  card <- requireCard cardName mempty

  if hasAttribute "token" card then
    remove cardName
  else
    let loc = view location card in
      move loc (second (const Exile) loc) cardName

copySpell targetName newName = do
  card <- requireCard targetName mempty

  let newCard = setAttribute "copy" . set cardName newName $ card

  modifying
    cards
    (M.insert newName $ BaseCard newCard)

  modifying
    stack
    ((:) newName)

storm :: (Int -> GameMonad ()) -> GameMonad ()
storm action = do
  maybeStorm <- use $ counters . at "storm"

  case maybeStorm of
    Nothing -> throwError "No counter in state: storm"
    Just c -> forM [1..c-1] $ \n -> action n

  return ()

resetStrength :: CardName -> (Int, Int) -> GameMonad ()
resetStrength cn desired = do
  c <- requireCard cn (matchAttribute "creature")

  modifyCard cn cardStrength (const $ mkStrength desired)

-- TODO: handle tokens, use `move`
moveToGraveyard cn = modifyCard cn location (\(player, _) -> (player, Graveyard))

modifyStrength :: (Int, Int) -> CardName -> GameMonad ()
modifyStrength (x, y) cn = do
  _ <- requireCard cn (matchInPlay <> matchAttribute "creature")

  modifyCard cn cardStrength (CardStrength x y <>)

  -- Fetch card again to get new strength
  c <- requireCard cn mempty

  when (view cardToughness c <= 0) $ removeFromPlay cn

attackWith :: [CardName] -> GameMonad ()
attackWith cs = do
  validatePhase FirstMain
  assign phase Combat

  forM_ cs $ \cn -> do
    c <- requireCard cn
           (matchLocation (Active, Play)
             <> matchAttribute "creature"
             <> (
                  matchAttribute "haste"
                  `matchOr`
                  missingAttribute "summoned"
                ))
    tap cn
    gainAttribute "attacking" cn

damagePlayer cn = do
  c <- requireCard cn matchInPlay
  modifying
    (life . at Opponent . non 0)
    (\x -> x - view cardPower c)

-- TODO: Better name (resolveMentor?), check source has mentor attribute
triggerMentor sourceName targetName = do
  source <- requireCard sourceName $ matchAttribute "attacking"
  _      <- requireCard targetName $
                 matchAttribute "attacking"
              <> matchLesserPower (view cardPower source)

  modifyStrength (1, 1) targetName


fight :: CardName -> CardName -> GameMonad ()
fight x y = do
  _ <- requireCard x matchInPlay
  _ <- requireCard y matchInPlay

  target x
  target y

  fight' x y
  unless (x == y) $ fight' y x

  where
    fight' x y = do
      cx <- requireCard x (matchAttribute "creature")
      cy <- requireCard y (matchAttribute "creature")

      let xdmg = max 0 $ view cardPower cx
      modifyCard y cardDamage (+ xdmg)
      cy' <- requireCard y mempty

      when (hasAttribute "lifelink" cx) $
        do
          let owner = fst . view location $ cx
          modifying (life . at owner . non 0) (+ xdmg)

      when (view cardDamage cy' >= view cardToughness cy' || (xdmg > 0 && hasAttribute "deathtouch" cx )) $
        destroy y

damageCard :: CardName -> CardName -> GameMonad ()
damageCard sourceName destName = do
  source <- requireCard sourceName (matchAttribute creature)
  dest   <- requireCard destName   (matchAttribute creature)

  let dmg = max 0 $ view cardPower source

  modifyCard destName cardDamage (+ dmg)

  when (hasAttribute lifelink source) $
    do
      let owner = fst . view location $ source
      modifying (life . at owner . non 0) (+ dmg)

  dest <- requireCard destName (matchAttribute creature)
  return ()
  -- TODO: Why isn't indestructible check working?
  -- TODO: Move this in to a state-based check?
  --when (not (hasAttribute indestructible dest) && (view cardDamage dest >= view cardToughness dest || (dmg > 0 && hasAttribute deathtouch source ))) $
    --destroy sourceName

forCards :: CardMatcher -> (CardName -> GameMonad ()) -> GameMonad ()
forCards matcher f = do
  cs <- allCards

  let matchingCs = filter (applyMatcher matcher) cs

  forM_ (map (view cardName) matchingCs) f

gainLife :: Player -> Int -> GameMonad ()
gainLife player amount =
  modifying
    (life . at player . non 0)
    (+ amount)

loseLife :: Player -> Int -> GameMonad ()
loseLife player amount = gainLife player (-amount)

setLife :: Player -> Int -> GameMonad ()
setLife p n = assign (life . at p) (Just n)

returnToHand = move (Active, Graveyard) (Active, Hand)
returnToPlay = move (Active, Graveyard) (Active, Play)

activatePlaneswalker :: Int -> CardName -> GameMonad ()
activatePlaneswalker loyalty cn = do
  c <- requireCard cn matchInPlay

  if view cardLoyalty c - loyalty < 0 then
    throwError $ cn <> " does not have enough loyalty"
  else
    modifyCard cn cardLoyalty (+ loyalty)

gainAttribute attr cn = do
  c <- requireCard cn mempty

  modifyCard cn id (setAttribute attr)

loseAttribute attr cn = do
  c <- requireCard cn mempty

  modifyCard cn id (removeAttribute attr)

-- HIGH LEVEL FUNCTIONS
--
-- A proof consists on multiple steps. Each step is a human-readable
-- description, then a definition of that step using actions. If a step fails,
-- no subsequent steps will be run.
step :: String -> GameMonad () -> GameMonad ()
step desc m = do
  b <- get
  let (e, b', _) = runMonad b m

  tell [(desc, b')]
  put b'

  case e of
    Left x -> throwError x
    Right _ -> return ()

fork :: [GameMonad ()] -> GameMonad ()
fork options = do
  b <- get

  forM_ options $ \m -> do
    m
    put b


with x f = f x

run :: (Int -> Formatter) -> GameMonad () -> IO ()
run formatter solution = do
  let (e, _, log) = runMonad emptyBoard solution

  forM_ (zip log [1..]) $ \((step, board), n) -> do
    putStr $ show n <> ". "
    putStr step
    putStrLn (formatter n board)

  putStrLn ""
  case e of
    Left x -> do
      putStrLn "ERROR:"
      putStrLn x
      putStrLn ""
      System.Exit.exitFailure
    Right _ -> return ()

runVerbose :: GameMonad () -> IO ()
runVerbose = run (\_ -> boardFormatter)
