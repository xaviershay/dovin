{-# LANGUAGE TemplateHaskell #-}

module Solutions.MTGTC where

import Dovin.V2
import Dovin.Prelude
import Data.List (intercalate)

import Control.Lens
import qualified Data.List
import qualified Data.Set

alice = Opponent
bob = Active

-- Indicates that a card's color text has been changed by Glamerdye
colorHacked = "color-hacked"

shroud = "shroud"

aetherborn = "aetherborn"
rhino = "rhino"
sliver = "sliver"

assassin = "assassin"
assemblyWorker = "assembly-worker"

white = "white"
blue = "blue"
black = "black"
red = "red"
green = "green"
allColors = [white, blue, black, red, green]

extractColors = Data.Set.toList . Data.Set.intersection (Data.Set.fromList [white, blue, black, red, green]) . view cardAttributes

castWithWildEvocation = castNoRestrictions ""

castNoRestrictions mana name = do
  card <- requireCard name mempty
  spendMana mana
  modifyCard (location . _2) (const Stack) name
  modifying
    stack
    ((:) name)
  when
    (hasAttribute sorcery card || hasAttribute instant card) $
    modifying
      (counters . at storm . non 0)
      (+ 1)

-- Technically the card should never hit the graveyard, but that's not relevant
-- in this proof. Instead, move from graveyard to bottom of deck.
wheelOfSunAndMoon name = do
  active <- view envActor
  validate (matchLocation (active, Graveyard)) name
  moveTo Deck name

lookupSingleCard :: CardMatcher -> GameMonad Card
lookupSingleCard matcher = do
  matchingCs <- lookupCards matcher

  case matchingCs of
    [] -> throwError "No matches"
    [x] -> return x
    xs -> throwError $ "Ambigious match: " <> (intercalate ", " . map (view cardName) $ xs)

lookupCards :: CardMatcher -> GameMonad [Card]
lookupCards matcher = do
  cs <- allCards

  return $ filter (applyMatcher matcher) cs

whenNotHalted m = do
  halted <- gameFinished

  unless halted m

data State = Q1 | Q2

data Rule = Rule
  { _ruleState :: State
  , _ruleNumber :: Int
  , _ruleTrigger :: CardAttribute
  , _ruleAttributes :: [CardAttribute]
  }
makeLenses ''Rule

mkRule s n trigger attributes = Rule
  { _ruleState = s
  , _ruleNumber = n
  , _ruleTrigger = trigger
  , _ruleAttributes = attributes
  }

instance Show Rule where
  show = triggeringCreature

triggeringCreature rule =
  let baseCreature = case view ruleState rule of
                       Q1 -> "Rotlung Reanimator"
                       Q2 -> "Xathrid Necromancer"
  in

  baseCreature <> " " <> show (view ruleNumber rule)

solution :: GameMonad ()
solution = do
  let hand = ["Infest"]

  step "Initial state" $ do
    transitionToForced Untap

    as alice $ do
      withLocation Hand $ do
        addSorcery "Infest"

      withLocation Deck $ do
        addInstant "Cleansing Beam"
        addSorcery "Coalition Victory"
        withAttribute black $ addCreature (3, 3) "Soul Snuffers"

      withLocation Play $ do
        withAttributes [token, aetherborn] $ addCreature (2, 2) "Rhino"
        addAura "Illusory Gains"
        withAttributes allColors $ addLand "Island"
        withAttribute green $ addEnchantment "Choke"
        withEffect
          matchInPlay
          (pure $ matchAttribute creature <> matchAttribute assemblyWorker)
          (pure . setAttribute shroud)
          $ addEnchantment "Steely Resolve"

    as bob $ do
      withLocation Play $ do
        addEnchantment "Wild Evocation"
        withEffect
          matchInPlay
          (const $ matchAttributes [creature, black])
          (pure . over cardStrengthModifier (mkStrength (-1, -1) <>))
          $ withAttribute colorHacked
          $ addEnchantment "Dread of Night 1"

        withEffect
          matchInPlay
          (const $ matchAttributes [creature, black])
          (pure . over cardStrengthModifier (mkStrength (-1, -1) <>))
          $ withAttribute colorHacked
          $ addEnchantment "Dread of Night 2"

        -- TODO: setup this color pallete from black
        -- TODO: Correct P/T
        withAttributes [red, green, black, white]
          $ withPlusOneCounters 5 -- TODO: Where does this come from?
          $ addCreature (2, 2) "Rotlung Reanimator 1"
        withAttributes [red, green, black, white]
          $ withPlusOneCounters 5 -- TODO: Where does this come from?
          $ addCreature (2, 2) "Rotlung Reanimator 17"

        withEffect
          matchInPlay
          (matchLocation . view cardLocation)
          (pure . setAttribute hexproof)
          $ addEnchantment "Privileged Position"

    -- Create rotlung reanimators
    -- Create Xathrid Necromancers
    -- Create some tokens for initial tape
    -- Create sides of tape
    -- Create Alice's dec
    return ()


  runLoop

runLoop = do
  turn1
  turn2
  -- TODO: Check if this turn is effectively skipped by Mesmeric Orb
  turn3

  whenNotHalted $ do
    turn4
    runLoop


turn1 = do
  deadToken <- step "Read cell IV.A: Infest" $ do
    transitionToForced Untap
    transitionTo Upkeep

    as bob $ do
      trigger "Force cast" "Wild Evocation" >> resolveTop

    as alice $ do
      _ <- lookupSingleCard (matchLocation (alice, Hand) <> matchName "Infest")

      castWithWildEvocation "Infest" >> resolveTop
      wheelOfSunAndMoon "Infest"

    forCards (matchInPlay <> matchAttribute creature) $
      modifyCard cardStrengthModifier (mkStrength (-2, -2) <>)

    lookupSingleCard (matchInPlay <> matchToughness 0)

  let rules =
        [ mkRule Q1 1 aetherborn [sliver, white]
        , mkRule Q1 17 rhino [assassin, blue]
        ]

  let maybeRule = Data.List.find (\rule -> hasAttribute (view ruleTrigger rule) deadToken) rules

  case maybeRule of
    Nothing -> throwError $ "Unknown card died: " <> view cardName deadToken
    Just rule -> do
      let c = triggeringCreature rule

      step ("Action cell IV.B: " <> show rule) $ do
        as bob $ do
          trigger (c <> " Trigger") c >> resolveTop

          withLocation Play
            $ withAttributes (token : view ruleAttributes rule)
            $ addCreature (2, 2) "Token 1" -- TODO: Dynamic name

  step "Action cell, IV.B: Illusory Gains" $ do
    as alice $ do
      trigger "Steal" "Illusory Gains" >> resolveTop
      forCards (matchController alice <> matchAttribute creature <> matchInPlay) $
        move (alice, Play) (bob, Play)
      move (bob, Play) (alice, Play) "Token 1"

  step "Draw step, then end of turn" $ do
    transitionTo DrawStep
    move (alice, Deck) (alice, Hand) "Cleansing Beam"

    -- TODO: Validate alice can't do anything

  step "Bob's turn" $ return ()

turn2 = do
  step "Changing State, IV.D" $ do
    transitionToForced Untap

  step "Move tape, IV.C: Cleansing Beam" $ do
    transitionTo Upkeep

    as bob $ do
      trigger "Force cast" "Wild Evocation" >> resolveTop

    as alice $ do
      _ <- lookupSingleCard
             (  matchLocation (alice, Hand)
             <> matchName "Cleansing Beam"
             )

      castWithWildEvocation "Cleansing Beam" >> resolveTop
      wheelOfSunAndMoon "Cleansing Beam"

      card <- lookupSingleCard
                (  matchInPlay
                <> matchAttribute creature
                <> missingAttribute shroud
                <> missingAttribute hexproof
                )
      target (view cardName card)

      forCards (
          matchAttribute creature
          <> (foldl (\b a -> matchAttribute a `matchOr` b) (invert mempty) (extractColors card))
        ) $ \cn -> do
          -- TODO: Check for vigor
          modifyCard cardPlusOneCounters (+ 1) cn

  step "Check halt, IV.F: Draw" $ do
    transitionTo DrawStep
    move (alice, Deck) (alice, Hand) "Coalition Victory"

  step "Bob's turn" $ return ()

turn3 = do
  step "Check halt, IV.F: Coalition Victory" $ do
    transitionToForced Untap
    transitionTo Upkeep

    as bob $ do
      trigger "Force cast" "Wild Evocation" >> resolveTop

    as alice $ do
      _ <- lookupSingleCard
             (  matchLocation (alice, Hand)
             <> matchName "Coalition Victory"
             )

      castWithWildEvocation "Coalition Victory" >> resolveTop

      matches <-
        sequence
          . map (\(c, t) -> (not . null) <$> lookupCards (matchAttributes [c, t]))
           $ [(x, y) | x <- allColors, y <- [creature, land]]

      if (Prelude.all id matches) then
        transitionTo (Won alice)
      else
        wheelOfSunAndMoon "Coalition Victory"

  whenNotHalted $ do
    step "Move tape, IV.C: Draw" $ do
      transitionTo DrawStep
      move (alice, Deck) (alice, Hand) "Soul Snuffers"

    step "Bob's turn" $ return ()

turn4 = do
  step "Move tape, IV.C: Soul Snuffers" $ do
    transitionToForced Untap
    transitionTo Upkeep

    as bob $ do
      trigger "Force cast" "Wild Evocation" >> resolveTop

    as alice $ do
      _ <- lookupSingleCard
             (  matchLocation (alice, Hand)
             <> matchName "Soul Snuffers"
             )

      castWithWildEvocation "Soul Snuffers" >> resolveTop

      forCards (matchInPlay <> matchAttribute creature) $
        modifyCard cardMinusOneCounters (+ 1)

formatter _ = boardFormatter
