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
  cs <- allCards

  let matchingCs = filter (applyMatcher matcher) cs

  case matchingCs of
    [] -> throwError "No matches"
    [x] -> return x
    xs -> throwError $ "Ambigious match: " <> (intercalate ", " . map (view cardName) $ xs)

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

      withLocation Play $ do
        withAttributes [token, "rhino"] $ addCreature (2, 2) "Rhino"
        addAura "Illusory Gains"
        withEffect
          matchInPlay
          (pure $ matchAttribute creature <> matchAttribute assemblyWorker)
          (pure . setAttribute shroud)
          $ addEnchantment "Steely Resolve"

    as bob $ do
      withLocation Play $ do
        addEnchantment "Wild Evocation"
        addCreature (20, 20) "Rotlung Reanimator 17" -- TODO P/T

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

  deadToken <- step "Read cell IV.A: Infest" $ do
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
            $ addCreature (2, 2) "Token 1"

      step "Action cell, IV.B: Illusory Gains" $ do
        as alice $ do
          trigger "Steal" "Illusory Gains" >> resolveTop
          forCards (matchController alice <> matchAttribute creature <> matchInPlay) $
            move (alice, Play) (bob, Play)
          move (bob, Play) (alice, Play) "Token 1"

      step "Main phase and combat: no available actions" $ return ()

      step "Draw step, then end of turn" $ do
        move (alice, Deck) (alice, Hand) "Cleansing Beam"
      step "Bob's turn" $ return ()

      step "Changing State, IV.D" $ do
        transitionToForced Untap

      step "Move tape, IV.C" $ do
        transitionTo Upkeep

        as bob $ do
          trigger "Force cast" "Wild Evocation" >> resolveTop

        as alice $ do
          _ <- lookupSingleCard (matchLocation (alice, Hand) <> matchName "Cleansing Beam")

          castWithWildEvocation "Cleansing Beam" >> resolveTop
          wheelOfSunAndMoon "Cleansing Beam"

          card <- lookupSingleCard
                    (  matchInPlay
                    <> matchAttribute creature
                    <> missingAttribute shroud
                    <> missingAttribute hexproof
                    )
          target (view cardName card)

          forCards (matchAttributes (creature : extractColors card)) $ \name -> do
            -- TODO: Check for vigor
            modifyCard cardPlusOneCounters (+ 1) name

formatter _ = boardFormatter
