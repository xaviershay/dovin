{-# LANGUAGE TemplateHaskell #-}

-- https://arxiv.org/abs/1904.09828
module Solutions.MTGTC where

import Dovin.V2
import Dovin.Prelude
import Dovin.Monad
import Data.List (intercalate)

import Control.Lens
import qualified Data.List
import qualified Data.Set
import qualified Data.Ord
import qualified Data.Map as M

alice = Opponent
bob = Active

-- Indicates that a card's color text has been changed by Glamerdye
colorHacked = "color-hacked"
phasing = "phasing"
phasedOut = "phasedOut"

shroud = "shroud"

aetherborn = "aetherborn"
basilisk = "basilisk"
cephalid = "cephalid"
demon = "demon"
elf = "elf"
faerie = "fairie"
giant = "giant"
harpy = "harpy"
illusion = "illusion"
juggernaut = "juggernaut"
kavu = "kavu"
leviathan = "leviathan"
myr = "myr"
noggle = "noggle"
orc = "orc"
pegasus = "pegasus"
rhino = "rhino"
sliver = "sliver"

tapeTypes =
  [ assassin
  , aetherborn
  , basilisk
  , cephalid
  , demon
  , elf
  , faerie
  , giant
  , harpy
  , illusion
  , juggernaut
  , kavu
  , leviathan
  , myr
  , noggle
  , orc
  , pegasus
  , rhino
  , sliver
  ]

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

draw :: CardName -> GameMonad ()
draw expectedName = do
  active <- view envActor
  s <- use $ deck . at active . non mempty

  case s of
    [] -> throwError $ "deck is empty, expecting " <> expectedName
    (name:cs) -> do
      unless (name == expectedName) $
        throwError $ "unexpected top of deck: expected "
                       <> expectedName
                       <> ", got "
                       <> name
      moveTo Hand expectedName
      modifying
        (deck . at alice . non mempty)
        (drop 1)

phaseCards owner = do
  forCards (matchAttribute phasing <> matchController owner) $ \cn -> do
    c <- requireCard cn mempty

    if hasAttribute phasedOut c then
      loseAttribute phasedOut cn
    else
      gainAttribute phasedOut cn

findM :: (a -> GameMonad ()) -> [a] -> GameMonad a
findM f [] = throwError "No matching element"
findM f (x:xs) = (f x >> return x) `catchError` (\_ -> findM f xs)

-- Technically the card should never hit the graveyard, but that's not relevant
-- in this proof. Instead, move from graveyard to bottom of deck.
wheelOfSunAndMoon name = do
  active <- view envActor
  validate (matchLocation (active, Graveyard)) name
  moveTo Deck name
  modifying
    (deck . at active . non mempty)
    (\xs -> xs ++ [name])

lookupSingleCard :: CardMatcher -> GameMonad Card
lookupSingleCard matcher = do
  matchingCs <- lookupCards matcher

  case matchingCs of
    [] -> throwError $ "No matches: " <> show matcher
    [x] -> return x
    xs -> throwError $ "Ambigious match: " <> (intercalate ", " . map (view cardName) $ xs)

lookupCards :: CardMatcher -> GameMonad [Card]
lookupCards matcher = do
  cs <- allCards

  return $ filter (applyMatcher matcher) cs

whenNotHalted m = do
  halted <- gameFinished

  unless halted m

data State = Q1 | Q2 deriving (Show, Eq)

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
  let baseCreature = if tapped `elem` view ruleAttributes rule then
                       "Xathrid Necromancer"
                     else
                       "Rotlung Reanimator"
  in

  baseCreature <> " " <> show (view ruleState rule) <> " " <> show (view ruleNumber rule)


rules =
  [ mkRule Q1 1 aetherborn [sliver, white]
  , mkRule Q1 2 basilisk [green, elf]
  , mkRule Q1 3 cephalid [sliver, white]
  , mkRule Q1 4 demon [green, aetherborn]
  , mkRule Q1 5 elf [white, demon]
  , mkRule Q1 6 faerie [green, harpy]
  , mkRule Q1 7 giant [green, juggernaut]
  , mkRule Q1 8 harpy [white, faerie]
  , mkRule Q1 9 illusion [green, faerie]
  , mkRule Q1 10 juggernaut [white, illusion]
  , mkRule Q1 11 kavu [white, leviathan, tapped]
  , mkRule Q1 12 leviathan [white, illusion, tapped]
  , mkRule Q1 13 myr [white, basilisk, tapped]
  , mkRule Q1 14 noggle [green, orc]
  , mkRule Q1 15 orc [white, pegasus]
  , mkRule Q1 16 pegasus [green, rhino, tapped]
  , mkRule Q1 17 rhino [assassin, blue]
  , mkRule Q1 18 sliver [green, cephalid]
  , mkRule Q2 1 aetherborn [green, cephalid]
  , mkRule Q2 2 basilisk [green, cephalid]
  , mkRule Q2 3 cephalid [white, basilisk]
  , mkRule Q2 4 demon [green, elf]
  , mkRule Q2 5 elf [white, aetherborn]
  , mkRule Q2 6 faerie [green, kavu, tapped]
  , mkRule Q2 7 giant [green, harpy]
  , mkRule Q2 8 harpy [white, giant]
  , mkRule Q2 9 illusion [green, juggernaut]
  , mkRule Q2 10 juggernaut [white, giant]
  , mkRule Q2 11 kavu [green, faerie, tapped]
  , mkRule Q2 12 leviathan [green, juggernaut]
  , mkRule Q2 13 myr [green, orc]
  , mkRule Q2 14 noggle [green, orc]
  , mkRule Q2 15 orc [white, noggle]
  , mkRule Q2 16 pegasus [green, sliver]
  , mkRule Q2 17 rhino [white, sliver, tapped]
  , mkRule Q2 18 sliver [white, myr]
  ]

mappings = M.fromList
  . map (\x -> (head x, x))
  $ tapeTypes

charToType :: Char -> GameMonad CardAttribute
charToType c = do
  case M.lookup c mappings of
    Nothing -> throwError ("Unknown tape symbol: " <> [c])
    Just x -> return x

encodeTape :: String -> Char -> String -> GameMonad ()
encodeTape ls c rs = do
  forM_ (zip (reverse ls) [3..]) $ \(l, s) -> do
    t <- charToType l
    withAttributes [token, green, t] $ addCreature (s, s) ("Initial L" <> show s)

  t <- charToType c
  withAttributes [token, t] $ addCreature (2, 2) ("Initial C")

  forM_ (zip rs [3..]) $ \(l, s) -> do
    t <- charToType l
    withAttributes [token, white, t] $ addCreature (s, s) ("Initial R" <> show s)

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

        assign
          (deck . at alice)
          (Just ["Cleansing Beam", "Coalition Victory", "Soul Snuffers"])

      withLocation Play $ do

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
        encodeTape "" 's' "sr"
        addEnchantment "Wild Evocation"
        withEffect
          matchInPlay
          (const $ matchAttribute creature <> matchAttribute black)
          (pure . over cardStrengthModifier (mkStrength (-1, -1) <>))
          $ withAttribute colorHacked
          $ addEnchantment "Dread of Night 1"

        withEffect
          matchInPlay
          (const $ matchAttribute creature <> matchAttribute black)
          (pure . over cardStrengthModifier (mkStrength (-1, -1) <>))
          $ withAttribute colorHacked
          $ addEnchantment "Dread of Night 2"

        -- TODO: setup this color pallete from black
        forM_ rules $ \rule -> do
          let name = triggeringCreature rule

          let extraAttributes = if view ruleState rule == Q2 then
                                  [phasedOut]
                                else
                                  []
          withAttributes [red, green, black, white]
            $ withAttributes (phasing:extraAttributes)
            $ addCreature (2, 2) name

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


  runLoop 1

runLoop n
  | n > 5 = throwError "Looping!"
  | True = do
    turn1 n
    turn2 n
    -- TODO: Check if this turn is effectively skipped by Mesmeric Orb
    turn3 n

    whenNotHalted $ do
      turn4 n
      runLoop (n + 1)

turnStep c n l = step ("Cycle " <> show c <> ", Turn " <> show n <> ": " <> l)

turn1 n = do
  turnStep n 1 "Alice: Untap" $ do
    transitionToForced Untap
    phaseCards alice

  deadToken <- turnStep n 1 "Upkeep: Infest" $ do
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

  let matchingRules =
        filter
          (\rule -> hasAttribute (view ruleTrigger rule) deadToken)
          rules

  when (null matchingRules) $
    throwError ("Unknown card died: " <> formatCard deadToken)

  rule <- findM
            (validate (missingAttribute phasedOut) . triggeringCreature)
            matchingRules

  let c = triggeringCreature rule

  turnStep n 1 ("Action: " <> show rule) $ do
    as bob $ do
      trigger (c <> " Trigger") c >> resolveTop

      withLocation Play
        $ withAttributes (token : view ruleAttributes rule)
        $ addCreature (2, 2) ("Token " <> show n)

  turnStep n 1 "Illusory Gains" $ do
    as alice $ do
      trigger "Steal" "Illusory Gains" >> resolveTop
      forCards (matchController alice <> matchAttribute creature <> matchInPlay) $
        move (alice, Play) (bob, Play)
      move (bob, Play) (alice, Play) ("Token " <> show n)

  turnStep n 1 "Draw" $ do
    transitionTo DrawStep
    as alice $ draw "Cleansing Beam"

    -- TODO: Validate alice can't do anything

  turnStep n 1 "EoT: Undo infest -2/-2" $ do
    forCards (matchInPlay <> matchAttribute creature) $
      modifyCard cardStrengthModifier (const $ mkStrength (0, 0))

  turnStep n 1 "Bob: Untap and Phase" $ do
    phaseCards bob

turn2 n = do
  turnStep n 2 "Alice Untap" $ do
    transitionToForced Untap
    phaseCards alice

  turnStep n 2 "Upkeep: Cleansing Beam" $ do
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
          modifyCard cardPlusOneCounters (+ 2) cn

  turnStep n 2 "Alice Draw" $ do
    transitionTo DrawStep
    as alice $ draw "Coalition Victory"

  turnStep n 2 "Bob: Untap and Phase" $ do
    phaseCards bob

turn3 n = do
  turnStep n 3 "Upkeep: Coalition Victory" $ do
    transitionToForced Untap
    phaseCards alice

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
        throwError "won game"
      else
        wheelOfSunAndMoon "Coalition Victory"

  whenNotHalted $ do
    turnStep n 3 "Alice Draw" $ do
      transitionTo DrawStep
      as alice $ draw "Soul Snuffers"

  turnStep n 3 "Bob: Untap and Phase" $ do
    phaseCards bob

turn4 n = do
  turnStep n 4 "Upkeep: Soul Snuffers" $ do
    transitionToForced Untap
    phaseCards alice
    transitionTo Upkeep

    as bob $ do
      trigger "Force cast" "Wild Evocation" >> resolveTop

    as alice $ do
      _ <- lookupSingleCard
             (  matchLocation (alice, Hand)
             <> matchName "Soul Snuffers"
             )

      castWithWildEvocation "Soul Snuffers" >> resolveTop

      withStateBasedActions $ do
        forCards (matchInPlay <> matchAttribute creature) $
          modifyCard cardMinusOneCounters (+ 1)

      wheelOfSunAndMoon "Soul Snuffers"


  turnStep n 4 "Alice Draw" $ do
    transitionTo DrawStep
    as alice $ draw "Infest"

  turnStep n 4 "Bob: Untap and Phase" $ do
    phaseCards bob

matchAny = foldl (\b a -> a `matchOr` b) (invert mempty)

tapeFormatter :: Formatter
tapeFormatter board =
  let f matcher = Data.List.sortBy (Data.Ord.comparing tapePosition)
            . filter (applyMatcher $ matcher <> matchAny (map matchAttribute tapeTypes))
             $ cs in

  let leftCs = f (matchAttribute green <> invert (matchToughness 2)) in
  let rightCs = f (matchAttribute white <> invert (matchToughness 2)) in
  let centerCs = f (matchToughness 2) in

  let tapeValid = length centerCs == 1
                  && contiguous (map (view cardToughness) leftCs)
                  && contiguous (map (view cardToughness) rightCs)
                  && (null leftCs || minimum (map (view cardToughness) leftCs) == 3)
                  && (null rightCs || minimum (map (view cardToughness) rightCs) == 3)
                in

  let tapeWithHead = map extractSymbol (reverse leftCs) <> "[" <> map extractSymbol centerCs <> "]" <> map extractSymbol rightCs in

  if tapeValid then
    "\n      tape: " <> tapeWithHead
  else
    ""

  where
    cs = let Right value = execMonad board allCards in value
    tapePosition c = view cardPower c

extractSymbol c = if hasAttribute assassin c then 'H' else head . head . Data.Set.toList $ (Data.Set.fromList tapeTypes) `Data.Set.intersection` (view cardAttributes c)

matchOwner :: Player -> CardMatcher
matchOwner x = CardMatcher ("owner " <> show x) $
  (==) x . fst . view cardLocation

contiguous xs = Prelude.all (\(x, y) -> y - x == 1) $ zip xs (tail xs)

formatter _ =
     tapeFormatter
   -- <> cardFormatter "tape (bob)" (matchAny (map matchAttribute tapeTypes) <> matchOwner bob)
   -- <> cardFormatter "tape (alice)" (matchAny (map matchAttribute tapeTypes) <> matchOwner alice)
