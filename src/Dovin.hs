{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}

module Dovin where

import Data.Maybe (fromJust)
import Control.Lens
import Data.Monoid
import qualified Data.HashMap.Strict as M
import qualified Data.Set as S
import Data.Hashable
import GHC.Generics hiding (to)
import           Control.Monad.Except
import           Control.Monad.Writer
import           Control.Monad.State hiding (state)
import Control.Arrow ((&&&))
import Data.List
import Data.Function

-- CORE TYPES
--



type CardName = String
type CardAttribute = String
type CardLocation = (Player, Location)
data SpellType = Cast | Copy deriving (Show, Eq)
type Spell = (CardName, SpellType)

data Player = Active | Opponent deriving (Show, Eq, Generic, Ord)
data Location = Hand | Graveyard | Play | Playing | Exile
  deriving (Show, Eq, Ord)

data Card = Card
  { _cardName :: CardName
  , _location :: (Player, Location)
  , _cardAttributes :: S.Set CardAttribute
  , _cardStrength :: (Int, Int)
  , _cardDamage :: Int
  } deriving (Show)
instance Hashable Player

data Board = Board
  { _cards :: M.HashMap CardName Card
  , _stack :: [Spell]
  , _counters :: M.HashMap String Int
  , _life :: M.HashMap Player Int
  , _effects :: M.HashMap String (CardMatcher, Effect)
  }

type GameMonad a = (ExceptT String (StateT Board (WriterT [(String, Board)] Identity))) a

data CardMatcher = CardMatcher (Card -> Bool)
data Effect = Effect (Card -> Card) (Card -> Card)

makeLenses ''Board
makeLenses ''Card

emptyBoard = Board
               { _cards = mempty
               , _counters = mempty
               , _stack = mempty
               , _life = mempty
               , _effects = mempty
               }


setAttribute :: CardAttribute -> Card -> Card
setAttribute attr = over cardAttributes (S.insert attr)

removeAttribute :: CardAttribute -> Card -> Card
removeAttribute attr = over cardAttributes (S.delete attr)

hasAttribute attr = S.member attr . view cardAttributes

mkCard name location =
  Card
    { _cardName = name
    , _location = location
    , _cardAttributes = mempty
    , _cardStrength = (0, 0)
    , _cardDamage = 0
    }

requireCard :: CardName -> CardMatcher -> GameMonad Card
requireCard name f = do
  board <- get

  case view (cards . at name) board of
    Nothing -> throwError $ "Card does not exist: " <> name
    Just card -> if applyMatcher f card then
                   return card
                 else
                   throwError $ "Card does not match requirements: " <> name
-- CARD MATCHERS
--
-- Matchers are used for both filtering sets of cards, and also for verifying
-- attributes of cards.
--
-- TODO: Rename to match*?
-- A wrapping type is used since I intend to add labels/introspection
-- capabilities at some point.

instance Show CardMatcher where
  show _ = "<matcher>"

instance Semigroup CardMatcher where
  (CardMatcher f) <> (CardMatcher g) = CardMatcher $ \c -> f c && g c

instance Monoid CardMatcher where
  mempty = CardMatcher $ const True

requireLocation :: CardLocation -> CardMatcher
requireLocation loc = CardMatcher $ (==) loc . view location

-- TODO: Rename requireInPlay
inPlay = CardMatcher $ \c -> case view location c of
                               (_, Play) -> True
                               _         -> False

requireAttribute :: CardAttribute -> CardMatcher
requireAttribute attr = CardMatcher $ S.member attr . view cardAttributes

requireName :: CardName -> CardMatcher
requireName n = CardMatcher $ (==) n . view cardName

requireOther = invert . requireName

requireController player = CardMatcher $ (==) player . view (location . _1)

missingAttribute = invert . requireAttribute

invert :: CardMatcher -> CardMatcher
invert (CardMatcher f) = CardMatcher $ not . f

applyMatcher :: CardMatcher -> Card -> Bool
applyMatcher (CardMatcher f) c = f c

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

addEffect cn f effect = do
  modifying effects (M.insert cn (f, effect))

requireEffect effectName = do
  board <- get
  case view (effects . at effectName) board of
    Nothing -> throwError $ "No effect named: " <> effectName
    Just x -> return x

applyEffect effectName = do
  (matcher, Effect forward _) <- requireEffect effectName

  forCards matcher forward

removeEffect effectName = do
  (matcher, Effect _ undo) <- requireEffect effectName

  forCards matcher undo

attributeEffect attr = Effect (setAttribute attr) (removeAttribute attr)
strengthEffect (x, y) = Effect
  (over cardStrength (\(a, b) -> (a + x, b + y)))
  (over cardStrength (\(a, b) -> (a - x, b - y)))

-- ACTIONS
--
-- These correspond to things you can do in Magic. They progress the state
-- machine while verifying applicable properties. They all run inside the
-- library monad.

tap name = do
  let tapA = "tapped"

  card <- requireCard name
    (requireLocation (Active, Play) <> missingAttribute tapA)

  modifying
    (cards . at name . _Just . cardAttributes)
    (S.insert tapA)

cast name = do
  card <- requireCard name (requireLocation (Active, Hand))

  assign
    (cards . at name . _Just . location)
    (Active, Playing)

  when
    (hasAttribute "sorcery" card || hasAttribute "instant" card) $
    modifying
      (counters . at "storm" . non 0)
      (+ 1)

  modifying
    stack
    (\s -> (name, Cast) : s)

-- TODO: DRY up with cast
castFromLocation name loc = do
  card <- requireCard name (requireLocation loc)

  assign
    (cards . at name . _Just . location)
    (Active, Playing)

  when
    (hasAttribute "sorcery" card || hasAttribute "instant" card) $
    modifying
      (counters . at "storm" . non 0)
      (+ 1)

  modifying
    stack
    (\s -> (name, Cast) : s)

jumpstart discard name = do
  -- TODO: Discard
  card <- requireCard name (requireLocation (Active, Graveyard))

  assign
    (cards . at name . _Just . location)
    (Active, Playing)

  modifying
    (counters . at "storm" . non 0)
    (+ 1)

  modifying
    stack
    (\s -> (name, Cast) : s)

resolve :: CardName -> GameMonad ()
resolve expectedName = do
  s <- use stack

  case s of
    [] -> throwError $ "No spell on stack to resolve for: " <> expectedName
    ((name, spellType):ss) ->

      if name /= expectedName then
        throwError $ "Top spell on stack does not match: " <> name
      else do
        c <- requireCard name mempty

        assign stack ss

        when (hasAttribute "creature" c) $
          modifying
            (cards . at name . _Just)
            (setAttribute "summoned")

        if (hasAttribute "sorcery" c || hasAttribute "instant" c) then
          when (spellType == Cast) $
            assign
              (cards . at name . _Just . location)
              (Active, Graveyard)
        else
          modifying
            (cards . at name . _Just)
            (set location (Active, Play))

target targetName = do
  card <- requireCard targetName (inPlay <> missingAttribute "hexproof")

  return ()

targetInLocation targetName zone = do
  card <- requireCard targetName (requireLocation zone)

  return ()

trigger targetName = do
  -- TODO: Technically some cards can trigger from other zones, figure out best
  -- way to represent.
  card <- requireCard targetName inPlay

  return ()

validateRemoved targetName = do
  board <- get
  case view (cards . at targetName) board of
    Nothing -> return ()
    Just _ -> throwError $ "Card should be removed: " <> targetName

validate targetName reqs = do
  _ <- requireCard targetName reqs
  return ()

destroy targetName = do
  card <- requireCard targetName (inPlay <> missingAttribute "indestructible")

  case S.member "token" (view cardAttributes card) of
    True -> modifying cards (M.delete targetName)
    False ->
          modifying
            (cards . at targetName . _Just . location)
            (\(x, _) -> (x, Graveyard))

  return ()

sacrifice targetName = do
  card <- requireCard targetName inPlay

  case S.member "token" (view cardAttributes card) of
    True -> modifying cards (M.delete targetName)
    False ->
          modifying
            (cards . at targetName . _Just . location)
            (\(x, _) -> (x, Graveyard))

  return ()

copy targetName = do
  card <- requireCard targetName mempty

  modifying
    stack
    (\s -> (targetName, Copy) : s)

storm :: (Int -> GameMonad ()) -> GameMonad ()
storm action = do
  maybeStorm <- use $ counters . at "storm"

  case maybeStorm of
    Nothing -> throwError $ "No counter in state: storm"
    Just c -> forM [1..c-1] $ \n -> action n

  return ()

resetStrength :: CardName -> (Int, Int) -> GameMonad ()
resetStrength cn desired = do
  c <- requireCard cn (requireAttribute "creature")

  let c' = set cardStrength desired c

  assign
    (cards . at cn . _Just)
    c'

moveToGraveyard cn = do
  c <- requireCard cn mempty

  let c' = over location (\(player, _) -> (player, Graveyard)) c

  assign
    (cards . at cn . _Just)
    c'

modifyStrength :: CardName -> (Int, Int) -> GameMonad ()
modifyStrength cn (x, y) = do
  c <- requireCard cn (inPlay <> requireAttribute "creature")

  let c' = over cardStrength (\(a, b) -> (a + x, b + y)) c

  -- TODO: Handle tokens
  let c'' =
        if view (cardStrength . _2) c' <= 0 then
          over location (\(player, _) -> (player, Graveyard)) c'
        else
          c'

  assign
    (cards . at cn . _Just)
    c''

attackWith :: [CardName] -> GameMonad ()
attackWith cs = do
  forM_ cs $ \cn -> do
    c <- requireCard cn
           (requireLocation (Active, Play)
             <> requireAttribute "creature"
             <> missingAttribute "summoned")
    tap cn
    modifying
      (life . at Opponent . non 0)
      (\x -> x - view (cardStrength . _1) c)

numbered n name = name <> " " <> show n
fight :: CardName -> CardName -> GameMonad ()
fight x y = do
  _ <- requireCard x inPlay
  _ <- requireCard y inPlay

  target x
  target y

  fight' x y
  fight' y x

  where
    fight' x y = do
      cx <- requireCard x (requireAttribute "creature")
      cy <- requireCard y (requireAttribute "creature")

      let xdmg = max 0 $ view (cardStrength . _1) cx
      let cy' = over cardDamage (+ xdmg) cy

      assign
        (cards . at y . _Just)
        cy'

      if hasAttribute "lifelink" cx then
        do
          let owner = fst . view location $ cx
          modifying (life . at owner . non 0) (+ xdmg)
      else
        return ()

      if view cardDamage cy' >= view (cardStrength . _2) cy' || (xdmg > 0 && hasAttribute "deathtouch" cx ) then
        destroy y
      else
        return ()

forCards :: CardMatcher -> (Card -> Card) -> GameMonad ()
forCards matcher f = do
  cs <- use cards

  let matchingCs = filter (applyMatcher matcher) (M.elems cs)

  forM_ matchingCs $ \c ->
    assign
      (cards . at (view cardName c) . _Just)
      (f c)

-- TODO: Not needed anymore, replace usage with addCard
createToken name strength location = do
  board <- get

  let board' = execState (
                  addCreature name strength location ["creature", "token", "summoned"]
                ) board
  put board'

gainLife player amount =
  modifying
    (life . at player . non 0)
    (\x -> x + amount)

loseLife player amount =
  modifying
    (life . at player . non 0)
    (\x -> x - amount)

setLife p n =
  assign (life . at p) (Just n)

returnToHand cn =
  assign
    (cards . at cn . _Just . location)
    (Active, Hand)

returnToPlay cn =
  assign
    (cards . at cn . _Just . location)
    (Active, Play)

-- CARD HELPERS
--
-- These are a type of effect that create cards. They can be used for initial
-- board setup, but also to create cards as needed (such as tokens).

-- TODO: This naming scheme sucks
addCardFull name strength loc attrs = do
  let c = set cardStrength strength $ set cardAttributes (S.fromList attrs) $ mkCard name loc

  modifying cards (M.insert name c)

addCard name loc attrs = do
  addCardFull name (0, 0) loc attrs

addCreature name strength loc attrs =
  addCardFull name strength loc ("creature":attrs)

addCards 0 name loc attrs = return ()
addCards n name loc attrs = do
  addCard (name <> " " <> show n) loc attrs
  addCards (n - 1) name loc attrs

-- HIGH LEVEL FUNCTIONS
--
-- A proof consists on multiple steps. Each step is a human-readable
-- description, then a definition of that step using actions. If a step fails,
-- no subsequent steps will be run.
step desc m = do
  b <- get
  let (e, b', _) = runMonad b m

  tell [(desc, b')]
  put b'

  case e of
    Left x -> throwError x
    Right _ -> return ()

runMonad :: Board -> GameMonad () -> (Either String (), Board, [(String, Board)])
runMonad state m =
  let ((e, b), log) = runIdentity $
                        runWriterT (runStateT (runExceptT m) state) in

  (e, b, log)

printBoard board = do
  putStr "Opponent Life: "
  putStrLn . show $ view (life . at Opponent . non 0) board
  putStr "Storm: "
  putStrLn . show $ view (counters . at "storm" . non 0) board
  putStrLn ""
  let sections = groupByWithKey (view location) (M.elems $ view cards board)

  forM_ sections $ \(loc, cs) -> do
    putStrLn . show $ loc
    forM_ (sortBy (compare `on` view cardName) cs) $ \c -> do
      putStrLn $ "  " <> view cardName c <>
        " (" <> (intercalate "," . sort . S.toList $ view cardAttributes c) <> ")"
        <> if hasAttribute "creature" c then
             " (" <> (show $ view (cardStrength . _1) c) <> "/" <> (show $ view (cardStrength . _2) c) <> ", " <> (show $ view cardDamage c) <> ")"
           else
            ""

  when (not . null $ view stack board) $ do
    putStrLn "Stack"
    forM_ (view stack board) $ \(cn, spellType) -> do
      putStrLn $ "  " <> cn <> (if spellType == Cast then "" else " (copy)")

  where
    -- https://stackoverflow.com/questions/15412027/haskell-equivalent-to-scalas-groupby
    groupByWithKey :: (Ord b) => (a -> b) -> [a] -> [(b, [a])]
    groupByWithKey f = map (f . head &&& id)
                       . groupBy ((==) `on` f)
                       . sortBy (compare `on` f)

with x f = f x

runVerbose solution = do
  let (e, _, log) = runMonad emptyBoard solution

  forM_ (zip log [1..]) $ \((step, board), n) -> do
    putStr $ (show n) <> ". "
    putStrLn $ step
    putStrLn ""
    printBoard board
    putStrLn ""
    putStrLn ""

  case e of
    Left x -> fail x
    Right _ -> return ()
