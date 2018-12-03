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
import Control.Arrow ((&&&), (>>>), first, second)
import Data.List
import Data.Function
import System.Exit

-- CORE TYPES
--

type CardName = String
type CardAttribute = String
type CardLocation = (Player, Location)

data Player = Active | Opponent deriving (Show, Eq, Generic, Ord)
-- TODO: Stack shouldn't be in here because there is only one of them
data Location = Hand | Graveyard | Play | Stack | Exile
  deriving (Show, Eq, Ord)

data Card = Card
  { _cardName :: CardName
  , _location :: (Player, Location)
  , _cardAttributes :: S.Set CardAttribute
  , _cardStrength :: (Int, Int)
  , _cardDamage :: Int
  , _cardLoyalty :: Int
  } deriving (Show)
instance Hashable Player

data Board = Board
  { _cards :: M.HashMap CardName Card
  -- The stack is currently the only location where we care about order, so
  -- store that information alongside the main _cards map. This won't scale -
  -- deck and graveyard need to be ordered also - but works for now. Need to
  -- think more about "hiding" this data structure.
  , _stack :: [CardName]
  , _counters :: M.HashMap String Int
  -- In theory, life could be just another counter. Need to think more about
  -- making that happen.
  , _life :: M.HashMap Player Int
  , _effects :: M.HashMap String (CardMatcher, Effect)
  }

type GameMonad a = (ExceptT String (StateT Board (WriterT [(String, Board)] Identity))) a

data CardMatcher = CardMatcher String (Card -> Bool)
data Effect = Effect (Card -> Card) (Card -> Card)

makeLenses ''Board
makeLenses ''Card

cardPower = cardStrength . _1
cardToughness = cardStrength . _2

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
    , _cardLoyalty = 0
    }

requireCard :: CardName -> CardMatcher -> GameMonad Card
requireCard name f = do
  board <- get

  case view (cards . at name) board of
    Nothing -> throwError $ "Card does not exist: " <> name
    Just card -> case applyMatcherWithDesc f card of
                   Right () -> return card
                   Left msg ->
                     throwError $ name <> " does not match requirements: " <> msg

-- CARD MATCHERS
--
-- Matchers are used for both filtering sets of cards, and also for verifying
-- attributes of cards.
--
-- A wrapping type is used since I intend to add labels/introspection
-- capabilities at some point.

instance Show CardMatcher where
  show _ = "<matcher>"

instance Semigroup CardMatcher where
  (CardMatcher d1 f) <> (CardMatcher d2 g) =
    CardMatcher (d1 <> " and " <> d2) $ \c -> f c && g c

instance Monoid CardMatcher where
  mempty = CardMatcher "" $ const True

matchLocation :: CardLocation -> CardMatcher
matchLocation loc = CardMatcher ("in location " <> show loc) $
  (==) loc . view location

matchInPlay = CardMatcher "in play" $ \c -> snd (view location c) == Play

matchAttribute :: CardAttribute -> CardMatcher
matchAttribute attr = CardMatcher ("has attribute " <> attr) $
  S.member attr . view cardAttributes

matchName :: CardName -> CardMatcher
matchName n = CardMatcher ("has name " <> n) $ (==) n . view cardName

matchOther = invert . matchName

matchController player = CardMatcher ("has controller " <> show player) $
  (==) player . view (location . _1)

matchLesserPower n = CardMatcher ("power < " <> show n) $
  (< n) . view cardPower

missingAttribute = invert . matchAttribute

(CardMatcher d1 f) `matchOr` (CardMatcher d2 g) =
    CardMatcher (d1 <> " or " <> d2) $ \c -> f c || g c

invert :: CardMatcher -> CardMatcher
invert (CardMatcher d f) = CardMatcher ("not " <> d) $ not . f

applyMatcher :: CardMatcher -> Card -> Bool
applyMatcher matcher c =
  case applyMatcherWithDesc matcher c of
    Left _ -> False
    Right _ -> True

applyMatcherWithDesc :: CardMatcher -> Card -> Either String ()
applyMatcherWithDesc (CardMatcher d f) c =
  if f c then
    Right ()
  else
    Left d

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

addEffect cn f effect =
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
  (over cardStrength (first (+ x) >>> second (+ y)))
  (over cardStrength (first (subtract x) >>> second (subtract y)))

-- ACTIONS
--
-- These correspond to things you can do in Magic. They progress the state
-- machine while verifying applicable properties. They all run inside the
-- library monad.

tap name = do
  let tapA = "tapped"

  card <- requireCard name
    (matchLocation (Active, Play) <> missingAttribute tapA)

  modifying
    (cards . at name . _Just . cardAttributes)
    (S.insert tapA)

cast name = castFromLocation name (Active, Hand)

castFromLocation name loc = do
  move name loc (Active, Stack)

  card <- requireCard name mempty

  when
    (hasAttribute "sorcery" card || hasAttribute "instant" card) $
    modifying
      (counters . at "storm" . non 0)
      (+ 1)

  modifying
    stack
    ((:) name)

jumpstart discardName castName = do
  discard discardName
  castFromLocation castName (Active, Graveyard)
  modifying
    (cards . at castName . _Just)
    (setAttribute "exile-when-leave-stack")

discard name = move name (Active, Hand) (Active, Graveyard)

move name from to = do
  c <- requireCard name $ matchLocation from

  assign
    (cards . at name . _Just . location)
    to

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
          modifying
            (cards . at name . _Just)
            (setAttribute "summoned")

        if hasAttribute "sorcery" c || hasAttribute "instant" c then
          if hasAttribute "copy" c then
            remove name
          else if hasAttribute "exile-when-leave-stack" c then
            do
              modifying
                (cards . at name . _Just)
                (removeAttribute "exile-when-leave-stack")
              move name (Active, Stack) (Active, Exile)
          else
            move name (Active, Stack) (Active, Graveyard)
        else
          move name (Active, Stack) (Active, Play)

target targetName = do
  card <- requireCard targetName (matchInPlay <> missingAttribute "hexproof")

  return ()

targetInLocation targetName zone = do
  card <- requireCard targetName (matchLocation zone)

  return ()

trigger targetName = do
  -- TODO: Technically some cards can trigger from other zones, figure out best
  -- way to represent.
  card <- requireCard targetName matchInPlay

  return ()

activate targetName = do
  card <- requireCard targetName mempty

  return ()

validateRemoved targetName = do
  board <- get
  case view (cards . at targetName) board of
    Nothing -> return ()
    Just _ -> throwError $ "Card should be removed: " <> targetName

validate targetName reqs = do
  _ <- requireCard targetName reqs
  return ()

validateLife player n = do
  current <- use (life . at player . non 0)

  when (current /= n) $
    throwError $ show player
      <> " life was "
      <> show current
      <> ", expected "
      <> show n

destroy targetName = do
  _ <- requireCard targetName (matchInPlay <> missingAttribute "indestructible")

  removeFromPlay targetName

sacrifice targetName = removeFromPlay targetName

remove cn = do
  modifying cards (M.delete cn)
  modifying stack (filter (/= cn))

removeFromPlay cardName = do
  card <- requireCard cardName matchInPlay

  if hasAttribute "token" card then
    remove cardName
  else
    let loc = view location card in
      move cardName loc $ second (const Graveyard) loc

copySpell targetName newName = do
  card <- requireCard targetName mempty

  let newCard = setAttribute "copy" . set cardName newName $ card

  modifying
    cards
    (M.insert newName newCard)

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
  _ <- requireCard cn (matchInPlay <> matchAttribute "creature")

  modifying
    (cards . at cn . _Just . cardStrength)
    (first (+ x) >>> second (+ y))

  -- Fetch card again to get new strength
  c <- requireCard cn mempty

  when (view cardToughness c <= 0) $ removeFromPlay cn

attackWith :: [CardName] -> GameMonad ()
attackWith cs =
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
    gainAttribute cn "attacking"

damagePlayer cn = do
  c <- requireCard cn matchInPlay
  modifying
    (life . at Opponent . non 0)
    (\x -> x - view cardPower c)

mentor sourceName targetName = do
  source <- requireCard sourceName $ matchAttribute "attacking"
  _      <- requireCard targetName $
                 matchAttribute "attacking"
              <> matchLesserPower (view cardPower source)

  modifyStrength targetName (1, 1)

numbered n name = name <> " " <> show n
fight :: CardName -> CardName -> GameMonad ()
fight x y = do
  _ <- requireCard x matchInPlay
  _ <- requireCard y matchInPlay

  target x
  target y

  fight' x y
  fight' y x

  where
    fight' x y = do
      cx <- requireCard x (matchAttribute "creature")
      cy <- requireCard y (matchAttribute "creature")

      let xdmg = max 0 $ view cardPower cx
      let cy' = over cardDamage (+ xdmg) cy

      assign
        (cards . at y . _Just)
        cy'

      when (hasAttribute "lifelink" cx) $
        do
          let owner = fst . view location $ cx
          modifying (life . at owner . non 0) (+ xdmg)

      when (view cardDamage cy' >= view cardToughness cy' || (xdmg > 0 && hasAttribute "deathtouch" cx )) $
        destroy y

forCards :: CardMatcher -> (Card -> Card) -> GameMonad ()
forCards matcher f = do
  cs <- use cards

  let matchingCs = filter (applyMatcher matcher) (M.elems cs)

  forM_ matchingCs $ \c ->
    assign
      (cards . at (view cardName c) . _Just)
      (f c)

gainLife player amount =
  modifying
    (life . at player . non 0)
    (+ amount)

loseLife player amount = gainLife player (-amount)

setLife p n = assign (life . at p) (Just n)

returnToHand cn = move cn (Active, Graveyard) (Active, Hand)
returnToPlay cn = move cn (Active, Graveyard) (Active, Play)

activatePlaneswalker cn loyalty = do
  c <- requireCard cn matchInPlay

  if view cardLoyalty c - loyalty < 0 then
    throwError $ cn <> " does not have enough loyalty"
  else
    modifying
      (cards . at cn . _Just . cardLoyalty)
      (+ loyalty)

gainAttribute cn attr = do
  c <- requireCard cn mempty

  modifying
    (cards . at cn . _Just)
    (setAttribute attr)


-- CARD HELPERS
--
-- These are a type of effect that create cards. They can be used for initial
-- board setup, but also to create cards as needed (such as tokens).

addCardRaw name strength loc attrs = do
  let c = set cardStrength strength $ set cardAttributes (S.fromList attrs) $ mkCard name loc

  modifying cards (M.insert name c)

addCard name =
  addCardRaw name (0, 0)

addCreature name strength loc attrs =
  addCardRaw name strength loc ("creature":attrs)

addPlaneswalker name loyalty loc = do
  let c = set cardLoyalty loyalty $ set cardAttributes (S.fromList ["planeswalker"]) $ mkCard name loc

  modifying cards (M.insert name c)

addToken name strength loc attrs =
  addCreature name strength loc ("token":attrs)

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
  print $ view (life . at Opponent . non 0) board
  putStrLn ""
  let sections = groupByWithKey (view location) (M.elems $ view cards board)

  forM_ sections $ \(loc, cs) ->
    unless (snd loc == Stack) $ do
      print loc
      forM_ (sortBy (compare `on` view cardName) cs) $ \c ->
        putStrLn $ formatCard c

  unless (null $ view stack board) $ do
    putStrLn "Stack"
    forM_ (view stack board) $ \cn ->
      case view (cards . at cn) board of
        Just c -> putStrLn $ formatCard c
        Nothing -> fail $ cn <> " was on stack but doesn't exist"

  where
    formatCard c =
      "  " <> view cardName c <>
      " (" <> (intercalate "," . sort . S.toList $ view cardAttributes c) <> ")"
      <> if hasAttribute "creature" c then
           " ("
             <> show (view cardPower c)
             <> "/"
             <> show (view cardToughness c)
             <> ", "
             <> show (view cardDamage c)
             <> ")"
         else if hasAttribute "planeswalker" c then
           " ("
             <> show (view cardLoyalty c)
             <> ")"
         else
          ""

    -- https://stackoverflow.com/questions/15412027/haskell-equivalent-to-scalas-groupby
    groupByWithKey :: (Ord b) => (a -> b) -> [a] -> [(b, [a])]
    groupByWithKey f = map (f . head &&& id)
                       . groupBy ((==) `on` f)
                       . sortBy (compare `on` f)

with x f = f x

runVerbose :: GameMonad () -> IO ()
runVerbose solution = do
  let (e, _, log) = runMonad emptyBoard solution

  forM_ (zip log [1..]) $ \((step, board), n) -> do
    putStr $ show n <> ". "
    putStrLn step
    putStrLn ""
    printBoard board
    putStrLn ""
    putStrLn ""

  case e of
    Left x -> do
      putStrLn "ERROR:"
      putStrLn x
      putStrLn ""
      System.Exit.exitFailure
    Right _ -> return ()
