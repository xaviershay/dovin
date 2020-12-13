{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}


module Dovin.Types where

import Control.Lens (Lens', Prism', makeLenses, over, view, _1, _2, _Just, at, non)
import Control.Monad.Trans
import Control.Monad.Reader (ReaderT)
import Control.Monad.Except (ExceptT)
import Control.Monad.Identity (Identity)
import Control.Monad.State (StateT, MonadState, put, get)
import Control.Monad.Writer (WriterT, MonadWriter, tell, listen, pass)
import qualified Data.HashMap.Strict as M
import Data.Hashable (Hashable)
import qualified Data.Set as S
import GHC.Generics

newtype ComputedStateT s m a = ComputedStateT { runComputedStateT :: s -> m (a,s) }

instance Functor m => Functor (ComputedStateT s m) where
  fmap f m = ComputedStateT $ \s -> fmap (\(a, s') -> (f a, s')) $ runComputedStateT m s

instance (Functor m, Monad m) => Applicative (ComputedStateT s m) where
  pure x = ComputedStateT $ \s -> return (x, s)
  ComputedStateT mf <*> ComputedStateT mx = ComputedStateT $ \s -> do
    ~(f, s') <- mf s
    ~(x, s'') <- mx s'
    return (f x, s'')

instance (Monad m) => Monad (ComputedStateT s m) where
    return a = ComputedStateT $ \s -> return (a, s)
    m >>= k  = ComputedStateT $ \s0 -> do          -- in monad m
                 ~(a, s1) <- runComputedStateT m s0
                 runComputedStateT (k a) s1

instance MonadTrans (ComputedStateT s) where
    lift ma = ComputedStateT $ \s -> do            -- in monad m
                a <- ma
                return (a, s)

instance Monad m => MonadState Board (ComputedStateT Board m) where
  get = ComputedStateT $ \s -> return (s, s)
  put x = ComputedStateT $ \_ -> return ((), resolveEffects x)

instance MonadWriter w m => MonadWriter w (ComputedStateT s m) where
  tell = lift . Control.Monad.Writer.tell
  listen = Control.Monad.Writer.listen
  pass = Control.Monad.Writer.pass

evalComputedStateT :: (Monad m) => ComputedStateT s m a -> s -> m a
evalComputedStateT m s = do
    ~(a, _) <- runComputedStateT m s
    return a

resolveEffects :: Board -> Board
resolveEffects = id

-- Some good test cases https://blogs.magicjudges.org/ftw/l2-prep/rules-and-policy/continuous-effects/
-- For each card, map (Card, [Effect])
-- An effect:
--   Has a timestamp
--   Be conditional (on board state)
--   Affect a subset of cards ("attached card")
--   Specify changes that will belong in a layer, that are dependent on board
--   state ("pro from colors of commander of owning player")

-- The final state of the board can only change if the board is explicitly
-- modified. => the board should be re-calculated on each change, important to
-- ensure correct validations are run
-- 
-- We have a "board as printed" in which attributes of cards should never
-- change, can only be added/removed/moved
--
-- The result of "evaluating" a board "consumes" all effects and produces a
-- board where the cards have new attributes.
--
-- For each card, collect effects that apply to it _from all cards_, apply them
-- according to layers.
--
-- Collect all effects. For each card, apply the effect stack to them.

-- A card can have many effects applied to it, each with a layer + timestamp
-- .. or define the types of effects explicitly then have a function to layer them
-- CDA, can we type a CDA change? Power/Toughness/Subtype
--   CDA = intrinsic (part of object when created)
--         color, subtype, P/T
--         only affects same card
--
--         .. should be able to have on the card?
--         Doesn't depend on zone
-- How to model dependency?
--   Seems pretty hard, would need to spell out all the things that can be
--   dependend on?
--   .... try in different orders and see if different results!?
--   .... 613.8b loop = abort, use timestamp order
-- Instants creating effects? Seems better than the current modifying P/T thing
--   Add effects to cards? Then can be applied same as all other effects. Need
--   to be stored separately from intrinsic effects so can be removed when
--   moving zones (more generically, after a duration - "Leave Play", "End of turn", Forever, etc?
--
-- Maybe don't export board lens? Life & counters maybe used in solutions
--
--
-- Should we separate "Card Definition" from "Resolved Card"?
-- Definition: Color/Attributes/Power/Toughness/Effects
-- Resolved: Defintion+Damage+Attributes+Targets+Owner+Counters+Loyalty+Location
--
-- At rest: Color/Attributes/Power/Toughness/Effects/Counters/Location/Targets
-- What can change as a result of effects? Attributes/PT
--
-- Builders can only affect definition? Nah - need to be able to set up board
-- state as well as definition.

instance Show CardMatcher where
  show (CardMatcher l _) = l

instance Semigroup CardMatcher where
  (CardMatcher d1 f) <> (CardMatcher d2 g) =
    CardMatcher (d1 <> " and " <> d2) $ \c -> f c && g c

instance Monoid CardMatcher where
  mempty = CardMatcher "" $ const True

instance Semigroup CardStrength where
  CardStrength p1 t1 <> CardStrength p2 t2 =
   CardStrength (p1 + p2) (t1 + t2)

instance Monoid CardStrength where
  mempty = CardStrength 0 0

type CardName = String
type CardAttribute = String
data Player = Active | Opponent | OpponentN Integer deriving (Show, Eq, Generic, Ord)
-- This is pretty dodgy - one char per mana - but works for now.
type ManaPool = String
type ManaString = String
-- TODO: Stack shouldn't be in here because there is only one of them
data Location = Hand | Graveyard | Play | Stack | Exile | Deck | Command
  deriving (Show, Eq, Ord)

data CardEffect = CardEffect
  { _effectEnabled :: CardMatcher
  , _effectFilter :: Card -> CardMatcher
  , _effectAction :: Card -> GameMonad Card
  }

mkEffect enabled filter action = CardEffect
  -- For an effect to be enabled, it's host card must currently match this
  -- matcher.
  { _effectEnabled = enabled
  -- If the effect is enabled, this filter determines whether any particular
  -- card is affected by it.
  , _effectFilter = filter
  -- The action to apply to affected cards.
  , _effectAction = action
  }

-- A target for a spell or ability.
data Target =
    TargetPlayer Player -- ^ Target a player, use 'targetPlayer' to construct.
  | TargetCard CardName -- ^ Target a card, use 'targetCard' to construct.

targetPlayer = TargetPlayer
targetCard = TargetCard

type CardLocation = (Player, Location)
type CardAttributes = S.Set CardAttribute
data CardStrength = CardStrength Int Int deriving (Eq)
instance Show CardStrength where
  show (CardStrength p t) = show p <> "/" <> show t

-- | A phase or step in a turn. Phases and steps are not distinguished between
-- because haven't seen a need to.
data Phase
  = Untap
  | Upkeep
  | DrawStep
  | FirstMain
  | BeginCombat
  | DeclareAttackers
  | DeclareBlockers
  | FirstStrikeDamage
  | CombatDamage
  | EndCombat
  | SecondMain
  | EndStep
  | Won Player
  deriving (Show, Eq, Ord)

data Card = Card
  { _cardName :: CardName
  , _location :: (Player, Location)
  , _cardDefaultAttributes :: CardAttributes
  , _cardAttributes :: CardAttributes
  , _cardStrength :: CardStrength
  , _cardStrengthModifier :: CardStrength
  , _cardDamage :: Int
  , _cardLoyalty :: Int
  , _cardEffects :: [CardEffect]
  , _cardTargets :: [Target]
  , _cardOwner :: Player
  , _cardColors :: S.Set String -- TODO: Color type

  -- Can probably generalize this more at some point.
  , _cardPlusOneCounters :: Int
  , _cardMinusOneCounters :: Int
  }
instance Hashable Player
instance Show Card where
  show = _cardName
instance Eq Card where
  a == b = _cardName a == _cardName b

newtype BaseCard = BaseCard Card deriving (Show, Eq)

data CardMatcher = CardMatcher String (Card -> Bool)
type EffectName = String

data Board = Board
  { _cards :: M.HashMap CardName BaseCard
  -- The stack is currently the only location where we care about order, so
  -- store that information alongside the main _cards map. This won't scale -
  -- deck and graveyard need to be ordered also - but works for now. Need to
  -- think more about "hiding" this data structure.
  , _stack :: [CardName]
  -- ... well I guess the deck cares about order too now hmmm. Need to figure
  -- this out.
  , _deck :: M.HashMap Player [CardName]
  , _counters :: M.HashMap String Int
  -- In theory, life could be just another counter. Need to think more about
  -- making that happen.
  , _life :: M.HashMap Player Int
  , _manaPool :: M.HashMap Player ManaPool
  , _phase :: Phase
  , _currentStep :: StepIdentifier
  }

data Env = Env
  { _envTemplate :: Card
  , _envSBAEnabled :: Bool
  , _envActor :: Player
  , _envOwner :: Maybe Player
  }

type StepIdentifier = (Maybe String, Int)
data Step = Step
  { _stepId :: StepIdentifier
  , _stepLabel :: String
  , _stepState :: Board
  }

type GameMonad a
   = (ExceptT String (ReaderT Env (ComputedStateT Board (WriterT [Step] Identity)))) a
type Formatter = Board -> String

incrementStep :: StepIdentifier -> StepIdentifier
incrementStep (f, s) = (f, s + 1)

makeLenses ''Board
makeLenses ''Card
makeLenses ''CardEffect
makeLenses ''Env
makeLenses ''Step

stepFork :: Control.Lens.Lens' Step (Maybe String)
stepFork = stepId . _1

stepNumber :: Control.Lens.Lens' Step Int
stepNumber = stepId . _2

mkStep id label state = Step
  { _stepId = id
  , _stepLabel = label
  , _stepState = state
  }

cardLocation :: Control.Lens.Lens' Card (Player, Location)
cardLocation = location

-- TODO: How to define these lenses using built-in Lens primitives
-- (Control.Lens.Wrapped?)
cardPower :: Control.Lens.Lens' Card Int
cardPower f parent = fmap
  (\x -> over cardStrength (setPower x) parent)
  (f . power . view cardStrength $ parent)
  where
    setPower p (CardStrength _ t) = CardStrength p t
    power (CardStrength p _) = p

cardToughness :: Control.Lens.Lens' Card Int
cardToughness f parent = fmap
  (\x -> over cardStrength (setToughness x) parent)
  (f . toughness . view cardStrength $ parent)
  where
    setToughness t (CardStrength p _) = CardStrength p t
    toughness (CardStrength _ t) = t

manaPoolFor p = manaPool . at p . non mempty

-- I can't figure out the right type signature for manaPoolFor, so instead
-- providing this function to make it inferrable.
_manaPoolForTyping :: Board -> ManaPool
_manaPoolForTyping = view (manaPoolFor Active)

emptyEnv = Env
  { _envTemplate = emptyCard
  , _envSBAEnabled = True
  , _envActor = Active
  -- This is a bit of a hack for allowing us to default the owner to the
  -- location if none was specified. Ideally, the type of cardOwner in the
  -- template would be a Maybe, but that would require duplicating the Card
  -- type which doesn't seem worth it.
  , _envOwner = Nothing
  }

mkStrength (p, t) = CardStrength p t
emptyCard = mkCard "" (Active, Hand)
mkCard name location =
  Card
    { _cardName = name
    , _location = location
    , _cardDefaultAttributes = mempty
    , _cardAttributes = mempty
    , _cardStrength = mempty
    , _cardStrengthModifier = mempty
    , _cardDamage = 0
    , _cardLoyalty = 0
    , _cardEffects = mempty
    , _cardPlusOneCounters = 0
    , _cardMinusOneCounters = 0
    , _cardTargets = mempty
    , _cardOwner = fst location
    }

opposing :: Player -> Player
opposing Active = Opponent
opposing Opponent = Active

emptyBoard = Board
               { _cards = mempty
               , _counters = mempty
               , _stack = mempty
               , _deck = mempty
               , _life = mempty
               , _manaPool = mempty
               , _phase = FirstMain
               , _currentStep = (Nothing, 0)
               }

