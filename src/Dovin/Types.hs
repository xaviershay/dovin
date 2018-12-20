{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module Dovin.Types where

import Control.Lens (Lens', makeLenses, over, view)
import Control.Monad.Reader (ReaderT)
import Control.Monad.Except (ExceptT)
import Control.Monad.Identity (Identity)
import Control.Monad.State (StateT)
import Control.Monad.Writer (WriterT)
import qualified Data.HashMap.Strict as M
import Data.Hashable (Hashable)
import qualified Data.Set as S
import GHC.Generics

type CardName = String
type CardAttribute = String
data Player = Active | Opponent deriving (Show, Eq, Generic, Ord)
-- This is pretty dodgy - one char per mana - but works for now.
type ManaPool = String
type ManaString = String
-- TODO: Stack shouldn't be in here because there is only one of them
data Location = Hand | Graveyard | Play | Stack | Exile
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
  -- If the effect is enabled, this filter determines wheter any particular
  -- card is affected by it.
  , _effectFilter = filter
  -- The action to apply to affected cards.
  , _effectAction = action
  }

type CardLocation = (Player, Location)
type CardAttributes = S.Set CardAttribute
data CardStrength = CardStrength Int Int deriving (Eq)
instance Show CardStrength where
  show (CardStrength p t) = show p <> "/" <> show t

data Phase = FirstMain | Combat deriving (Show, Eq)

data Card = Card
  { _cardName :: CardName
  , _location :: (Player, Location)
  , _cardAttributes :: CardAttributes
  , _cardStrength :: CardStrength
  , _cardDamage :: Int
  , _cardLoyalty :: Int
  , _cardEffects :: [CardEffect]
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
  , _counters :: M.HashMap String Int
  -- In theory, life could be just another counter. Need to think more about
  -- making that happen.
  , _life :: M.HashMap Player Int
  , _manaPool :: ManaPool
  , _phase :: Phase
  }

type GameMonad a = (ExceptT String (ReaderT Card (StateT Board (WriterT [(String, Board)] Identity)))) a
type Formatter = Board -> String

makeLenses ''Board
makeLenses ''Card
makeLenses ''CardEffect


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

instance Show CardMatcher where
  show _ = "<matcher>"

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

mkStrength (p, t) = CardStrength p t
emptyCard = mkCard "" (Active, Hand)
mkCard name location =
  Card
    { _cardName = name
    , _location = location
    , _cardAttributes = mempty
    , _cardStrength = mempty
    , _cardDamage = 0
    , _cardLoyalty = 0
    , _cardEffects = mempty
    }


