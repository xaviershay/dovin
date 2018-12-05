{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module Dovin.Types where

import GHC.Generics

import qualified Data.HashMap.Strict as M
import qualified Data.Set as S

import Data.Hashable (Hashable)
import Control.Arrow (first, second)
import Control.Lens (makeLenses, view, _1, _2, Lens', over)
import Control.Monad.Identity (Identity)
import           Control.Monad.Except (ExceptT)
import           Control.Monad.Writer (WriterT)
import           Control.Monad.State (StateT)

type CardName = String
type CardAttribute = String
data Player = Active | Opponent deriving (Show, Eq, Generic, Ord)
-- This is pretty dodgy - one char per mana - but works for now.
type ManaPool = String
type ManaString = String
-- TODO: Stack shouldn't be in here because there is only one of them
data Location = Hand | Graveyard | Play | Stack | Exile
  deriving (Show, Eq, Ord)

type CardLocation = (Player, Location)
type CardAttributes = S.Set CardAttribute
newtype CardStrength = CardStrength (Int, Int) deriving (Show, Eq)

data Phase = FirstMain | Combat deriving (Show, Eq)

data Card = Card
  { _cardName :: CardName
  , _location :: (Player, Location)
  , _cardAttributes :: CardAttributes
  , _cardStrength :: CardStrength
  , _cardDamage :: Int
  , _cardLoyalty :: Int
  } deriving (Show, Eq)
instance Hashable Player

data CardMatcher = CardMatcher String (Card -> Bool)
data Effect = Effect (Card -> Card) (Card -> Card)
type EffectName = String

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
  , _effects :: M.HashMap EffectName (CardMatcher, Effect)
  , _manaPool :: ManaPool
  , _phase :: Phase
  }

type GameMonad a = (ExceptT String (StateT Board (WriterT [(String, Board)] Identity))) a
type Formatter = Board -> String

makeLenses ''Board
makeLenses ''Card

-- TODO: How to define these lenses using built-in Lens primitives
-- (Control.Lens.Wrapped?)
cardPower :: Control.Lens.Lens' Card Int
cardPower f parent = fmap
  (\x -> over cardStrength (CardStrength . first (const x) . unwrap) parent)
  (f . fst . unwrap . view cardStrength $ parent)
  where
    unwrap (CardStrength x) = x

cardToughness :: Control.Lens.Lens' Card Int
cardToughness f parent = fmap
  (\x -> over cardStrength (CardStrength . second (const x) . unwrap) parent)
  (f . snd . unwrap . view cardStrength $ parent)
  where
    unwrap (CardStrength x) = x

instance Show CardMatcher where
  show _ = "<matcher>"

instance Semigroup CardMatcher where
  (CardMatcher d1 f) <> (CardMatcher d2 g) =
    CardMatcher (d1 <> " and " <> d2) $ \c -> f c && g c

instance Monoid CardMatcher where
  mempty = CardMatcher "" $ const True

instance Semigroup CardStrength where
  CardStrength (p1, t1) <> CardStrength (p2, t2) =
   CardStrength (p1 + p2, t1 + t2)

instance Monoid CardStrength where
  mempty = CardStrength (0, 0)
