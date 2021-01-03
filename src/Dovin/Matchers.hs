module Dovin.Matchers where

import Dovin.Prelude
import Dovin.Attributes (creature)
import Dovin.Types

import qualified Data.Set as S
import Data.List (foldl')
import Control.Lens (_1)

-- CARD MATCHERS
--
-- Matchers are used for both filtering sets of cards, and also for verifying
-- attributes of cards.
matchDamage :: Int -> CardMatcher
matchDamage n = CardMatcher (show n <> " damage") $
  (==) n . view cardDamage

matchLoyalty :: Int -> CardMatcher
matchLoyalty n = CardMatcher (show n <> " loyalty") $
  (==) n . view cardLoyalty

matchPlusOneCounters :: Int -> CardMatcher
matchPlusOneCounters n = CardMatcher (show n <> " +1/+1 counters") $
  (==) n . view cardPlusOneCounters

matchMinusOneCounters :: Int -> CardMatcher
matchMinusOneCounters n = CardMatcher (show n <> " -1/-1 counters") $
  (==) n . view cardMinusOneCounters

matchZone :: Zone -> CardMatcher
matchZone loc = CardMatcher ("in zone " <> show loc) $
  (==) loc . view cardZone
matchLocation :: CardLocation -> CardMatcher
matchLocation loc = CardMatcher ("in location " <> show loc) $
  (==) loc . view cardLocation

matchInPlay = CardMatcher "in play" $ \c -> view cardZone c == Play

matchAttribute :: CardAttribute -> CardMatcher
matchAttribute attr = CardMatcher ("has attribute " <> attr) $
  S.member attr . view cardAttributes

matchAttributes :: [CardAttribute] -> CardMatcher
matchAttributes = foldl' (flip $ (<>) . matchAttribute) mempty

matchName :: CardName -> CardMatcher
matchName n = CardMatcher ("has name " <> n) $ (==) n . view cardName

matchOtherCreatures :: Card -> CardMatcher
matchOtherCreatures = matchOther creature

matchOther :: CardAttribute -> Card -> CardMatcher
matchOther attribute card =
     matchLocation (view cardLocation card)
  <> matchAttribute attribute
  <> invert (matchName (view cardName card))

matchController player = CardMatcher ("has controller " <> show player) $
  (==) player . view cardController

matchLesserPower n = CardMatcher ("power < " <> show n) $
  (< n) . view cardPower

matchProtection :: Color -> CardMatcher
matchProtection color = CardMatcher ("protection from " <> show color) $
  S.member color . view cardProtection

matchNone = CardMatcher "never match" (const False)

matchAll :: CardMatcher
matchAll = labelMatch "match all" mempty

matchCard :: Card -> CardMatcher
matchCard = matchName . view cardName

matchToughness :: Int -> CardMatcher
matchToughness n = labelMatch ("toughness = " <> show n) $ CardMatcher ""
  ((== n) . view cardToughness) <> matchAttribute creature

matchPower :: Int -> CardMatcher
matchPower n = labelMatch ("power = " <> show n) $ CardMatcher ""
  ((== n) . view cardPower) <> matchAttribute creature

matchStrength :: (Int, Int) -> CardMatcher
matchStrength (p, t) = labelMatch ("P/T = " <> show p <> "/" <> show t) $
  matchPower p <> matchToughness t

matchTarget :: Target -> CardMatcher
matchTarget t = labelMatch ("target = " <> show t) $ CardMatcher ""
  (elem t . view cardTargets)

matchAny :: (a -> CardMatcher) -> [a] -> CardMatcher
matchAny f = foldl' (flip matchOr) matchNone . map f

missingAttribute = invert . matchAttribute

(CardMatcher d1 f) `matchOr` (CardMatcher d2 g) =
    CardMatcher (d1 <> " or " <> d2) $ \c -> f c || g c

invert :: CardMatcher -> CardMatcher
invert (CardMatcher d f) = CardMatcher ("not " <> d) $ not . f

labelMatch :: String -> CardMatcher -> CardMatcher
labelMatch label (CardMatcher d f) = CardMatcher label f

applyMatcherWithDesc :: CardMatcher -> Card -> Either String ()
applyMatcherWithDesc (CardMatcher d f) c =
  if f c then
    Right ()
  else
    Left d

applyMatcher :: CardMatcher -> Card -> Bool
applyMatcher matcher c =
  case applyMatcherWithDesc matcher c of
    Left _ -> False
    Right _ -> True
