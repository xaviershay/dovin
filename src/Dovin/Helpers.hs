module Dovin.Helpers where

import Dovin.Types

import Data.List (intercalate, sort)
import qualified Data.HashMap.Strict as M
import qualified Data.Set as S
import Data.Char (isDigit)
import Control.Lens (at, view, use, _1, _2, _Just, modifying)
import Control.Monad (forM_, when)
import Control.Monad.Except (throwError)
import Control.Monad.State (get)

import Debug.Trace

applyMatcherWithDesc :: CardMatcher -> Card -> Either String ()
applyMatcherWithDesc (CardMatcher d f) c =
  if f c then
    Right ()
  else
    Left d

hasAttribute attr = S.member attr . view cardAttributes

parseMana :: String -> ManaPool
-- sort puts the Xs at the back
parseMana pool = sort $ concatMap (\char -> if isDigit char then replicate (read [char]) 'X' else [char]) pool

requireCard :: CardName -> CardMatcher -> GameMonad Card
requireCard name f = do
  maybeCard <- use $ cards . at name

  case maybeCard of
    Nothing -> throwError $ "Card does not exist: " <> name
    Just card -> case applyMatcherWithDesc f card of
                   Right () -> return card
                   Left msg ->
                     throwError $ name <> " does not match requirements: " <> msg

-- TODO: Should probably be in Dovin.Actions
validateRemoved :: CardName -> GameMonad ()
validateRemoved targetName = do
  board <- get
  case view (cards . at targetName) board of
    Nothing -> return ()
    Just _ -> throwError $ "Card should be removed: " <> targetName


-- CARD MATCHERS
--
-- Matchers are used for both filtering sets of cards, and also for verifying
-- attributes of cards.
--
-- A wrapping type is used since I intend to add labels/introspection
-- capabilities at some point.

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

-- For each creature, for each effect, remove if no longer valid.
checkEffects :: GameMonad ()
checkEffects = do
  forCards mempty $ \cn -> do
    c <- requireCard cn mempty
    cs <- M.elems <$> use cards

    let effects = view cardEffects c
    let toRemove = filter (\(name, (m, _)) -> null $ filter (applyMatcher m) cs) . M.toList $ effects

    traceM . show $ "TO REMOVE: " <> intercalate ", " (map fst toRemove)

    when (not . null $ toRemove) $ do
      -- Remove all effects
      forM_ effects $ \(_, Effect _ f) -> do
        modifying
          (cards . at cn . _Just)
          f

      -- Drop toRemove effects
      forM_ toRemove $ \(name, _) -> do
        modifying
          (cards . at cn . _Just . cardEffects)
          (M.delete name)

      -- Apply remaining effects
      c <- requireCard cn mempty
      let effects = view cardEffects c
      forM_ effects $ \(_, Effect f _) -> do
        modifying
          (cards . at cn . _Just)
          f

forCards :: CardMatcher -> (CardName -> GameMonad ()) -> GameMonad ()
forCards matcher f = do
  cs <- use cards

  let matchingCs = filter (applyMatcher matcher) (M.elems cs)

  forM_ (map (view cardName) matchingCs) f
