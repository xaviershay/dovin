{-# LANGUAGE ScopedTypeVariables #-}

module Dovin.Helpers where

import Dovin.Types

import Data.List (sort)
import Debug.Trace
import qualified Data.HashMap.Strict as M
import qualified Data.Set as S
import Data.Char (isDigit)
import Control.Lens (at, view, use, _1, _2, ASetter, over, _Just, modifying)
import Control.Monad (foldM, when, unless)
import Control.Monad.Except (throwError, catchError)
import Control.Monad.State (get)

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
    Just card -> do
      card' <- applyEffects card
      case applyMatcherWithDesc f card' of
        Right () -> return card'
        Left msg ->
          throwError $ name <> " does not match requirements: " <> msg

applyEffects :: BaseCard -> GameMonad Card
applyEffects (BaseCard card) = do
  cs <- map unwrap . M.elems <$> use cards

  let allEffects =
        concatMap
          (\c -> map (\e -> (e, c)) . view cardEffects $ c)
          cs

  let enabledEffects =
        filter
          (\(e, c) -> applyMatcher (view effectEnabled e) c)
          allEffects

  let applicableEffects =
        filter
          (\(e, c) -> applyMatcher ((view effectFilter e) c) card)
          enabledEffects

  foldM (\c (e, _) -> applyEffect2 c e) card applicableEffects

  where
    applyEffect2 :: Card -> CardEffect -> GameMonad Card
    applyEffect2 card e = (view effectAction e) card

    unwrap :: BaseCard -> Card
    unwrap (BaseCard card) = card

-- TODO: Should probably be in Dovin.Actions
validateRemoved :: CardName -> GameMonad ()
validateRemoved targetName = do
  board <- get
  case view (cards . at targetName) board of
    Nothing -> return ()
    Just _ -> throwError $ "Card should be removed: " <> targetName

validatePhase :: Phase -> GameMonad ()
validatePhase expected = do
  actual <- use phase

  when (actual /= expected) $
    throwError $ "phase was "
      <> show actual
      <> ", expected "
      <> show expected

validateCanCastSorcery :: GameMonad ()
validateCanCastSorcery = do
  validatePhase FirstMain
    `catchError` (const $ validatePhase SecondMain)
    `catchError` (const $ throwError "not in a main phase")

  s <- use stack

  unless (null s) $ throwError "stack is not empty"

allCards :: GameMonad [Card]
allCards = do
  bases <- M.elems <$> use cards

  mapM applyEffects bases

modifyCard :: CardName -> ASetter Card Card a b -> (a -> b) -> GameMonad ()
modifyCard name lens f =
  modifying
    (cards . at name . _Just)
    (\(BaseCard c) -> BaseCard $ over lens f c)

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

matchAttributes :: [CardAttribute] -> CardMatcher
matchAttributes (x:xs) = matchAttribute x <> matchAttributes xs
matchAttributes [] = mempty

matchName :: CardName -> CardMatcher
matchName n = CardMatcher ("has name " <> n) $ (==) n . view cardName

matchOtherCreatures :: Card -> CardMatcher
matchOtherCreatures card = matchLocation (view cardLocation card) <> (invert $ matchName (view cardName card))

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
