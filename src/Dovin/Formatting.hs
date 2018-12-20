{-# LANGUAGE FlexibleContexts #-}
module Dovin.Formatting where

import Control.Lens
import Control.Monad.Writer
import qualified Data.HashMap.Strict as M
import qualified Data.Set as S
import Data.List (intercalate, sort, nub)

import Dovin.Helpers
import Dovin.Monad
import Dovin.Types

type FormatMonad = Writer [(String, GameMonad String)]

blankFormatter :: Formatter
blankFormatter _ = ""

attributeFormatter :: FormatMonad () -> Formatter
attributeFormatter m = do
  f $ execWriter m
  where
    f :: [(String, GameMonad String)] -> Formatter
    f attrs board = do
      "\n      " <> (intercalate ", " . map (\(x, y) -> x <> ": " <> y) $
        map formatAttribute attrs)

      where
        formatAttribute :: (String, GameMonad String) -> (String, String)
        formatAttribute (label, m) =
          let Right value = execMonad board m in
          (label, value)

cardFormatter :: String -> CardMatcher -> Formatter
cardFormatter title matcher board =
  let matchingCs = filter (applyMatcher matcher) cs in

    "\n      " <> title <> ":\n" <> (intercalate "\n" . sort $ map (("      " <>) . formatCard) matchingCs)

  where
    cs = let Right value = execMonad board allCards in value

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

boardFormatter :: Formatter
boardFormatter board =
  let allLocations = nub . sort . map (view location) $ cs in

  let formatters = map (\l -> cardFormatter (show l) (matchLocation l)) allLocations :: [Formatter] in

  mconcat formatters $ board

  where
    cs = let Right value = execMonad board allCards in value

attribute :: Show a => String -> GameMonad a -> FormatMonad ()
attribute label m = tell [(label, show <$> m)]

countLife :: Player -> GameMonad Int
countLife player = use (life . at player . non 0)

countValue :: String -> GameMonad Int
countValue name = use (counters . at name . non 0)

countCards :: CardMatcher -> GameMonad Int
countCards matcher =
  length . filter (applyMatcher matcher) <$> allCards

countManaPool :: GameMonad Int
countManaPool =
  length <$> use manaPool
