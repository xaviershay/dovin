{-# LANGUAGE FlexibleContexts #-}
module Dovin.Formatting where

import Dovin.Helpers
import Dovin.Matchers
import Dovin.Monad
import Dovin.Prelude
import Dovin.Types

import Control.Monad.Writer (Writer, execWriter, tell)

import qualified Data.HashMap.Strict as M
import qualified Data.Set as S
import Data.List (intercalate, sort, sortBy, nub)
import Data.Ord (comparing)

type FormatMonad = Writer [(String, GameMonad String)]

blankFormatter :: Formatter
blankFormatter _ = ""

attributeFormatter :: FormatMonad () -> Formatter
attributeFormatter m =
  f $ execWriter m
  where
    f :: [(String, GameMonad String)] -> Formatter
    f attrs board =
      "\n      " <> (intercalate ", " . map (\(x, y) -> x <> ": " <> y) $
        map formatAttribute attrs)

      where
        formatAttribute :: (String, GameMonad String) -> (String, String)
        formatAttribute (label, m) =
          let Right value = execMonad board m in
          (label, value)

stackFormatter :: Formatter
stackFormatter board =
  let matchingCs = map lookupCard $ view stack board in

  "\n      Stack:\n" <> formatCards matchingCs

  where
    lookupCard cn =
      let Right value = execMonad board (requireCard cn mempty) in value


cardFormatter :: String -> CardMatcher -> Formatter
cardFormatter title matcher board =
  let matchingCs = sortBy (comparing $ view cardName) . filter (applyMatcher matcher) $ cs in

    "\n      " <> title <> ":\n" <> formatCards matchingCs

  where
    cs = let Right value = execMonad board allCards in value

formatCards = intercalate "\n" . map (("      " <>) . formatCard)

formatCard c =
  let targets = view cardTargets c in
  "  " <> view cardName c <>
  " (" <> (intercalate "," . sort . S.toList $ view cardAttributes c) <> ")"
  <> if hasAttribute "creature" c then
       " ("
         <> show (view cardPower c)
         <> "/"
         <> show (view cardToughness c)
         <> (let n = view cardPlusOneCounters c in
              if n > 0 then
                ", +" <> show n <> "/+" <> show n
              else
                mempty)
         <> (let n = view cardMinusOneCounters c in
              if n > 0 then
                ", -" <> show n <> "/-" <> show n
              else
                mempty)
         <> (if view cardDamage c > 0 then
              ", " <> show (view cardDamage c)
            else
              mempty)
         <> ")"
     else if hasAttribute "planeswalker" c then
       " ("
         <> show (view cardLoyalty c)
         <> ")"
     else
      ""
  <> if length targets > 0 then
       " (targets: "
       <> (intercalate "," . sort . map formatTarget $ targets)
       <> ")"
     else
       ""
  where
    formatTarget (TargetCard cn) = cn
    formatTarget (TargetPlayer p) = show p

boardFormatter :: Formatter
boardFormatter board =
  let allLocations = nub . sort . map (view location) $ cs in

  let formatters = map
                     formatLocation
                     allLocations in

  mconcat formatters board

  where
    cs = let Right value = execMonad board allCards in value
    formatLocation (Active, Stack) = stackFormatter
    formatLocation l = cardFormatter (show l) (matchLocation l)

attribute :: Show a => String -> GameMonad a -> FormatMonad ()
attribute label m = tell [(label, show <$> m)]

countLife :: Player -> GameMonad Int
countLife player = use (life . at player . non 0)

countValue :: String -> GameMonad Int
countValue name = use (counters . at name . non 0)

countCards :: CardMatcher -> GameMonad Int
countCards matcher =
  length . filter (applyMatcher matcher) <$> allCards

countManaPool :: Player -> GameMonad Int
countManaPool p = length <$> use (manaPoolFor p)
