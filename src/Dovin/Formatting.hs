{-# LANGUAGE FlexibleContexts #-}
module Dovin.Formatting where

import Control.Lens
import Control.Monad.Writer
import qualified Data.HashMap.Strict as M
import Data.List (intercalate)

import Dovin.Helpers
import Dovin.Monad
import Dovin.Types

type FormatMonad = Writer [(String, GameMonad String)]

attributeFormatter :: FormatMonad () -> Formatter
attributeFormatter m = do
  f $ execWriter m
  where
    f :: [(String, GameMonad String)] -> Formatter
    f attrs board = do
      "\n     " <> (intercalate ", " . map (\(x, y) -> x <> ": " <> y) $
        map formatAttribute attrs)

      where
        formatAttribute :: (String, GameMonad String) -> (String, String)
        formatAttribute (label, m) =
          let Right value = execMonad board m in
          (label, value)

attribute :: Show a => String -> GameMonad a -> FormatMonad ()
attribute label m = tell [(label, show <$> m)]

countLife :: Player -> GameMonad Int
countLife player = use (life . at player . non 0)

countValue :: String -> GameMonad Int
countValue name = use (counters . at name . non 0)

countCards :: CardMatcher -> GameMonad Int
countCards matcher =
  length . filter (applyMatcher matcher) . M.elems <$> use cards
