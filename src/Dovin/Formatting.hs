module Dovin.Formatting where

import Data.List (intercalate)

import Dovin.Types
import Dovin.Monad

attributeFormatter :: [(String, GameMonad String)] -> Formatter
attributeFormatter attrs board = do
  "\n      " <> (intercalate ", " . map (\(x, y) -> x <> ": " <> y) $
    map formatAttribute attrs)

  where
    formatAttribute :: (String, GameMonad String) -> (String, String)
    formatAttribute (label, m) =
      let Right value = execMonad board m in
      (label, value)

formatStrength (p, t) = show p <> "/" <> show t
