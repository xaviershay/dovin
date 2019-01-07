{-# LANGUAGE FlexibleContexts #-}

module TestPrelude
  ( module Test.Tasty
  , module Test.Tasty.HUnit
  , module Dovin.V1
  , module Control.Lens
  , module Control.Monad
  , prove
  , refute
  , validateBoardEquals
  ) where

import Test.Tasty
import Test.Tasty.HUnit

import Dovin.V1
import Dovin.Monad

import Data.List (isInfixOf)

import Control.Monad
import Control.Lens
import Control.Monad.Except


prove name m = testCase name $
  case runMonad emptyBoard m of
    (Left msg, _, _) -> assertFailure msg
    (Right (), _, _) -> return mempty

refute name expectedFailure m = testCase name $
  case runMonad emptyBoard m of
    (Left msg, _, _) -> assertBool ("expected: " <> expectedFailure <> "\n but got: " <> msg) $ expectedFailure `isInfixOf` msg
    (Right (), _, _) -> assertFailure "proof was not refuted"

validateBoardEquals lens expected = do
  x <- use lens

  unless (x == expected) $
    throwError ("want: " <> show expected <> ", got: " <> show x)

