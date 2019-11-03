module Life where

import TestPrelude.V2

test_Life = testGroup "life"
  [ prove "manipulates life total" $ do
      as Opponent $ do
        setLife 10
        validateLife 10 Opponent
        gainLife 2
        validateLife 12 Opponent
        loseLife 1
        validateLife 11 Opponent
  ]
