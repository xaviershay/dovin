module Sacrifice where

import TestPrelude.V2

test_Sacrifice = testGroup "sacrifice"
  [ prove "moves card to graveyard" $ do
      withLocation Play $ addArtifact "Mox"
      sacrifice "Mox"
      validate (matchLocation (Active, Graveyard)) "Mox"
  , refute
      "cannot sacrifice permanents owned by opponent"
      "has controller Active" $ do
        as Opponent $ withLocation Play $ addArtifact "Mox"
        sacrifice "Mox"
  , refute
      "requires in play"
      "in play" $ do
        withLocation Graveyard $ addArtifact "Mox"
        destroy "Mox"
  ]
