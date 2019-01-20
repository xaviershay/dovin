module TargetInLocation where

import TestPrelude.V2

test_Target = testGroup "targetInLocation"
  [ prove "targets a card in graveyard" $ do
      withLocation Graveyard $ addArtifact "Mox"
      targetInLocation (Active, Graveyard) "Mox"
  , refute
      "requires location"
      "in location (Active,Graveyard)" $ do
        withLocation Play $ addArtifact "Mox"
        targetInLocation (Active, Graveyard) "Mox"
  ]
