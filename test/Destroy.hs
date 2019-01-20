module Destroy where

import TestPrelude.V2

test_Destroy = testGroup "destroy"
  [ prove "destroys a card in play" $ do
      withLocation Play $ addArtifact "Mox"
      destroy "Mox"
      validate (matchLocation (Active, Graveyard)) "Mox"
  , refute
      "cannot destroy indestructible"
      "not has attribute indestructible" $ do
        withLocation Play $ withAttribute indestructible $ addArtifact "Mox"
        destroy "Mox"
  , refute
      "requires in play"
      "in play" $ do
        withLocation Graveyard $ addArtifact "Mox"
        destroy "Mox"
  ]
