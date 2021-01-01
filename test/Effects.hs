module Effects where

import TestPrelude.V3

test_Effects = testGroup "V3 effects" $
  [ testGroup "assorted cards"
    [ prove "Effects only apply when enabled" $ do
        withLocation Hand $ do
          withEffect
            (pure $ matchAttribute creature)
            [ effectPTSet (1, 1)
            ]
            "Creatures are 1/1" $ do
              addCreature (4, 4) "Some Creature"

          c <- requireCard "Some Creature" mempty

          unless (view cardStrength c == mkStrength (4, 4)) $
            throwError "Applied effect when disabled"
    , prove "Sliver Legion" $ do
        let sliver = "sliver"
        withLocation Play $ do
          withAttribute sliver $
            withEffect
              (pure $ matchAttribute sliver <> matchInPlay)
              [ effectPTAdjustF . const $ do
                  self <- askSelf
                  slivers <- length <$> askCards (
                                             matchAttribute sliver
                                          <> matchInPlay
                                          <> matchOtherCreatures self)

                  return (slivers, slivers)
              ]
              "All Slivers get +1/+1 for each other Sliver"
                $ addCreature (7, 7) "Sliver Legion"

          c <- requireCard "Sliver Legion" mempty

          unless (view cardStrength c == mkStrength (7, 7)) $
            throwError "No other slivers yet, should be 7/7"

          withLocation Play $ do
            withAttribute sliver $ addCreature (2, 2) "Sliver 1"
            withAttribute sliver $ addCreature (2, 2) "Sliver 2"

          c <- requireCard "Sliver Legion" mempty

          unless (view cardStrength c == mkStrength (9, 9)) $
            throwError "Should get +2/+2 from other slivers"

          c <- requireCard "Sliver 1" mempty

          unless (view cardStrength c == mkStrength (4, 4)) $
            throwError "Should get +2/+2 from other slivers"

    , prove "Drover of the Mighty" $ do
        let dinosaur = "dinosaur"

        as Opponent $ do
          withLocation Play $ do
            withAttribute dinosaur $ addCreature (4, 4) "Dinosaur Op."

        withLocation Hand $ do
          withAttribute dinosaur $ addCreature (4, 4) "Dinosaur Hand"

        withLocation Play $ do
          withEffectWhen
            (do
              controller <- viewSelf cardOwner
              dinos <- askCards $ matchAttribute dinosaur
                               <> matchInPlay
                               <> matchController controller

              return $ length dinos > 0
            )
            (matchCard <$> askSelf)
            [ effectPTAdjust (2, 2)
            ]
            "+2/+2 so long as you control another dinosaur"
              $ addCreature (1, 1) "Drover of the Mighty"

        c <- requireCard "Drover of the Mighty" mempty

        unless (view cardStrength c == mkStrength (1, 1)) $
          throwError "Should still be a 1/1, no dinos"

        withLocation Play $ do
          withAttribute dinosaur $ addCreature (4, 4) "Dinosaur"

        c <- requireCard "Drover of the Mighty" mempty

        unless (view cardStrength c == mkStrength (3, 3)) $
          throwError "Should be a 3/3"


    --, prove "Commander's Plate" $ do
    --    withLocation Play $
    --      withEffect
    --        (do
    --          ts <- viewSelf cardTargets
    --          return $ foldl (\a v -> a `matchOr` (matchTarget v)) mempty ts
    --        )
    --        [ effectPTAdjust (3, 3)
    --        , effectProtectionF (
    --            const $ do
    --              --owner <- viewSelf cardOwner
    --              commanders <- askCards $ matchAttribute "commander" -- <> matchOwner owner

    --              -- TODO: Need to invert color list!
    --              let colors =   foldr (<>) mempty
    --                           . map (view cardColors)
    --                           $ commanders

    --              return colors
    --          )
    --          ]
    --        "+3/+3 and pro commander colors" $
    --          addArtifact "Commander's Plate"
    ]
  -- https://blogs.magicjudges.org/ftw/l2-prep/rules-and-policy/continuous-effects/
  , testGroup "effects (magic judges examples)"
    [ prove "Humble & Godhead of Awe" $ do
        withLocation Play $ do
          addCreature (2, 2) "Grizzly Bear"
          withEffect
            (matchOtherCreatures <$> askSelf)
            [ effectPTSet (1, 1)
            ]
            "Other creatures are 1/1" $ do
              addCreature (4, 4) "Godhead of Awe"

          c <- requireCard "Grizzly Bear" mempty

          unless (view cardStrength c == mkStrength (1, 1)) $
            throwError "Did not apply 1/1 effect"

          addEffect (effectPTSet (0, 1)) "Godhead of Awe"
          addEffect (effectNoAbilities) "Godhead of Awe"

          c <- requireCard "Grizzly Bear" mempty

          unless (view cardStrength c == mkStrength (2, 2)) $
            throwError "Did not remove ability"
    , prove "Humility & Opalescence" $ do
        withLocation Play $ do
          withCMC 4 $ withEffect
            (pure $ matchAttribute creature)
            [ effectPTSet (0, 1)
            , effectNoAbilities
            ]
            "Each creature loses all abilities and is 0/1" $
              addEnchantment "Humility"

          withCMC 4 $ withEffect
            (matchOther enchantment <$> askSelf)
            [ effectPTSetF (\c -> let cmc = view cardCmc c in return (cmc, cmc))
            , effectAddType creature
            ]
            "Other enchanments are creatures with P/T equal to CMC" $
              addEnchantment "Opalescence"

          c <- requireCard "Humility" mempty

          unless (view cardStrength c == mkStrength (4, 4)) $
            throwError "Did not make Humility a 4/4"
    , prove "Opalescence & Humility" $ do
        withLocation Play $ do
          withCMC 4 $ withEffect
            (matchOther enchantment <$> askSelf)
            [ effectPTSetF (\c -> let cmc = view cardCmc c in return (cmc, cmc)) , effectAddType creature
            ]
            "Other enchanments are creatures with P/T equal to CMC" $
              addEnchantment "Opalescence"

          withCMC 4 $ withEffect
           (pure $ matchAttribute creature)
           [ effectPTSet (0, 1)
           , effectNoAbilities
           ]
           "Each creature loses all abilities and is 0/1" $
             addEnchantment "Humility"


          c <- requireCard "Humility" mempty

          unless (view cardStrength c == mkStrength (0, 1)) $
            throwError "Did not make Humility a 0/1"
    ]
  ]
