module Effects where

import TestPrelude.V2

import qualified Data.Set as S

-- https://blogs.magicjudges.org/ftw/l2-prep/rules-and-policy/continuous-effects/
test_Effects = testGroup "V3 effects" $
  [ testGroup ""
    [ prove "Effects only apply when enabled" $ do
        withLocation Hand $ do
          withEffectV3
            (applyMatcher matchInPlay <$> askSelf)
            (pure $ matchAttribute creature)
            [ effectPTSet (1, 1)
            ]
            "Creatures are 1/1" $ do
              addCreature (4, 4) "Some Creature"

          c <- requireCard "Some Creature" mempty

          unless (view cardStrength c == mkStrength (4, 4)) $
            throwError "Applied effect when disabled"
    , prove "Commander's Plate" $ do
        withLocation Play $
          withEffectV3
            (applyMatcher matchInPlay <$> askSelf)
            (do
              ts <- viewSelf cardTargets
              return $ foldl (\a v -> a `matchOr` (matchTarget v)) mempty ts
            )
            [ effectPTAdjustment (3, 3)
            , effectProtectionF (
                const $ do
                  --owner <- viewSelf cardOwner
                  commanders <- askCards $ matchAttribute "commander" -- <> matchOwner owner

                  -- TODO: Need to invert color list!
                  let colors =   foldr (<>) mempty
                               . map (view cardColors)
                               $ commanders

                  return colors
              )
              ]
            "+3/+3 and pro commander colors" $
              addArtifact "Commander's Plate"
    ]
  , testGroup "effects (magic judges examples)"
    [ prove "Humble & Godhead of Awe" $ do
        withLocation Play $ do
          addCreature (2, 2) "Grizzly Bear"
          withEffectV3
            (applyMatcher matchInPlay <$> askSelf)
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
          withCMC 4 $ withEffectV3
            (applyMatcher matchInPlay <$> askSelf)
            (pure $ matchAttribute creature)
            [ effectPTSet (0, 1)
            , effectNoAbilities
            ]
            "Each creature loses all abilities and is 0/1" $
              addEnchantment "Humility"

          withCMC 4 $ withEffectV3
            (applyMatcher matchInPlay <$> askSelf)
            (matchOther enchantment <$> askSelf)
            [ effectPTSetF (\c -> let cmc = view cardCmc c in return (cmc, cmc))
            , effectType creature
            ]
            "Other enchanments are creatures with P/T equal to CMC" $
              addEnchantment "Opalescence"

          c <- requireCard "Humility" mempty

          unless (view cardStrength c == mkStrength (4, 4)) $
            throwError "Did not make Humility a 4/4"
    , prove "Opalescence & Humility" $ do
        withLocation Play $ do
          withCMC 4 $ withEffectV3
            (applyMatcher matchInPlay <$> askSelf)
            (matchOther enchantment <$> askSelf)
            [ effectPTSetF (\c -> let cmc = view cardCmc c in return (cmc, cmc)) , effectType creature
            ]
            "Other enchanments are creatures with P/T equal to CMC" $
              addEnchantment "Opalescence"

          withCMC 4 $ withEffectV3
           (pure True)
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