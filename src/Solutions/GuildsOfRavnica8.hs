module Solutions.GuildsOfRavnica8 where

import Control.Monad.Except (throwError, when)
import Control.Lens
import qualified Data.Set as S

import Dovin.V4

-- http://www.possibilitystorm.com/089-guilds-of-ravnica-season-puzzle-7-2/
solution :: GameMonad ()
solution = do
  let black = "black"

  -- This solution relies on triggering Diamond Mare to gain life, which in
  -- turns triggers Epicure of Blood to cause the opponent to lose life. This
  -- helper can wrap cast actions with that combination.
  let withTriggers = \action name -> do
        action name
        trigger "Mare" "Diamond Mare" >> resolveTop

        whenMatch (matchAttribute black) name $ do
          gainLife 1
          trigger "Epicure" "Epicure of Blood" >> resolveTop
          as Opponent $ loseLife 1

  -- Helper function to keep track of which permanent types have been cast
  -- using Muldrotha's ability.
  let castWithMuldrotha = \ptype mana cn -> do
        let ptypes = S.fromList ["artifact", "creature", "land", "enchantment"]
        let counterName = "muldrotha-" <> ptype

        when (not $ S.member ptype ptypes) $
          throwError $ "Invalid permanent type: " <> ptype

        n <- use (counters . at counterName . non 0)

        if n > 0 then
          throwError $ "Already cast card of type with Muldrotha: " <> ptype
        else
          do
            castFromZone Graveyard mana cn
            resolve cn
            assign (counters . at counterName) (Just 1)

  step "Initial state" $ do
    as Opponent $ setLife 7

    withZone Play $ do
      addCreature (4, 4) "Epicure of Blood"
      addCreature (6, 6) "Muldrotha, the Gravetide"

      addLands 3 "Memorial to Folly"
      addLands 4 "Watery Grave"
      addLands 4 "Overgrown Tomb"

    withZone Graveyard $ do
      withAttribute artifact $ addCreature (1, 3) "Diamond Mare"
      addLand "Detection Tower"
      addArtifact "Mox Amber"
      withAttribute black $ addCreature (1, 2) "Vicious Conquistador"
      addCreature (1, 4) "Sailor of Means"

    withZone Hand $ withAttribute black $ do
      addSorcery "March of the Drowned"
      addSorcery "Gruesome Menagerie"
      withEffect
        matchAttached
        [ effectPTAdjust (-2, -2) ]
        "-2/-2"
        $ addAura "Dead Weight"
      addSorcery "Find"

  step "Detection Tower, Mox Amber, Diamond Mare from graveyard" $ do
    castWithMuldrotha "land" "" "Detection Tower"
    castWithMuldrotha "artifact" "" "Mox Amber"
    tapForMana "1" "Detection Tower"
    tapForMana "1" "Mox Amber"
    castWithMuldrotha "creature" "2" "Diamond Mare"

  step "March of the Drowned on Vicious Conquistador" $ do
    tapForMana "B" "Memorial to Folly 1"
    withTriggers (cast "B") "March of the Drowned"
    resolve "March of the Drowned"
    moveTo Hand "Vicious Conquistador"

  step "Vicious Conquistador" $ do
    tapForMana "B" "Memorial to Folly 2"
    withTriggers (cast "B") "Vicious Conquistador"
    resolve "Vicious Conquistador"

  step "Dead Weight on Vicious Conquistador" $ do
    tapForMana "B" "Memorial to Folly 3"
    withTriggers (cast "B") "Dead Weight" >> resolveTop
    withStateBasedActions $
      attach "Dead Weight" "Vicious Conquistador"
    moveTo Graveyard "Dead Weight"

  step "Gruesome Menagerie for Sailor of Means and Vicious Conquistador" $ do
    tapForMana "B" "Watery Grave 1"
    tapForMana "B" "Watery Grave 2"
    tapForMana "B" "Watery Grave 3"
    tapForMana "B" "Watery Grave 4"
    tapForMana "B" "Overgrown Tomb 1"
    withTriggers (cast "3BB") "Gruesome Menagerie"
    resolve "Gruesome Menagerie"
    targetInLocation (Active, Graveyard) "Vicious Conquistador"
    targetInLocation (Active, Graveyard) "Sailor of Means"
    moveTo Play "Vicious Conquistador"
    moveTo Play "Sailor of Means"
    withZone Play
      $ withAttribute token
      $ addArtifact "Treasure"

  step "Dead Weight on Vicious Conquistador" $ do
    tapForMana "B" "Overgrown Tomb 2"
    withTriggers (castWithMuldrotha "enchantment" "B") "Dead Weight"
    withStateBasedActions $
      attach "Dead Weight" "Vicious Conquistador"
    moveTo Graveyard "Dead Weight"

  step "Find for Vicous Conquistador" $ do
    tapForMana "B" "Overgrown Tomb 3"
    tapForMana "B" "Overgrown Tomb 4"
    withTriggers (cast "BB") "Find"
    resolve "Find"
    targetInLocation (Active, Graveyard) "Vicious Conquistador"
    moveTo Hand "Vicious Conquistador"

  step "Vicious Conquistador" $ do
    tapForMana "B" "Treasure"
    sacrifice "Treasure"
    withTriggers (cast "B") "Vicious Conquistador"
    resolve "Vicious Conquistador"

    validateLife 0 Opponent

formatter = oldFormatter . view stepNumber

oldFormatter :: Int -> Formatter
oldFormatter _ = attributeFormatter $ do
  attribute "mana" $ countCards (matchAttribute "land" <> missingAttribute "tapped")
  attribute "life" $ countLife Opponent
