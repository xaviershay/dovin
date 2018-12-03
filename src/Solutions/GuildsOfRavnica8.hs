module Solutions.GuildsOfRavnica8 where

import Control.Monad.Except (throwError, when)
import Control.Lens
import qualified Data.Set as S

import Dovin

-- http://www.possibilitystorm.com/089-guilds-of-ravnica-season-puzzle-7-2/
solution :: GameMonad ()
solution = do
  -- This solutions relies on triggering Diamond Mare to gain life, which in
  -- turns triggers Epicure of Blood to cause the opponent to lose life. This
  -- helper can wrap cast actions with that combination.
  let withTriggers = \action name -> do
        action name
        trigger "Diamond Mare"
        c <- requireCard name mempty

        when (hasAttribute "black" c) $ do
          gainLife Active 1
          trigger "Epicure of Blood"
          loseLife Opponent 1


  -- Helper function to keep track of which permanent types have been cast
  -- using Muldrotha's ability.
  let castWithMuldrotha = \ptype cn -> do
        let ptypes = S.fromList ["artifact", "creature", "land", "enchantment"]
        let counterName = "muldrotha-" <> ptype

        when (not $ S.member ptype ptypes) $
          throwError $ "Invalid permanent type: " <> ptype

        n <- use (counters . at counterName . non 0)

        if n > 0 then
          throwError $ "Already cast card of type with Muldrotha: " <> ptype
        else
          do
            castFromLocation cn (Active, Graveyard)
            resolve cn
            assign (counters . at counterName) (Just 1)

  step "Initial state" $ do
    setLife Opponent 7

    addCreature "Epicure of Blood" (4, 4) (Active, Play) []
    addCreature "Muldrotha, the Gravetide" (6, 6) (Active, Play) []
    addCreature "Diamond Mare" (1, 3) (Active, Graveyard) ["artifact"]
    addCard "Detection Tower" (Active, Graveyard) ["land"]
    addCard "Mox Amber" (Active, Graveyard) ["artifact"]

    addCards 3 "Memorial to Folly" (Active, Play) ["land"]
    addCards 4 "Watery Grave" (Active, Play) ["land"]
    addCards 4 "Overgrown Tomb" (Active, Play) ["land"]

    addCard "March of the Drowned" (Active, Hand) ["sorcery", "black"]
    addCard "Gruesome Menagerie" (Active, Hand) ["sorcery", "black"]
    addCard "Dead Weight" (Active, Hand) ["aura", "black"]
    addCard "Find" (Active, Hand) ["sorcery", "black"]
    addCreature "Vicious Conquistador" (1, 2) (Active, Graveyard) ["black"]
    addCreature "Sailor of Means" (1, 4) (Active, Graveyard) []

  step "Detection Tower, Mox Amber, Diamond Mare from graveyard" $ do
    castWithMuldrotha "land" "Detection Tower"
    castWithMuldrotha "artifact" "Mox Amber"
    tap "Detection Tower"
    tap "Mox Amber"
    castWithMuldrotha "creature" "Diamond Mare"

  step "March of the Drowned on Vicious Conquistador" $ do
    tap "Memorial to Folly 1"
    withTriggers cast "March of the Drowned"
    resolve "March of the Drowned"
    returnToHand "Vicious Conquistador"

  step "Vicious Conquistador" $ do
    tap "Memorial to Folly 2"
    withTriggers cast "Vicious Conquistador"
    resolve "Vicious Conquistador"

  step "Dead Weight on Vicious Conquistador" $ do
    tap "Memorial to Folly 3"
    withTriggers cast "Dead Weight"
    target "Vicious Conquistador"
    resolve "Dead Weight"
    modifyStrength "Vicious Conquistador" (-2, -2)
    resetStrength "Vicious Conquistador" (1, 2)
    moveToGraveyard "Dead Weight"

  step "Gruesome Menagerie for Sailor of Means and Vicious Conquistador" $ do
    tap "Watery Grave 1"
    tap "Watery Grave 2"
    tap "Watery Grave 3"
    tap "Watery Grave 4"
    tap "Overgrown Tomb 1"
    withTriggers cast "Gruesome Menagerie"
    resolve "Gruesome Menagerie"
    targetInLocation "Vicious Conquistador" (Active, Graveyard)
    targetInLocation "Sailor of Means" (Active, Graveyard)
    returnToPlay "Vicious Conquistador"
    returnToPlay "Sailor of Means"
    addCard "Treasure" (Active, Play) ["artifact"]

  step "Dead Weight on Vicious Conquistador" $ do
    tap "Overgrown Tomb 2"
    withTriggers (castWithMuldrotha "enchantment") "Dead Weight"
    target "Vicious Conquistador"
    modifyStrength "Vicious Conquistador" (-2, -2)
    resetStrength "Vicious Conquistador" (1, 2)
    moveToGraveyard "Dead Weight"

  step "Find for Vicous Conquistador" $ do
    tap "Overgrown Tomb 3"
    tap "Overgrown Tomb 4"
    withTriggers cast "Find"
    resolve "Find"
    targetInLocation "Vicious Conquistador" (Active, Graveyard)
    returnToHand "Vicious Conquistador"

  step "Vicious Conquistador" $ do
    tap "Treasure"
    withTriggers cast "Vicious Conquistador"
    resolve "Vicious Conquistador"

    validateLife Opponent 0
