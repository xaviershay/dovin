module Solutions.GuildsOfRavnica1 where

import Control.Monad (forM_)

import Dovin.V1

-- http://www.possibilitystorm.com/083-guilds-of-ravnica-puzzle-1/
solution :: GameMonad ()
solution = do
  let mentor = "mentor"

  step "Initial state" $ do
    setLife Opponent 5

    withLocation (Active, Hand)
      $ withAttributes [flying, mentor]
      $ addCreature (2, 5) "Aurelia, Exemplar of Justice"

    withLocation (Active, Play) $ do
      withAttribute mentor $ do
        addCreature (3, 1) "Blade Instructor"
        addCreature (1, 1) "Goblin Banneret"
        addCreature (4, 2) "Barging Sergeant"
      addPlaneswalker 5 "Angrath, the Flame-Chained"

      addLands 4 "Sacred Foundry"
      addLands 4 "Dragonskull Summit"

  step "Cast Aurelia" $ do
    tapForMana "R" "Sacred Foundry 1"
    tapForMana "W" "Sacred Foundry 2"
    tapForMana "W" "Sacred Foundry 3"
    tapForMana "W" "Sacred Foundry 4"
    cast "2RW" "Aurelia, Exemplar of Justice"
    resolve "Aurelia, Exemplar of Justice"

  step "Angrath gain control, targeting Aurelia" $ do
    activatePlaneswalker (-3) "Angrath, the Flame-Chained"
    target "Aurelia, Exemplar of Justice"
    gainAttribute "haste" "Aurelia, Exemplar of Justice"

  step "Activate Goblin Banneret" $ do
    forM_ [1..2] $ \n -> tapForMana "R" (numbered n "Dragonskull Summit")
    activate "R1" "Goblin Banneret"
    modifyStrength (2, 0) "Goblin Banneret"

  step "Begin combat, put Aurelia's trigger on Banneret" $ do
    trigger "Aurelia, Exemplar of Justice"
    modifyStrength (2, 0) "Goblin Banneret"

  step "Attack with everything, stacking Mentor triggers on to Aurelia" $ do
    attackWith ["Aurelia, Exemplar of Justice", "Blade Instructor", "Goblin Banneret", "Barging Sergeant"]
    triggerMentor "Blade Instructor" "Aurelia, Exemplar of Justice"
    triggerMentor "Barging Sergeant" "Aurelia, Exemplar of Justice"
    triggerMentor "Goblin Banneret" "Aurelia, Exemplar of Justice"

    combatDamage [] "Aurelia, Exemplar of Justice"

    validateLife Opponent 0

formatter _ = boardFormatter
