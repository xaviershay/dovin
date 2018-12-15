module Solutions.GuildsOfRavnica1 where

import Control.Monad (forM_)

import Dovin

-- http://www.possibilitystorm.com/083-guilds-of-ravnica-puzzle-1/
solution :: GameMonad ()
solution = do
  step "Initial state" $ do
    setLife Opponent 5

    addCreature "Aurelia, Exemplar of Justice" (2, 5) (Active, Hand) ["flying", "mentor"]

    addCreature "Blade Instructor" (3, 1) (Active, Play) ["mentor"]
    addCreature "Goblin Banneret" (1, 1) (Active, Play) ["mentor"]
    addCreature "Barging Sergeant" (4, 2) (Active, Play) ["mentor"]
    addPlaneswalker "Angrath, the Flame-Chained" 5 (Active, Play)

    addCards 4 "Sacred Foundry" (Active, Play) ["land"]
    addCards 4 "Dragonskull Summit" (Active, Play) ["land"]

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
    modifyStrength "Goblin Banneret" (2, 0)

  step "Begin combat, put Aurelia's trigger on Banneret" $ do
    trigger "Aurelia, Exemplar of Justice"
    modifyStrength "Goblin Banneret" (2, 0)

  step "Attack with everything, stacking Mentor triggers on to Aurelia" $ do
    attackWith ["Aurelia, Exemplar of Justice", "Blade Instructor", "Goblin Banneret", "Barging Sergeant"]
    mentor "Blade Instructor" "Aurelia, Exemplar of Justice"
    mentor "Barging Sergeant" "Aurelia, Exemplar of Justice"
    mentor "Goblin Banneret" "Aurelia, Exemplar of Justice"

    damagePlayer "Aurelia, Exemplar of Justice"

    validateLife Opponent 0
