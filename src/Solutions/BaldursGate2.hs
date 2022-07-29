module Solutions.BaldursGate2 where
import Dovin.V4

-- NOTE: This solution is wrong (it doesn't match the published one), I'm pretty
-- sure because Astarion is dealing player damage in second combat where it
-- shouldn't because it was blocked in the first one.

solution :: GameMonad ()
solution = do
  step "Initial state" $ do
    as Active $ setLife 3
    as Opponent $ setLife 12

    as Opponent $ do
      withZone Play $ do
        addLands 4 "Op. Island"
        addLands 3 "Op. Swamp"

        withAttribute hexproof $ addCreature (5, 7) "Angler Turtle"
        withAttribute lifelink $ withPlusOneCounters 3 $ addCreature (1, 1) "Mold Folk 1"
        withAttribute lifelink $ withPlusOneCounters 4 $ addCreature (1, 1) "Mold Folk 2"

    as Active $ do
      withZone Play $ do
        addLands 2 "Plains"
        addLands 2 "Swamp"
        addLand "Furycalm Snarl"
        withAttributes [deathtouch, lifelink] $ addCreature (4, 4) "Astarion, the Decadent"
        withAttribute flying $ addCreature (3, 3) "Vrock"
        addCreature (4, 3) "Nothic"
        addCreature (2, 2) "Marching Duodrone"
      withZone Hand $ do
        addSorcery "Sevinne's Reclamation"
        addAura "Minimus Containment"
      withZone Graveyard $
        addCreature (2, 2) "Swashbuckler Extraordinaire"
       

  step "Cast Reclamation for Swashbuckler" $ do
    tapForMana "W" "Plains 1"
    tapForMana "B" "Swamp 1"
    tapForMana "R" "Furycalm Snarl"
    cast "2W" "Sevinne's Reclamation" >> resolveTop
    targetInLocation (Active, Graveyard) "Swashbuckler Extraordinaire"
    moveTo Play "Swashbuckler Extraordinaire"
    trigger "" "Swashbuckler Extraordinaire" >> resolveTop
    withZone Play $ withAttribute token $ addArtifact "Treasure 1"
    

  step "Contain Nothic" $ do
    tapForMana "W" "Plains 2"
    tapForMana "B" "Swamp 2"
    tapForMana "1" "Treasure 1"
    sacrifice "Treasure 1"

    cast "2W" "Minimus Containment" >> resolveTop
    target "Nothic" -- TODO: remove ability and treasure attribute

  step "Attack with all and stack triggers" $ do
    attackWith ["Astarion, the Decadent", "Vrock", "Marching Duodrone"]
    trigger "Double Strike" "Swashbuckler Extraordinaire"
    trigger "Create Treasure" "Marching Duodrone"
    
  step "Resolve duodrone treasures" $ do
    resolve "Create Treasure"
    withZone Play $ withAttribute token $ addArtifact "Treasure 2"
    as Opponent $
        withZone Play $ withAttribute token $ addArtifact "Treasure Opponent"

  step "Resolve Swashbuckler double strike for 2" $ do
    resolve "Double Strike"
    sacrifice "Treasure 2"
    sacrifice "Nothic" -- TODO: validate it's a treasure

    gainAttribute doublestrike "Astarion, the Decadent"
    gainAttribute doublestrike "Vrock"

  step "Opponent sacrifices turtle to maximize life gain" $ do
    as Opponent $ do
      tapForMana "U" "Op. Island 1"
      activate "U" "Mold Harvest" "Mold Folk 1"
      sacrifice "Angler Turtle"
      resolveTop
      modifyCard cardPlusOneCounters (+ 1) "Mold Folk 1"

  step "First strike damage" $ do
    -- TODO: A hack to workaround combatDamage not having a way to do first
    -- strike damage without receiving damage from the blocker
    gainAttribute indestructible "Astarion, the Decadent" 
    combatDamage ["Mold Folk 1"] "Astarion, the Decadent"
    combatDamage [] "Vrock"

  step "Combat damage" $ do
    combatDamage [] "Astarion, the Decadent"
    combatDamage ["Mold Folk 2"] "Marching Duodrone"
    combatDamage [] "Vrock"

  step "Stack end step triggers" $ do
    trigger "Feed" "Astarion, the Decadent"
    trigger "Toxic Spores" "Vrock"

  step "Resolve Toxic Spores" $ do
    resolve "Toxic Spores"
    -- TODO: no way to verify a permanent left battlefield, but the treasures did.
    as Opponent $ loseLife 3
    
  step "Resolve Feed" $ do
    resolve "Feed"
    -- TODO: How to track life lost?
    -- 3 from Toxic spores, 4 from second Astarion attack, 3+3 from Vrock
    as Opponent $ loseLife (3 + 4 + 3 + 3)

  validateLife (-4) Opponent

attributes = attributeFormatter $ do
  attribute "life" $ countLife Opponent

formatter step = case view stepNumber step of
  1 -> attributes <> boardFormatter
  _ -> attributes -- <> boardFormatter