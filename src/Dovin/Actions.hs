{-|
Actions correspond to things you can do in Magic. They progress the state
machine while verifying applicable properties. The all run inside a
'GameMonad'.

Actions will modify the state as specified by the effects listed in their
documentation. If any of the validation steps fail away, the proof will fail.
Actions are /not/ atomic: if one fails, some effects may have already been
applied.
-}
module Dovin.Actions (
  step
  , fork
  -- * Casting
  , cast
  , castFromLocation
  , counter
  , flashback
  , jumpstart
  , resolve
  , resolveTop
  , resolveMentor
  , splice
  , tapForMana
  , target
  , targetInLocation
  -- * Uncategorized
  , activate
  , activatePlaneswalker
  , attackWith
  , combatDamage
  , copySpell
  , damage
  , destroy
  , discard
  , exert
  , exile
  , fight
  , modifyStrength
  , moveTo
  , sacrifice
  , transitionTo
  , transitionToForced
  , trigger
  , triggerMentor
  , with
  -- * Validations
  , validate
  , validateCanCastSorcery
  , validateLife
  , validatePhase
  , validateRemoved
  -- * State-based Actions
  -- | Fine-grained control over when state-based actions are run. By default,
  -- all actions will 'runStateBasedEffects' on completion, so most of the time
  -- you don't need to use these functions explicitly.
  , runStateBasedActions
  , withStateBasedActions
  -- * Low-level
  -- | These actions provide low-level control over the game. Where possible,
  -- try to use the more descriptive higher-level actions.
  , addMana
  , move
  , remove
  , spendMana
  , tap
  , untap
  ) where

import           Dovin.Attributes
import           Dovin.Helpers
import           Dovin.Prelude
import           Dovin.Types
import           Dovin.Builder
import Dovin.Monad

import qualified Data.HashMap.Strict as M
import Data.Maybe (listToMaybe)
import qualified Data.List

import Control.Arrow (second)
import Control.Monad.Reader (local)
import Control.Monad.State
import Control.Monad.Writer
import Control.Lens

action :: String -> GameMonad () -> GameMonad ()
action name m = m

-- | Add mana to actor's mana pool.
--
-- > addMana "2RG"
--
-- [Validates]:
--
--   * Mana specification is valid.
--
-- [Effects]:
--
--   * Mana pool is increased.
addMana :: ManaString -> GameMonad ()
addMana amount = do
  p <- view envActor

  modifying
    (manaPoolFor p)
    (parseMana amount <>)

-- | Casts a card from actor's hand. See 'castFromLocation' for specification.
--
-- > cast "R" "Shock"
--
-- [Validates]:
--
--   * Card exists in hand.
cast :: ManaPool -> CardName -> GameMonad ()
cast mana cn = do
  actor <- view envActor
  castFromLocation (actor, Hand) mana cn

-- | Move a card to the stack, spending the specified mana. If not tracking
-- mana, use the empty string to cast for no mana. Typically you will want to
-- 'resolve' after casting. For the common case of casting from hand, see
-- 'cast'. See 'spendMana' for additional mana validations and effects.
--
-- > castFromLocation "1B" "Oathsworn Vampire" >> resolveTop
--
-- [Validates]:
--
--   * Card exists in location.
--   * If not an instant or has flash, see 'validateCanCastSorcery` for extra
--     validations.
--
-- [Effects]:
--
--   * Card moved to top of stack.
--   * Counter 'storm' incremented if card has 'instant' or 'sorcery'
--     attribute.
castFromLocation :: CardLocation -> ManaPool -> CardName -> GameMonad ()
castFromLocation loc mana name = action "castFromLocation" $ do
  card <- requireCard name mempty

  validate (matchLocation loc) name
  unless
    (hasAttribute instant card || hasAttribute flash card)
    validateCanCastSorcery

  modifyCard (location . _2) (const Stack) name

  card <- requireCard name mempty

  spendMana mana

  when
    (hasAttribute sorcery card || hasAttribute instant card) $
    modifying
      (counters . at storm . non 0)
      (+ 1)

  modifying
    stack
    ((:) name)

-- | Remove a spell from the stack.
--
-- > counter "Shock"
--
-- [Validates]
--
--   * Card is on stack.
--   * Card is not a triggered or activated ability.
--
-- [Effects]
--
--   * Card is moved to graveyard. (See 'move' for alternate effects.)
counter :: CardName -> GameMonad ()
counter expectedName = do
  actor <- view envActor
  c <- requireCard expectedName $
            labelMatch "on stack" (matchLocation (actor, Stack))
         <> invert (          matchAttribute triggered
                    `matchOr` matchAttribute activated)

  moveTo Graveyard expectedName

  modifying
    stack
    (Data.List.delete expectedName)

-- | Cast a card from actor's graveyard, exiling it when it leaves
-- the stack. See 'castFromLocation' for further specification.
--
-- > flashback "R" "Shock"
--
-- Does not validate whether the card actually has a flashback cost. If
-- important, use a wrapper function in your solution:
--
-- @
-- flashbackSnapped mana castName = do
--   validate (matchAttribute "snapcastered") castName
--   flashback mana castName
-- @
--
-- [Validates]
--
--   * Card is in actor's graveyard.
--
-- [Effects]
--
--   * Card gains 'exileWhenLeaveStack'.
flashback :: ManaPool -> CardName -> GameMonad ()
flashback mana castName = do
  actor <- view envActor
  spendMana mana
  castFromLocation (actor, Graveyard) "" castName
  gainAttribute exileWhenLeaveStack castName

-- | Cast a card from active player's graveyard, discarding a card in
-- addition to its mana cost, exiling it when it leaves the stack. See
-- 'castFromLocation' for further specification.
--
-- > jumpstart "R" "Mountain" "Shock"
--
-- [Validates]
--
--   * Card is in actor's graveyard.
--   * Discard card is in actor's hand.
--
-- [Effects]
--
--   * Card gains 'exileWhenLeaveStack'.
--   * Discard card moved to graveyard.
jumpstart :: ManaPool -> CardName -> CardName -> GameMonad ()
jumpstart mana discardName castName = do
  actor <- view envActor
  spendMana mana
  discard discardName
  castFromLocation (actor, Graveyard) "" castName
  gainAttribute exileWhenLeaveStack castName

-- | Resolves a card on the stack.
--
-- > cast "R" "Shock" >> resolve "Shock"
--
-- [Validates]
--
--     * Stack is not empty.
--     * Card is on top of stack.
--
-- [Effects]
--
--     * See 'resolveTop'.
resolve :: CardName -> GameMonad ()
resolve expectedName = do
  s <- use stack

  case s of
    [] -> throwError $ "stack is empty, expecting " <> expectedName
    (name:ss) -> do
      unless (name == expectedName) $
        throwError $ "unexpected top of stack: expected "
                       <> expectedName
                       <> ", got "
                       <> name

      resolveTop

-- | Resolves the top card of the stack. Use this for simple cast-and-resolve
-- scenarios. For more complicated stack states, prefer 'resolve' with a named
-- spell to ensure the expected one is resolving.
--
-- [Validates]
--
--     * Stack is not empty.
--
-- [Effects]
--
--     * If spell, move card to graveyard of owner.
--     * If permanent, move card to play area of owner.
--     * If trigger, remove card.
--     * See 'move' for possible alternate effects, depending on card
--       attributes.
resolveTop :: GameMonad ()
resolveTop = action "resolveTop" $ do
  s <- use stack

  case s of
    []     -> throwError "stack is empty"
    (x:xs) -> do
      c <- requireCard x mempty

      if hasAttribute instant c || hasAttribute sorcery c then
        moveTo Graveyard x
      else if hasAttribute triggered c || hasAttribute activated c then
        remove x
      else
        moveTo Play x

      assign stack xs

-- | Resolves a trigger created by `triggerMentor`. Adds +1/+1 to target card
-- if still a valid mentor target.
--
-- > resolveMentor "Goblin 1" "Legion Warboss"
--
-- [Validates]
--
--   * Mentor trigger is top of stack.
--   * Target card is attacking.
--   * Target card has less power than source card.
--
-- [Effects]
--
--   * Target card gets +1/+1.
--   * Trigger is removed from top of stack.
resolveMentor targetName sourceName = do
  let triggerName = "Mentor " <> targetName <> " from " <> sourceName

  resolve triggerName

  source <- requireCard sourceName mempty
  _      <- requireCard targetName $
                 matchAttribute attacking
              <> matchLesserPower (view cardPower source)

  modifyStrength (1, 1) targetName

-- | Sacrifice a permanent.
--
-- > sacrifice "Soldier"
--
-- [Validates]
--
--   * Permanent controlled by current actor.
--   * Permanent is in play.
--
-- [Effects]
--
--   * Card is moved to graveyard. See 'move' for possible alternate effects.
sacrifice :: CardName -> GameMonad ()
sacrifice cn = do
  actor <- view envActor

  validate (matchController actor <> matchInPlay) cn

  moveTo Graveyard cn

-- | Splices a spell on to a previously cast arcane spell.
--
-- > splice "Goryo's Vengeance" "2RR" "Through the Breach"
--
-- [Validates]
--
--   * Target spell is arcane.
--   * Target spell is on stack.
--   * Spliced spell is in hand.
--   * See 'spendMana' for additional validations.
--
-- [Effects]
--
--   * See 'spendMana' for additional effects.
splice :: CardName -> ManaString -> CardName -> GameMonad ()
splice target cost name = action "splice" $ do
  actor <- view envActor

  validate (matchAttribute arcane) target
  validate (matchLocation (actor, Stack)) target
    `catchError` const (throwError $ target <> " not on stack")
  validate (matchLocation (actor, Hand)) name
    `catchError` const (throwError $ name <> " not in hand")
  spendMana cost

-- | Combination of 'tap' and 'addMana', see them for specification.
tapForMana :: ManaString -> CardName -> GameMonad ()
tapForMana amount name = do
  tap name
  addMana amount

-- | Transition to a new game phase or step.
--
-- > transitionTo DeclareAttackers
--
-- [Validates]
--
--   * The new phase would occur after the current phase in a normal turn.
--
-- [Effects]
--
--   * Empty the mana pool.
--   * Transition to new phase.
transitionTo :: Phase -> GameMonad ()
transitionTo newPhase = do
  actual <- use phase

  when (newPhase <= actual) $
    throwError $ "phase "
      <> show newPhase
      <> " does not occur after "
      <> show actual

  transitionToForced newPhase

-- | Equivalent to 'transitionTo' except it skips all validation. Useful when
-- an effect has modified the normal order of phases, such as adding an extra
-- combat step.
transitionToForced :: Phase -> GameMonad ()
transitionToForced newPhase = do
  assign manaPool mempty
  assign phase newPhase

-- | Triggers an effect of a permanent. Typically you will want to `resolve`
-- after triggering.
--
-- > trigger "Draw Card" "Dawn of Hope" >> resolveTop
--
-- [Validates]
--
--   * Card is in play or graveyard.
--   * Card is cotrolled by actor.
--
-- [Effects]
--
--   * A card with 'triggered' is added to stack.
trigger :: CardName -> CardName -> GameMonad ()
trigger triggerName sourceName = do
  actor <- view envActor
  card <-
    requireCard
      sourceName
      (  matchController actor
      <> labelMatch "in play or graveyard" (
           matchLocation (actor, Play)
           `matchOr`
           matchLocation (actor, Graveyard)
         )
      )

  withLocation Stack $ withAttribute triggered $ addCard triggerName

  modifying
    stack
    ((:) triggerName)

-- | Triggers a mentor effect from an attacking creature, targeting another
-- attacking creature with lesser power. Typically you will want to
-- `resolveMentor` after triggering.
--
-- > triggerMentor "Goblin 1" "Legion Warboss"
--
-- [Validates]
--
--   * Source card has attacking and mentor attributes.
--   * Target card is attacking.
--   * Target card has less power than source card.
--
-- [Effects]
--
--   * A triggered card is placed on the stack.
triggerMentor :: CardName -> CardName -> GameMonad ()
triggerMentor targetName sourceName = do
  source <- requireCard sourceName $ matchAttributes [attacking, mentor]
  _      <- requireCard targetName $
                 matchAttribute attacking
              <> matchLesserPower (view cardPower source)

  trigger ("Mentor " <> targetName <> " from " <> sourceName) sourceName


-- | Helper function to provide a scoped let.
--
-- > with "Angel" $ \cn -> target cn >> destroy cn
with :: CardName -> (CardName -> GameMonad ()) -> GameMonad ()
with x f = f x

-- | Move a card from one location to another.
--
-- > move (Opponent, Play) (Active, Play) "Angel"
--
-- [Validates]:
--
--     * Card exists in source location.
--     * Destination is not stack (use a 'cast' variant instead).
--     * Destination does not match source.
--     * If card has 'token' attribute, source is in play. (Removing token once
--       they have left play is handled by 'runStateBasedActions'.)
--     * If card has 'copy' attribute, source is the stack. (Removing token
--       once they have left play is handled by 'runStateBasedActions'.)
--
-- [Effects]:
--
--     * Card moved to destination location.
--     * If card is leaving play, remove all damage, counters, and gained
--       attributes.
--     * If card has 'exileWhenLeaveStack' attribute, move to exile and remove
--       'exileWhenLeaveStack' instead.
--     * If card has 'undying', is moving from play to graveyard, and has no
--       +1\/+1 counters, add a +1\/+1 counter instead. (Note: undying should
--       move card to graveyard then back to play for owner, but since neither
--       triggers nor owner tracking are implemented, this simplification is
--       valid.)
--     * If card is entering play or changing controller, add 'summoned'
--       attribute.
move :: CardLocation -> CardLocation -> CardName -> GameMonad ()
move from to name = action "move" $ do
  c <- requireCard name $ matchLocation from

  when (from == to) $
    throwError "cannot move to same location"

  when (snd to == Stack) $
    throwError "cannot move directly to stack"

  when (hasAttribute token c && snd from /= Play) $
    throwError "cannot move token from non-play location"

  when (hasAttribute copy c && snd from /= Stack) $
    throwError "cannot move copy from non-stack location"

  when (snd from == Stack) $
    modifying stack (filter (/= name))

  when (snd to == Play) $
    gainAttribute summoned name

  when (snd from == Play && snd to /= Play) $ do
    modifyCard cardPlusOneCounters (const 0) name
    modifyCard cardDamage (const 0) name
    modifyCard cardAttributes (const $ view cardDefaultAttributes c) name
    modifyCard cardStrengthModifier (const mempty) name

  -- These conditionals are acting on the card state _before_ any of the above
  -- changes were applied.
  if hasAttribute exileWhenLeaveStack c then
    do
      loseAttribute exileWhenLeaveStack name
      moveTo Exile name
  else if snd from == Play && snd to == Graveyard && view cardPlusOneCounters c == 0 && hasAttribute undying c then
    modifyCardDeprecated name cardPlusOneCounters (+ 1)
  else
    modifyCardDeprecated name location (const to)

-- | Target a permanent.
--
-- [Validates]
--
--   * Card is in play.
--   * If card belongs to opponent, does not have 'hexproof'.
target :: CardName -> GameMonad ()
target name = do
  actor <- view envActor
  card  <- requireCard name matchInPlay

  let controller = view (cardLocation . _1) card

  unless (actor == controller) $
    validate (missingAttribute hexproof) name

-- | Target a card in a non-play location.
--
-- [Validates]
--
--   * Card is in zone.
targetInLocation :: CardLocation -> CardName -> GameMonad ()
targetInLocation zone = validate (matchLocation zone)

-- | Activate an ability of a permanent. See 'spendMana' for additional mana
-- validations and effects. Typically you will want to `resolve` after
-- activating.
--
-- > activate "Create Soldier" "3W" "Dawn of Hope" >> resolveTop
--
-- [Validates]
--
--   * Card is in play or graveyard.
--   * Card is controlled by actor.
--
-- [Effects]
--
--   * A card with 'activated' is added to stack.
activate :: CardName -> ManaPool -> CardName -> GameMonad ()
activate stackName mana targetName = do
  actor <- view envActor
  card <-
    requireCard
      targetName
      (  matchController actor
      <> labelMatch "in play or graveyard" (
           matchLocation (actor, Play)
           `matchOr`
           matchLocation (actor, Graveyard)
         )
      )

  spendMana mana

  withLocation Stack $ withAttribute activated $ addCard stackName

  modifying
    stack
    ((:) stackName)

-- | Activate a loyalty ability of a planeswalker. Typically you will want to
-- `resolve` after activating.
--
-- > activatePlaneswalker2 "Get a card" (-1) "Karn, Scion of Urza" >> resolveTop
--
-- [Validates]
--
--   * Card is in play.
--   * Card has enough loyalty.
--
-- [Effects]
--
--   * Card loyalty is adjusted.
--
-- See `activate` for additional validations and effects.
activatePlaneswalker :: CardName -> Int -> CardName -> GameMonad ()
activatePlaneswalker stackName loyalty targetName = do
  c <- requireCard targetName matchInPlay

  if view cardLoyalty c + loyalty < 0 then
    throwError $ targetName <> " does not have enough loyalty"
  else
    do
      modifyCard cardLoyalty (+ loyalty) targetName
      activate stackName "" targetName

-- | Start an attack with the given creatures.
--
-- > attackWith ["Fanatical Firebrand"]
--
-- [Validates]
--
--   * Cards are in play.
--   * Cards have 'creature' attribute.
--   * Cards either have 'haste' or are missing 'summoned'.
--   * Cards do not have 'defender'.
--
-- [Effects]
--
--   * Cards become tapped, unless they have 'vigilance'.
--   * Cards gain 'attacking' attribute.
--   * Transitions to 'DeclareAttackers' step.
attackWith :: [CardName] -> GameMonad ()
attackWith cs = do
  transitionTo DeclareAttackers

  forM_ cs $ \cn -> do
    c <- requireCard cn
           (matchInPlay
             <> matchAttribute "creature"
             <> missingAttribute "defender"
             <> labelMatch "does not have summoning sickness" (
                    matchAttribute haste
                    `matchOr`
                    missingAttribute summoned
                ))
    forCards
      (matchName cn <> missingAttribute vigilance)
      tap
    gainAttribute attacking cn

-- | Apply combat damage between an attacker and blockers, using a simple
-- damage assignment algorithm. For more complex assignments, use 'damage'
-- directly.
--
-- > combatDamage ["Spirit 1", "Spirit 2"] "Angel"
--
-- See 'damage' for other validations and effects.
--
-- [Validates]
--
--   * Attacker has attribute 'attacking'.
--   * Attacker and blockers are in play.
--   * Attacker controlled by current actor.
--   * Blockers have attribute 'creature'.
--
-- [Effects]
--
--   * Damage is dealt to blockers in order given, with the final blocker
--     receiving any left over damage.
--   * If no blockers, damage is dealt to opposing player.
--   * If attacker has 'trample', any remaining damage is dealt to opposing
--     player.
combatDamage :: [CardName] -> CardName -> GameMonad ()
combatDamage blockerNames attackerName = do
  actor <- view envActor
  attacker <- requireCard attackerName
    $ matchInPlay <> matchAttribute attacking <> matchController actor

  blockers <-
    mapM
      (\cn -> requireCard cn $ matchInPlay <> matchAttribute creature)
      blockerNames

  let power = view cardPower attacker

  rem <- foldM (folder attacker) power blockers

  if hasAttribute trample attacker || null blockers then
    -- Assign leftover damage to opponent
    damage (const rem) (targetPlayer . opposing $ actor) attackerName
  else
    -- Assign any leftover damage to final blocker
    maybe
      (return ())
      (\x -> damage
               (const rem)
               (targetCard . view cardName $ x)
               attackerName
      )
      (listToMaybe . reverse $ blockers)

  where
    folder attacker rem blocker = do
      let blockerName      = view cardName blocker
      let blockerPower     = view cardPower blocker
      let blockerToughness = view cardToughness blocker
      let attackPower = minimum [blockerToughness, rem]

      damage
        (const attackPower)
        (targetCard blockerName)
        attackerName

      damage
        (const blockerPower)
        (targetCard attackerName)
        blockerName

      return $ rem - attackPower

-- | Copy a spell on the stack, adding it to top of stack.
--
-- > copySpell "Snap Copy" "Snap"
--
-- [Validates]
--
--   * Card is on stack.
--
-- [Effects]
--
--   * New card is on top of stack.
copySpell newName targetName = do
  card <- requireCard targetName (labelMatch "on stack" $
                      matchLocation (Active, Stack)
            `matchOr` matchLocation (Opponent, Stack)
          )

  let newCard = setAttribute copy . set cardName newName $ card

  modifying
    cards
    (M.insert newName $ BaseCard newCard)

  modifying
    stack
    ((:) newName)

-- | Applies damage from a source to a target.
--
-- > damage (const 2) (targetPlayer Opponent) "Shock"
--
-- [Validates]
--
--   * Source exists.
--   * Damage is not less than zero.
--   * If targeting a card, target is in play and is either a creature or a
--     planeswalker.
--
-- [Effects]
--
--   * Adds damage to the target.
--   * If target is a planeswalker, remove loyalty counters instead.
--   * If source has 'deathtouch' and target is a creature and damage is
--     non-zero, add 'deathtouched'
--     attribute to target.
--   * If source has 'lifelink', controller of source gains life equal to
--     damage dealt.
--   * Note 'runStateBasedActions' handles actual destruction (if applicable)
--     of creatures and planeswalkers.
damage ::
     (Card -> Int) -- ^ A function that returns the amount of damage to apply,
                   --   given the source card.
  -> Target        -- ^ Target to apply damage to
  -> CardName      -- ^ Source card
  -> GameMonad ()
damage f t source = action "damage" $ do
  c <- requireCard source mempty

  let dmg = f c

  when (dmg < 0) $
    throwError $ "damage must be positive, was " <> show dmg

  damage' dmg t c

  when (hasAttribute lifelink c) $
    modifying (life . at (fst . view location $ c) . non 0) (+ dmg)

  where
    damage' dmg (TargetPlayer t) c =
      modifying
        (life . at t . non 0)
        (\x -> x - dmg)

    damage' dmg (TargetCard tn) c = do
      t <- requireCard tn $ matchInPlay <>
             (matchAttribute creature `matchOr` matchAttribute planeswalker)

      target tn
      when (hasAttribute creature t) $ do
        modifyCardDeprecated tn cardDamage (+ dmg)

        when (dmg > 0 && hasAttribute deathtouch c) $
          gainAttribute deathtouched tn

      when (hasAttribute planeswalker t) $
        modifyCardDeprecated tn cardLoyalty (\x -> x - dmg)


-- | Destroy a permanent.
--
-- [Validates]
--
--   * Card is in play.
--   * Card is not 'indestructible'
--
-- [Effects]
--
--   * Card is moved to graveyard. See 'move' for possible alternate effects.
destroy :: CardName -> GameMonad ()
destroy targetName = do
  validate (matchInPlay <> missingAttribute indestructible) targetName
  moveTo Graveyard targetName

-- | Discard a card from the active player's hand.
--
-- > discard "Mountain"
--
-- [Validates]
--
--   * Card exists in active player's hand.
--
-- [Effects]
--
--   * Card moved to graveyard.
discard :: CardName -> GameMonad ()
discard cn = do
  actor <- view envActor
  move (actor, Hand) (actor, Graveyard) cn

-- | Exert a card. Works best when card has an associated effect that applies
-- when 'exerted' attribute is present.
--
-- > withAttributes [flying]
-- >   $ withEffect
-- >       (matchInPlay <> matchAttribute exerted)
-- >       (    matchLocation . view cardLocation
-- >         <> const (matchAttribute creature)
-- >       )
-- >       (pure . over cardStrengthModifier (mkStrength (1, 1) <>))
-- >   $ addCreature (2, 2) "Tah-Crop Elite"
-- > attackWith ["Tah-Crop Elite"]
-- > exert "Tah-Crop Elite"
--
-- [Validates]
--
--   * Card has 'tapped' attribute.
--
-- [Effects]
--
--   * Card gains 'exerted' attribute.
exert :: CardName -> GameMonad ()
exert cn = do
  validate (matchAttribute tapped) cn
  gainAttribute exerted cn

-- | Move a card to the Exile zone.
--
-- > exile "Bridge from Below"
--
-- See `moveTo` for validations and effects.
exile :: CardName -> GameMonad ()
exile = moveTo Exile

-- | Have one card fight another (each deals damage to the other).
--
-- [Validates]
--
--   * Card is in play.
--   * Card is a creature.
--
-- [Effects]
--
--   * Each card has damage dealt to it equal to the other's power. A creature
--     fighting itself will take twice its power in damage.
--   * Note 'runStateBasedActions' handles actual destruction (if applicable)
--     of creatures.
fight :: CardName -> CardName -> GameMonad ()
fight x y = do
  validate (matchInPlay <> matchAttribute creature) x
  validate (matchInPlay <> matchAttribute creature) y

  fight' x y
  fight' y x

  where
    fight' src dst = damage (view cardPower) (targetCard dst) src

-- | Modify the strength of a card in play. It will be reset to base when the
-- card leaves play.
--
-- > modifyStrength (-2, -2) "Soldier"
--
-- [Validates]
--
--   * Card is in play.
--   * Card is a creature.
--
-- [Effects]
--
--   * Changes the strength modifier for the card.
modifyStrength :: (Int, Int) -> CardName -> GameMonad ()
modifyStrength strength cn = do
  _ <- requireCard cn (matchInPlay <> matchAttribute creature)

  modifyCard cardStrengthModifier (mkStrength strength <>) cn

-- | Move card to location with same controller.
--
-- > moveTo Graveyard "Forest"
--
-- [Validates]:
--
--     * Card exists.
--
-- [Effects]:
--
--     * Card is moved to location.
--     * See 'move' for possible alternate effects, depending on card
moveTo :: Location -> CardName -> GameMonad ()
moveTo dest cn = do
  c <- requireCard cn mempty

  let location = view cardLocation c

  move location (second (const dest) location) cn

remove :: CardName -> GameMonad ()
remove cn = do
  modifying cards (M.delete cn)
  modifying stack (filter (/= cn))

-- | Remove mana from the pool. Colored mana will be removed first, then extra
-- mana of any type will be removed to match the colorless required.
--
-- > spendMana "2RG"
--
-- [Validates]:
--
--     * Mana specification is valid.
--     * Sufficient mana exists in pool.
--
-- [Effects]:
--
--     * Mana pool is reduced.
spendMana :: ManaString -> GameMonad ()
spendMana amount =
  forM_ (parseMana amount) $ \mana -> do
    actor <- view envActor
    pool <- use $ manaPoolFor actor
    if mana == 'X' && (not . null $ pool) || mana `elem` pool then
      modifying
        (manaPoolFor actor)
        (deleteFirst (if mana == 'X' then const True else (==) mana))
    else
      throwError $ "Mana pool (" <> pool <> ") does not contain (" <> [mana] <> ")"
  where

    -- https://stackoverflow.com/questions/14688716/removing-the-first-instance-of-x-from-a-list
    deleteFirst _ [] = []
    deleteFirst f (b:bc) | f b    = bc
                         | otherwise = b : deleteFirst f bc


-- | Taps a card.
--
-- [Validates]:
--
--   * Card is in play.
--   * Card is not tapped.
--   * If creature, is not summoned or has haste.
--
-- [Effects]:
--
--   * Card gains tapped attribute.
tap :: CardName -> GameMonad ()
tap name = do
  c <- requireCard name (matchInPlay <> missingAttribute tapped)

  when (applyMatcher (matchAttribute creature) c) $
    validate (labelMatch "does not have summoning sickness" (
                    matchAttribute haste
                    `matchOr`
                    missingAttribute summoned
             )) name


  gainAttribute tapped name

-- | Untaps a card.
--
-- [Validates]:
--
--   * Card is in play.
--   * Card is tapped.
--
-- [Effects]:
--
--   * Card loses tapped attribute.
untap :: CardName -> GameMonad ()
untap name = do
  validate (matchInPlay <> matchAttribute tapped) name

  loseAttribute tapped name

-- | Validate that a card matches a matcher.
--
-- > validate (matchAttribute "pirate") "Angrath's Marauders"
--
-- [Validates]
--
--     * Card matches matcher.
validate :: CardMatcher -> CardName -> GameMonad ()
validate reqs targetName = do
  _ <- requireCard targetName reqs
  return ()

-- | Validates that a card is no longer present in the game. Particularly
-- helpful for checking destruction of tokens.
--
-- > validateRemoved "Angel"
--
-- [Validates]
--
--     * Name does not refer to a card.
validateRemoved :: CardName -> GameMonad ()
validateRemoved targetName = do
  card <- use $ cards . at targetName
  case card of
    Nothing -> return ()
    Just _ -> throwError $ "Card should be removed: " <> targetName

-- | Validates that the game is in a particular phase.
--
-- > validatePhase BeginCombat
--
-- [Validates]
--
--     * Game is in the given phase.
validatePhase :: Phase -> GameMonad ()
validatePhase expected = do
  actual <- use phase

  when (actual /= expected) $
    throwError $ "phase was "
      <> show actual
      <> ", expected "
      <> show expected

-- | Validates that a sorcery is able to be cast.
--
-- [Validates]
--
--     * Stack is empty.
--     * In a main phase.
validateCanCastSorcery :: GameMonad ()
validateCanCastSorcery = do
  validatePhase FirstMain
    `catchError` const (validatePhase SecondMain)
    `catchError` const (throwError "not in a main phase")

  s <- use stack

  unless (null s) $ throwError "stack is not empty"

-- | Validates a player has a specific life total.
--
-- > validateLife 0 Opponent
--
-- [Validates]
--
--     * Player life equals amount.
validateLife :: Int -> Player -> GameMonad ()
validateLife n player = do
  current <- use (life . at player . non 0)

  when (current /= n) $
    throwError $ show player
      <> " life was "
      <> show current
      <> ", expected "
      <> show n

-- | Pause running of state-based actions for the duration of the action,
-- running them at the end.
withStateBasedActions :: GameMonad a -> GameMonad a
withStateBasedActions m = do
  x <- local (set envSBAEnabled False) m
  runStateBasedActions
  return x

-- | Run state-based actions. These include:
--
--     * If a creature does not have 'indestructible', and has damage exceeding
--       toughess or 'deathtouched' attribute, destroy it.
--     * If a card is a 'token' and is not in play, remove it.
--     * If a card is a 'copy' and is not on the stack, remove it.
--
-- These are run implicitly at the end of each 'step', so it's not usually
-- needed to call this explicitly. Even then, using 'withStateBasedActions' is
-- usually preferred.
--
-- Running state-based actions can in turn trigger more state-based actions.
-- This method loops until no more are generated, which has the potential for
-- non-termination for pathological game states.
runStateBasedActions :: GameMonad ()
runStateBasedActions = do
  enabled <- view envSBAEnabled
  when enabled $
    local (set envSBAEnabled False) runStateBasedActions'

  where
    sbaCounter :: Control.Lens.Lens' Board Int
    sbaCounter = counters . at "sba-counter" . non 0

    runStateBasedActions' = do
      assign sbaCounter 0

      let incrementCounter = modifying sbaCounter (+ 1)

      forCards mempty $ \cn -> do
        c <- requireCard cn mempty

        when (applyMatcher (matchInPlay <> matchAttribute creature) c) $ do
          let dmg = view cardDamage c
          let toughness = view cardToughness c

          unless (hasAttribute indestructible c) $
            when (dmg >= toughness || hasAttribute deathtouched c) $
              moveTo Graveyard cn >> incrementCounter

        when (applyMatcher (invert matchInPlay) c) $
          when (hasAttribute token c) $
            remove cn >> incrementCounter

        let p1 = view cardPlusOneCounters c
        let m1 = view cardMinusOneCounters c
        let p1' = maximum [0, p1 - m1]
        let m1' = maximum [0, m1 - p1]

        when (p1 /= p1' || m1 /= m1') $ do
          modifyCard cardPlusOneCounters (const p1') cn
          modifyCard cardMinusOneCounters (const m1') cn
          incrementCounter

        let matchStack =
                       matchLocation (Active, Stack)
             `matchOr` matchLocation (Opponent, Stack)

        when (applyMatcher (invert matchStack) c) $
          when (hasAttribute copy c) $
            remove cn >> incrementCounter

      n <- use sbaCounter

      when (n > 0) runStateBasedActions'

-- | Define a high-level step in the proof. A proof typically consists on
-- multiple steps. Each step is a human-readable description, then a definition
-- of that step using actions. If a step fails, no subsequent steps will be
-- run.  'runStateBasedActions' is implicitly called at the end of each step.
-- Nested 'step' invocations execute the nested action but have no other
-- effects - generally they should be avoided.
step :: String -> GameMonad a -> GameMonad a
step desc m = withStateBasedActions $ do
  b <- get
  let (e, b', _) = runMonad b m
  let b'' = over currentStep incrementStep b'

  tell [mkStep (view currentStep b'') desc b'']
  put b''

  case e of
    Left x -> throwError x
    Right y -> return y

-- | Branch off a labeled alternate line. Steps inside the fork will be
-- reported at the end of the main line output.
fork :: String -> GameMonad () -> GameMonad ()
fork label m = do
  b <- get
  modifying
    (currentStep . _1)
    (f label)
  m
  put b

  where
    f label Nothing = Just label
    f label (Just existing) = Just $ existing <> " - " <> label
