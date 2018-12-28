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
  -- * Casting
  , cast
  , castFromLocation
  , resolve
  , resolveTop
  , splice
  , tapForMana
  -- * Uncategorized
  , attackWith
  , combatDamage
  , damage
  , exert
  , moveTo
  , transitionTo
  , transitionToForced
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
import Dovin.Monad

import qualified Data.HashMap.Strict as M
import Data.Maybe (listToMaybe)

import Control.Arrow (second)
import Control.Monad.Reader (local)
import Control.Monad.State
import Control.Monad.Writer
import Control.Lens

action :: String -> GameMonad () -> GameMonad ()
action name m = m


-- | Add mana to your mana pool.
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
addMana amount =
  modifying
    manaPool
    (parseMana amount <>)

-- | Casts a card from hand. See 'castFromLocation' for specification.
cast = castFromLocation (Active, Hand)

-- | Move a card to the stack, spending the specified mana. If not tracking
-- mana, use the empty string to cast for no mana. Typically you will want to
-- 'resolve' after casting. For the common case of casting from hand, see
-- 'cast'.
--
-- > castFromLocation "1B" "Oathsworn Vampire"
--
--   [Validates]:
--
--     * Card exists in location.
--     * Mana is available.
--     * If not an instant, see 'validateCanCastSorcery` for extra validations.
--
--   [Effects]:
--
--     * Card moved to top of stack.
--     * Mana removed from pool.
--     * Counter 'storm' incremented if card has 'instant' or 'sorcery'
--       attribute.
castFromLocation :: CardLocation -> ManaPool -> CardName -> GameMonad ()
castFromLocation loc mana name = action "castFromLocation" $ do
  card <- requireCard name mempty

  validate name $ matchLocation loc
  unless (hasAttribute instant card) validateCanCastSorcery

  modifyCard name location $ const (view cardOwner card, Stack)

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
--     * See 'move' for possible alternate effects, depending on card
--       attributes.
resolveTop :: GameMonad ()
resolveTop = action "resolveTop" $ do
  s <- use stack

  case s of
    []     -> throwError $ "stack is empty"
    (x:xs) -> do
      c <- requireCard x mempty

      if hasAttribute instant c || hasAttribute sorcery c then
        moveTo Graveyard x
      else
        moveTo Play x

      assign stack xs

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
  validate target $ matchAttribute arcane
  validate target (matchLocation (Active, Stack))
    `catchError` const (throwError $ target <> " not on stack")
  validate name (matchLocation (Active, Hand))
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
    modifyCard name cardPlusOneCounters (const 0)
    modifyCard name cardDamage (const 0)
    modifyCard name cardAttributes (const $ view cardDefaultAttributes c)

  -- These conditionals are acting on the card state _before_ any of the above
  -- changes were applied.
  if hasAttribute exileWhenLeaveStack c then
    do
      loseAttribute exileWhenLeaveStack name
      moveTo Exile name
  else if snd from == Play && snd to == Graveyard && view cardPlusOneCounters c == 0 && hasAttribute undying c then
    modifyCard name cardPlusOneCounters (+ 1)
  else
    modifyCard name location (const to)


-- | Start an attack with the given creatures.
--
-- > attackWith ["Fanatical Firebrand"]
--
-- [Validates]
--
--   * Cards are in play.
--   * Cards have 'creature' attribute.
--   * Cards either have 'haste' or are missing 'summoned'.
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
             <> ( labelMatch ("does not have summoning sickness") $
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
--   * Blockers have attribute 'creature'.
--
-- [Effects]
--
--   * Damage is dealt to blockers in order given, with the final blocker
--     receiving any left over damage.
--   * If no blockers, damage is dealt to opponent.
--   * If attacker has 'trample', any remaining damage is dealt to opponent.
combatDamage :: [CardName] -> CardName -> GameMonad ()
combatDamage blockerNames attackerName = do
  attacker <-
    requireCard attackerName $ matchInPlay <> matchAttribute attacking
  blockers <-
    mapM
      (\cn -> requireCard cn $ matchInPlay <> matchAttribute creature)
      blockerNames

  let power = view cardPower attacker

  rem <- foldM (folder attacker) power blockers

  if hasAttribute trample attacker || null blockers then
    -- Assign leftover damage to opponent
    damage (const rem) (targetPlayer Opponent) attackerName
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
                   -- given the source card.
  -> Target
  -> CardName
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
    damage' dmg (TargetPlayer t) c = do
      modifying
        (life . at t . non 0)
        (\x -> x - dmg)

    damage' dmg (TargetCard tn) c = do
      t <- requireCard tn $ matchInPlay <>
             (matchAttribute creature `matchOr` matchAttribute planeswalker)

      when (hasAttribute creature t) $ do
        modifyCard tn cardDamage (+ dmg)

        when (dmg > 0 && hasAttribute deathtouch c) $
          gainAttribute deathtouched tn

      when (hasAttribute planeswalker t) $ do
        modifyCard tn cardLoyalty (\x -> x - dmg)

destroy :: CardName -> GameMonad ()
destroy targetName = do
  validate targetName $ matchInPlay <> missingAttribute indestructible

  moveTo Graveyard targetName

-- | Exert a card. Works best when card has an associated effect that applies
-- when 'exerted' attribute is present.
--
-- > withAttributes [flying]
-- >   $ withEffect
-- >       (matchInPlay <> matchAttribute exerted)
-- >       (    matchLocation . view cardLocation
-- >         <> const (matchAttribute creature)
-- >       )
-- >       (pure . over cardStrength (mkStrength (1, 1) <>))
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
  validate cn $ matchAttribute tapped
  gainAttribute exerted cn

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
    pool <- use manaPool
    if mana == 'X' && (not . null $ pool) || mana `elem` pool then
      modifying
        manaPool
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
--
-- [Effects]:
--
--   * Card gains tapped attribute.
tap :: CardName -> GameMonad ()
tap name = do
  validate name $ matchInPlay <> missingAttribute tapped

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
  validate name $ matchInPlay <> matchAttribute tapped

  loseAttribute tapped name


-- | Validate that a card matches a matcher.
--
-- > validate "Angrath's Marauders" $ matchAttribute "pirate"
--
-- [Validates]
--
--     * Card matches matcher.
validate :: CardName -> CardMatcher -> GameMonad ()
validate targetName reqs = do
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
-- > validateLife Opponent 0
--
-- [Validates]
--
--     * Player life equals amount.
validateLife :: Player -> Int -> GameMonad ()
validateLife player n = do
  current <- use (life . at player . non 0)

  when (current /= n) $
    throwError $ show player
      <> " life was "
      <> show current
      <> ", expected "
      <> show n

-- | Pause running of state-based actions for the duration of the action,
-- running them at the end.
withStateBasedActions :: GameMonad () -> GameMonad ()
withStateBasedActions m = do
  local (set envSBAEnabled False) m
  runStateBasedActions

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
  when enabled
    $ local (set envSBAEnabled False)
    $ runStateBasedActions'

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

        let matchStack =
                matchLocation (Active, Stack)
             <> matchLocation (Opponent, Stack)

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
step :: String -> GameMonad () -> GameMonad ()
step desc m = withStateBasedActions $ do
  b <- get
  let (e, b', _) = runMonad b m

  tell [(desc, b')]
  put b'

  case e of
    Left x -> throwError x
    Right _ -> return ()
