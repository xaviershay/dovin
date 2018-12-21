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
  -- * Casting
    cast
  , castFromLocation
  , tapForMana
  -- * Uncategorized
  , transitionTo
  , transitionToForced
  -- * Validations
  , validate
  , validateCanCastSorcery
  , validateLife
  , validatePhase
  , validateRemoved
  --, resolve
  -- * Low-level
  -- | These actions provide low-level control over the game. Where possible,
  -- try to use the more descriptive higher-level actions.
  , addMana
  , move
  , spendMana
  , tap
  ) where

import           Dovin.Attributes
import           Dovin.Helpers
import           Dovin.Prelude
import           Dovin.Types

import qualified Data.Set as S

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
castFromLocation loc mana name = do
  card <- requireCard name mempty

  unless (hasAttribute instant card) validateCanCastSorcery

  move loc (Active, Stack) name

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
--
-- [Effects]:
--
--     * Card moved to destination location.
move :: CardLocation -> CardLocation -> CardName -> GameMonad ()
move from to name = do
  c <- requireCard name $ matchLocation from

  modifyCard name location (const to)

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
  card <- requireCard name
    ((matchLocation (Opponent, Play) `matchOr` matchLocation (Active, Play))
    <> missingAttribute tapped)

  modifyCard name cardAttributes (S.insert tapped)

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
