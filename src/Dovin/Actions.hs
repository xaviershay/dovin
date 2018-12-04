{-# LANGAUGE FlexibleContexts #-}
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
  --, resolve
  -- * Low-level
  -- | These actions provide low-level control over the game, and will rarely
  -- be used directly. Instead, try to use the more descriptive higher-level
  -- actions.
  , move
  , spendMana
  ) where

import           Dovin.Helpers
import           Dovin.Types

import           Control.Lens         (assign, at, modifying, non, use, _Just)
import           Control.Monad        (forM_, when)
import           Control.Monad.Except (throwError)

-- | Casts a card from hand. See 'castFromLocation' for specification.
cast mana name = do
  castFromLocation (Active, Hand) mana name

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
--
--   [Effects]:
--
--     * Card moved to top of stack.
--     * Mana removed from pool.
--     * Counter @storm@ incremented if card has @instant@ or @sorcery@ attribute.
castFromLocation :: CardLocation -> ManaPool -> CardName -> GameMonad ()
castFromLocation loc mana name = do
  move loc (Active, Stack) name

  card <- requireCard name mempty

  spendMana mana

  when
    (hasAttribute "sorcery" card || hasAttribute "instant" card) $
    modifying
      (counters . at "storm" . non 0)
      (+ 1)

  modifying
    stack
    ((:) name)

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

  assign
    (cards . at name . _Just . location)
    to

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
--     * Mana pool is reduced by the specified amount.
spendMana :: ManaString -> GameMonad ()
spendMana amount = do
  forM_ (parseMana amount) $ \mana -> do
    pool <- use manaPool
    if mana == 'X' && (not . null $ pool) || mana `elem` pool then
      modifying
        manaPool
        (deleteFirst (if mana == 'X' then (const True) else (==) mana))
    else
      throwError $ "Mana pool (" <> pool <> ") does not contain (" <> [mana] <> ")"
  where

    -- https://stackoverflow.com/questions/14688716/removing-the-first-instance-of-x-from-a-list
    deleteFirst _ [] = []
    deleteFirst f (b:bc) | f b    = bc
                         | otherwise = b : deleteFirst f bc

