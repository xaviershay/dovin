{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE FlexibleInstances #-}

module Tape where

import Control.Arrow
import Control.Applicative

import Data.Stream ( Stream(..) )
import qualified Data.Stream as S

import Prelude hiding ( iterate , take )

-- | A @Tape@ is like a Turing-machine tape: infinite in both directions, with a focus in the middle.
data Tape a = Tape { viewL :: Stream a -- ^ the side of the @Tape@ left of @focus@
                   , focus :: a        -- ^ the focused element
                   , viewR :: Stream a -- ^ the side of the @Tape@ right of @focus@
                   } deriving ( Functor )

-- | Produce a @Tape@ from a seed value, ala unfoldr for lists, or unfold for @Stream@s.
unfold :: (c -> (a,c)) -- ^ leftwards unfolding function
       -> (c -> a)     -- ^ function giving the focus value from the seed
       -> (c -> (a,c)) -- ^ rightwards unfolding function
       -> c            -- ^ seed value
       -> Tape a

unfold prev center next =
   Tape <$> S.unfold prev <*> center <*> S.unfold next
-- | Produce a @Tape@ consisting of the infinite iteration of two functions to a starting focus value,
--   ala iterate for lists or @Stream@s.
iterate :: (a -> a) -- ^ leftwards iteration function
        -> (a -> a) -- ^ rightwards iteration function
        -> a        -- ^ focus value
        -> Tape a
iterate prev next =
   unfold (dup . prev) id (dup . next)
   where dup a = (a,a)

-- | Given an enumerable type, produce the @Tape@ where the left side is the sequence of predecessors,
--   and the right side is the sequence of successors.
enumerate :: (Enum a) => a -> Tape a
enumerate = iterate pred succ


-- | The functions @moveR@ and @moveL@ move the focus on the tape right and left, respectively.
moveL, moveR :: Tape a -> Tape a
moveL (Tape (Cons l ls) c rs) = Tape ls l (Cons c rs)
moveR (Tape ls c (Cons r rs)) = Tape (Cons c ls) r rs
