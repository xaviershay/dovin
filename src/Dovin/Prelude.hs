-- | Re-exports common internal imports. Not recommended for external use.
module Dovin.Prelude
  ( module Control.Lens
  , module Control.Monad
  , module Control.Monad.Except
  , dup
  )
  where

import Control.Lens (assign, at, modifying, non, over, set, use, view)
import Control.Monad (foldM, forM_, unless, when)
import Control.Monad.Except (catchError, throwError)

dup :: a -> (a, a)
dup x = (x, x)
