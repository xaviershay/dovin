module Dovin.Monad where

import           Control.Monad.Identity
import           Control.Monad.Except
import           Control.Monad.Writer
import           Control.Monad.State hiding (state)

import Dovin.Types

runMonad :: Board -> GameMonad () -> (Either String (), Board, [(String, Board)])
runMonad state m =
  let ((e, b), log) = runIdentity $
                        runWriterT (runStateT (runExceptT m) state) in

  (e, b, log)

execMonad :: Board -> GameMonad a -> Either String a
execMonad state m =
  let result = fst $ runIdentity (runWriterT (evalStateT (runExceptT m) state)) in

  result
