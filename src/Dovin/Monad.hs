module Dovin.Monad where

import           Control.Monad.Identity
import           Control.Monad.Except
import           Control.Monad.Writer
import           Control.Monad.Reader
import           Control.Monad.State hiding (state)

import Dovin.Types

runMonad :: Board -> GameMonad a -> (Either String a, Board, [Step])
runMonad state m =
  let ((e, b), log) = runIdentity $
                        runWriterT (runStateT (runReaderT (runExceptT m) emptyEnv) state) in

  (e, b, log)

execMonad :: Board -> GameMonad a -> Either String a
execMonad state m =
  let result = fst $ runIdentity (runWriterT (evalStateT (runReaderT (runExceptT m) emptyEnv) state)) in

  result
