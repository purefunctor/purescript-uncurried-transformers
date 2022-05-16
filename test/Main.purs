module Test.Main where

import Prelude

import Cps.StateT as Cps
import Control.Monad.Rec.Class (class MonadRec, Step(..), tailRecM)
import Control.Monad.State.Trans (class MonadState, get, put)
import Control.Monad.State.Trans as Trs
import Effect (Effect)
import Effect.Class (class MonadEffect)
import Effect.Class.Console (log, logShow)
import Effect.Exception (catchException)

limit :: Int
limit = 50_000

program :: forall m. MonadEffect m => MonadState Int m => m Unit
program = go
  where
  go = do
    current <- get
    unless (current == limit) do
      put (current + 1)
      logShow current
      go

programSafe :: forall m. MonadRec m => MonadEffect m => MonadState Int m => m Unit
programSafe = tailRecM go unit
  where
  go _ = do
    current <- get
    if current == limit then do
      pure $ Done unit
    else do
      put (current + 1)
      logShow current
      pure $ Loop unit

main :: Effect Unit
main = do
  Cps.evalStateT 0 programSafe
  Trs.evalStateT programSafe 0

  -- should not max out the call stack
  Cps.evalStateT 0 program
  -- should max out the call stack
  Trs.evalStateT program 0 # catchException \_ -> do
    log "Terminated!"
