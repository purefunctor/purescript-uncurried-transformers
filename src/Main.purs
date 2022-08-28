module Main where

import Prelude

import Effect (Effect)
import Effect.Console (log, logShow)
import Uncurried.RWSE (RWSE, runRWSE)

a :: RWSE Unit Unit Unit Unit Int
a = do
  x <- pure 21
  y <- pure 21
  pure $ x + y

main :: Effect Unit
main = do
  log "Hello, World"
  logShow $ runRWSE unit unit a
