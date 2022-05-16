module Main where

import Prelude

import Benchotron.Core (Benchmark, benchFn, mkBenchmark)
import Benchotron.UI.Console (runSuite)
import Cps.State as Cps
import Control.Monad.Rec.Class (Step(..), tailRecM)
import Control.Monad.State.Trans (get, put)
import Control.Monad.RWS as Trs
import Data.Array ((..))
import Effect (Effect)

stateLoop :: Benchmark
stateLoop = mkBenchmark
  { slug: "stateLoop"
  , title: "State Loop"
  , sizes: (0 .. 9) <#> (\n -> 1000 + 100 * n)
  , sizeInterpretation: "Recursion depth"
  , inputsPerSize: 100
  , gen: pure
  , functions:
      [ benchFn "safe-uncurried-transformers" $ \n -> Cps.runState 0 (programSafe n)
      , benchFn "safe-transformers" $ \n -> Trs.runRWS (programSafe' n) unit 0
      , benchFn "unsafe-uncurried-transformers" $ \n -> Cps.runState 0 (program n)
      , benchFn "unsafe-transformers" $ \n -> Trs.runRWS (program' n) unit 0
      ]
  }
  where
  program :: Int -> Cps.State Int Unit
  program limit = go unit
    where
    go _ = do
      current <- get
      unless (current == limit) do
        put (current + 1)
        go unit

  program' :: Int -> Trs.RWS Unit Unit Int Unit
  program' limit = go unit
    where
    go _ = do
      current <- get
      unless (current == limit) do
        put (current + 1)
        go unit

  programSafe :: Int -> Cps.State Int Unit
  programSafe limit = tailRecM go unit
    where
    go _ = do
      current <- get
      if current == limit then do
        pure $ Done unit
      else do
        put (current + 1)
        pure $ Loop unit

  programSafe' :: Int -> Trs.RWS Unit Unit Int Unit
  programSafe' limit = tailRecM go unit
    where
    go _ = do
      current <- get
      if current == limit then do
        pure $ Done unit
      else do
        put (current + 1)
        pure $ Loop unit

main :: Effect Unit
main = runSuite
  [ stateLoop
  ]
