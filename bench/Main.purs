module Main where

import Prelude

import Benchotron.Core (Benchmark, benchFn, mkBenchmark)
import Benchotron.UI.Console (runSuite)
import Cps.State as Cps
import Control.Monad.Rec.Class (Step(..), tailRecM)
import Control.Monad.State.Trans (get, put)
import Control.Monad.Trampoline (Trampoline, runTrampoline)
import Control.Monad.RWS as Trs
import Data.Array ((..))
import Effect (Effect)

countToN :: Benchmark
countToN = mkBenchmark
  { slug: "countToN"
  , title: "Count-To-N"
  , sizes: (1 .. 10) <#> (1000 * _)
  , sizeInterpretation: "Limit"
  , inputsPerSize: 100
  , gen: pure
  , functions:
      [ benchFn "naive-transformers" $ \n -> runTrampoline $ Trs.runRWST (program' n) unit 0
      , benchFn "tailrecm-transformers" $ \n -> runTrampoline $ Trs.runRWST (programSafe' n) unit 0
      , benchFn "naive-uncurried-transformers" $ \n -> Cps.runState 0 (program n)
      , benchFn "tailrecm-uncurried-transformers" $ \n -> Cps.runState 0 (programSafe n)
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

  program' :: Int -> Trs.RWST Unit Unit Int Trampoline Unit
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

  programSafe' :: Int -> Trs.RWST Unit Unit Int Trampoline Unit
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
  [ countToN
  ]
