-- | This modules defines the writer monad, `Writer`.
module Uncurried.Writer where

import Prelude

import Data.Identity (Identity(..))
import Data.Tuple.Nested (type (/\))
import Safe.Coerce (coerce)
import Uncurried.WriterT (WriterT, evalWriterT, execWriterT, mapWriterT, runWriterT, writerT)

-- | A type synonym for a `WriterT` with `Identity` as its base monad.
type Writer s = WriterT s Identity

-- | Create a `Writer` monad from a pair of a result and an accumulator.
writer :: forall w a. Monoid w => (a /\ w) -> Writer w a
writer = writerT <<< coerce

-- | Runs a computation inside of `Writer`.
runWriter :: forall w a. Monoid w => Writer w a -> (a /\ w)
runWriter = coerce <<< runWriterT

-- | Runs a computation inside of `Writer`, discarding the final accumulator.
evalWriter :: forall w a. Monoid w => Writer w a -> a
evalWriter = coerce <<< evalWriterT

-- | Runs a computation inside of `Writer`, discarding the final result.
execWriter :: forall w a. Monoid w => Writer w a -> w
execWriter = coerce <<< execWriterT

-- | Modifies the result and accumulator types of a `Writer`.
mapWriter
  :: forall w1 w2 a b
   . Monoid w1
  => Monoid w2
  => ((a /\ w1) -> (b /\ w2))
  -> Writer w1 a
  -> Writer w2 b
mapWriter f = mapWriterT (coerce f)
