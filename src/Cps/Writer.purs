module Cps.Writer where

import Prelude

import Cps.RWSET (RWSET(..))
import Cps.WriterT (WriterT(..), evalWriterT, execWriterT, mapWriterT, runWriterT)
import Data.Function.Uncurried (mkFn6, runFn3)
import Data.Identity (Identity(..))
import Data.Tuple.Nested (type (/\), (/\))
import Safe.Coerce (coerce)

type Writer s = WriterT s Identity

writer :: forall w a. Monoid w => (a /\ w) -> Writer w a
writer (a /\ w) = WriterT
  ( RWSET
      ( mkFn6 \_ _ _ _ _ done ->
          runFn3 done unit a w
      )
  )

runWriter :: forall w a. Monoid w => Writer w a -> (a /\ w)
runWriter = coerce <<< runWriterT

evalWriter :: forall w a. Monoid w => Writer w a -> a
evalWriter = coerce <<< evalWriterT

execWriter :: forall w a. Monoid w => Writer w a -> w
execWriter = coerce <<< execWriterT

mapWriter
  :: forall w1 w2 a b
   . Monoid w1
  => Monoid w2
  => ((a /\ w1) -> (b /\ w2))
  -> Writer w1 a
  -> Writer w2 b
mapWriter f = mapWriterT (coerce f)
