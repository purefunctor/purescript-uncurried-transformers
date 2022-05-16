module Cps.Reader where

import Prelude

import Cps.ReaderT (ReaderT, mapReaderT, runReaderT, withReaderT)
import Data.Identity (Identity(..))
import Safe.Coerce (coerce)

type Reader r = ReaderT r Identity

runReader :: forall r a. r -> Reader r a -> a
runReader r = coerce <<< runReaderT r

withReader :: forall r1 r2 a. (r2 -> r1) -> Reader r1 a -> Reader r2 a
withReader = withReaderT

mapReader :: forall r a b. (a -> b) -> Reader r a -> Reader r b
mapReader = mapReaderT <<< coerce
