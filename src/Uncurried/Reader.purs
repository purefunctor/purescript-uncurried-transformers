-- | This module defines the `Reader` monad.
module Uncurried.Reader where

import Prelude

import Data.Identity (Identity(..))
import Safe.Coerce (coerce)
import Uncurried.ReaderT (ReaderT, mapReaderT, runReaderT, withReaderT)

-- | A type synonym for a `ReaderT` with `Identity` as its base monad.
type Reader r = ReaderT r Identity

-- | Runs a computation inside of `Reader`.
runReader :: forall r a. r -> Reader r a -> a
runReader r = coerce <<< runReaderT r

-- | Modifies the result type of a `Reader`.
mapReader :: forall r a b. (a -> b) -> Reader r a -> Reader r b
mapReader = mapReaderT <<< coerce

-- | Modifies the environment type of a `Reader`.
withReader :: forall r1 r2 a. (r2 -> r1) -> Reader r1 a -> Reader r2 a
withReader = withReaderT
