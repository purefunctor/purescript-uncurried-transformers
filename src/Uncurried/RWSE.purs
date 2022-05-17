-- | This module defines the `RWSE` monad.
module Uncurried.RWSE
  ( RWSE
  , rwse
  , runRWSE
  , evalRWSE
  , execRWSE
  , mapRWSE
  , withRWSE
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Function.Uncurried (mkFn6, runFn3)
import Data.Identity (Identity(..))
import Data.Tuple.Nested (type (/\), (/\))
import Safe.Coerce (coerce)
import Uncurried.RWSET (RWSET(..), evalRWSET, execRWSET, mapRWSET, runRWSET, withRWSET)

-- | A type synonym for a `RWSET` with `Identity` as its base monad.
type RWSE r w s e = RWSET r w s e Identity

-- | Create an `RWSE` monad from a function that takes the environment
-- | and the state, and returns a new state, an error or a result, and
-- | an accumulator.
rwse :: forall r w s e a. Monoid w => (r -> s -> (s /\ Either e a /\ w)) -> RWSE r w s e a
rwse f = RWSET
  ( mkFn6 \environment state0 _ _ error done ->
      case f environment state0 of
        s /\ Left e /\ w ->
          runFn3 error s e w
        s /\ Right a /\ w ->
          runFn3 done s a w
  )

-- | Runs a computation inside of `RWSE`.
runRWSE :: forall r w s e a. Monoid w => r -> s -> RWSE r w s e a -> (s /\ Either e a /\ w)
runRWSE r s = coerce <<< runRWSET r s

-- | Runs a computation inside of `RWSE`, discarding the final state.
evalRWSE :: forall r w s e a. Monoid w => r -> s -> RWSE r w s e a -> (Either e a /\ w)
evalRWSE r s = coerce <<< evalRWSET r s

-- | Runs a computation inside of `RWSE`, discarding the final result.
execRWSE :: forall r w s e a. Monoid w => r -> s -> RWSE r w s e a -> (s /\ w)
execRWSE r s = coerce <<< execRWSET r s

-- | Modifies the result and accumulator types of a `RWSE`.
mapRWSE
  :: forall r w1 w2 s e a b
   . ((s /\ Either e a /\ w1) -> (s /\ Either e b /\ w2))
  -> RWSE r w1 s e a
  -> RWSE r w2 s e b
mapRWSE f = mapRWSET (coerce f)

-- | Modifies the environment type of a `RWSE`.
withRWSE :: forall r1 r2 w s e a. (r2 -> s -> r1 /\ s) -> RWSE r1 w s e a -> RWSE r2 w s e a
withRWSE = withRWSET
