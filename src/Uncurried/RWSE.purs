module Uncurried.RWSE where

import Prelude

import Data.Either (Either)
import Data.Identity (Identity(..))
import Data.Tuple.Nested (type (/\))
import Safe.Coerce (coerce)
import Uncurried.RWSET (RWSET, evalRWSET, execRWSET, mapRWSET, runRWSET, withRWSET)

type RWSE r w s e = RWSET r w s e Identity

runRWSE :: forall r w s e a. Monoid w => r -> s -> RWSE r w s e a -> (s /\ Either e a /\ w)
runRWSE r s = coerce <<< runRWSET r s

evalRWSE :: forall r w s e a. Monoid w => r -> s -> RWSE r w s e a -> (Either e a /\ w)
evalRWSE r s = coerce <<< evalRWSET r s

execRWSE :: forall r w s e a. Monoid w => r -> s -> RWSE r w s e a -> (s /\ w)
execRWSE r s = coerce <<< execRWSET r s

mapRWSE
  :: forall r w1 w2 s e a b
   . ((s /\ Either e a /\ w1) -> (s /\ Either e b /\ w2))
  -> RWSE r w1 s e a
  -> RWSE r w2 s e b
mapRWSE f = mapRWSET (coerce f)

withRWS :: forall r1 r2 w s e a. (r2 -> s -> r1 /\ s) -> RWSE r1 w s e a -> RWSE r2 w s e a
withRWS = withRWSET
