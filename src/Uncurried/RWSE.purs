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

import Control.Alt (class Alt)
import Control.Apply (lift2)
import Control.Monad.Error.Class (class MonadError, class MonadThrow)
import Control.Monad.Reader (class MonadAsk, class MonadReader)
import Control.Monad.State (class MonadState)
import Control.Monad.Writer (class MonadTell, class MonadWriter)
import Data.Either (Either(..))
import Data.Function.Uncurried (Fn3, Fn4, mkFn3, mkFn4, runFn3, runFn4)
import Data.Tuple (snd)
import Data.Tuple.Nested (type (/\), (/\))

newtype RWSE :: Type -> Type -> Type -> Type -> Type -> Type
newtype RWSE r w s e a = RWSE
  ( forall c. Fn4 r s (Fn3 s e w c) (Fn3 s a w c) c
  )

instance Functor (RWSE r w s e) where
  map f (RWSE k) = RWSE
    ( mkFn4 \environment state error done ->
        runFn4 k environment state error
          ( mkFn3 \s a w ->
              runFn3 done s (f a) w
          )
    )

instance Monoid w => Apply (RWSE r w s e) where
  apply (RWSE kf) (RWSE ka) = RWSE
    ( mkFn4 \environment state0 error done ->
        runFn4 kf environment state0 error
          ( mkFn3 \state1 f w0 ->
              runFn4 ka environment state1 error
                ( mkFn3 \state2 a w1 ->
                    runFn3 done state2 (f a) (w0 <> w1)
                )
          )
    )

instance Monoid w => Applicative (RWSE r w s e) where
  pure a = RWSE
    ( mkFn4 \_ state _ done ->
        runFn3 done state a mempty
    )

instance Monoid w => Alt (RWSE r w s e) where
  alt (RWSE ka) (RWSE kb) = RWSE
    ( mkFn4 \environment state error done ->
        runFn4 ka environment state
          ( mkFn3 \s' _ _ ->
              runFn4 kb environment s' error done
          )
          done
    )

instance Monoid w => Bind (RWSE r w s e) where
  bind (RWSE kx) f = RWSE
    ( mkFn4 \environment state0 error done ->
        runFn4 kx environment state0 error
          ( mkFn3 \state1 x w0 ->
              case f x of
                RWSE ky ->
                  runFn4 ky environment state1 error
                    ( mkFn3 \state2 y w1 ->
                        runFn3 done state2 y (w0 <> w1)
                    )
          )
    )

instance Monoid w => Monad (RWSE r w s e)

instance Monoid w => MonadThrow e (RWSE r w s e) where
  throwError e = RWSE
    ( mkFn4 \_ state error _ ->
        runFn3 error state e mempty
    )

instance Monoid w => MonadError e (RWSE r w s e) where
  catchError (RWSE ka) f = RWSE
    ( mkFn4 \environment state0 error done ->
        runFn4 ka environment state0
          ( mkFn3 \state1 e w0 ->
              case f e of
                RWSE kb ->
                  runFn4 kb environment state1 error
                    ( mkFn3 \state2 b w1 ->
                        runFn3 done state2 b (w0 <> w1)
                    )
          )
          done
    )

instance Monoid w => MonadAsk r (RWSE r w s e) where
  ask = RWSE
    ( mkFn4 \environment state _ done ->
        runFn3 done state environment mempty
    )

instance Monoid w => MonadReader r (RWSE r w s e) where
  local f (RWSE ka) = RWSE
    ( mkFn4 \environment state0 error done ->
        runFn4 ka (f environment) state0 error
          ( mkFn3 \state1 a w ->
              runFn3 done state1 a w
          )
    )

instance Monoid w => MonadTell w (RWSE r w s e) where
  tell w = RWSE
    ( mkFn4 \_ state _ done ->
        runFn3 done state unit w
    )

instance Monoid w => MonadWriter w (RWSE r w s e) where
  listen (RWSE ka) = RWSE
    ( mkFn4 \environment state0 error done ->
        runFn4 ka environment state0 error
          ( mkFn3 \state1 a w ->
              runFn3 done state1 (a /\ w) w
          )
    )

  pass (RWSE kaf) = RWSE
    ( mkFn4 \environment state0 error done ->
        runFn4 kaf environment state0 error
          ( mkFn3 \state1 (a /\ f) w ->
              runFn3 done state1 a (f w)
          )
    )

instance Monoid w => MonadState s (RWSE r w s e) where
  state f = RWSE
    ( mkFn4 \_ state0 _ done ->
        case f state0 of
          a /\ state1 ->
            runFn3 done state1 a mempty
    )

instance (Monoid w, Semigroup a) => Semigroup (RWSE r w s e a) where
  append = lift2 (<>)

instance (Monoid w, Monoid a) => Monoid (RWSE r w s e a) where
  mempty = pure mempty

-- | Create an `RWSE` monad from a function that takes the environment
-- | and the state, and returns a new state, an error or a result, and
-- | an accumulator.
rwse :: forall r w s e a. Monoid w => (r -> s -> (s /\ Either e a /\ w)) -> RWSE r w s e a
rwse f = RWSE
  ( mkFn4 \environment state error done ->
      case f environment state of
        (s /\ Left e /\ w) ->
          runFn3 error s e w
        (s /\ Right a /\ w) ->
          runFn3 done s a w
  )

-- | Runs a computation inside of `RWSE`.
runRWSE :: forall r w s e a. r -> s -> RWSE r w s e a -> (s /\ Either e a /\ w)
runRWSE r s (RWSE k) =
  ( runFn4 k r s
      (mkFn3 \s' e w -> (s' /\ Left e /\ w))
      (mkFn3 \s' a w -> (s' /\ Right a /\ w))
  )

-- | Runs a computation inside of `RWSE`, discarding the final state.
evalRWSE :: forall r w s e a. Monoid w => r -> s -> RWSE r w s e a -> (Either e a /\ w)
evalRWSE r s = snd <<< runRWSE r s

-- | Runs a computation inside of `RWSE`, discarding the final result.
execRWSE :: forall r w s e a. Monoid w => r -> s -> RWSE r w s e a -> (s /\ w)
execRWSE r s = map snd <<< runRWSE r s

-- | Modifies the result and accumulator types of a `RWSE`.
mapRWSE
  :: forall r w1 w2 s e a b
   . ((s /\ Either e a /\ w1) -> (s /\ Either e b /\ w2))
  -> RWSE r w1 s e a
  -> RWSE r w2 s e b
mapRWSE f k = RWSE
  ( mkFn4 \environment state error done ->
      case f (runRWSE environment state k) of
        (s /\ Left e /\ w) ->
          runFn3 error s e w
        (s /\ Right a /\ w) ->
          runFn3 done s a w
  )

-- | Modifies the environment type of a `RWSE`.
withRWSE :: forall r1 r2 w s e a. (r2 -> s -> r1 /\ s) -> RWSE r1 w s e a -> RWSE r2 w s e a
withRWSE f (RWSE k) = RWSE
  ( mkFn4 \environment state error done ->
      case f environment state of
        (environment' /\ state') ->
          runFn4 k environment' state' error done
  )
