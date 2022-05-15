module Cps.RWSET where

import Prelude

import Control.Alt (class Alt)
import Control.Alternative (class Alternative)
import Control.Apply (lift2)
import Control.Lazy (class Lazy)
import Control.Monad.Error.Class (class MonadError, class MonadThrow)
import Control.Monad.Reader.Class (class MonadAsk, class MonadReader)
import Control.Monad.Rec.Class (class MonadRec, Step(..))
import Control.Monad.State.Class (class MonadState)
import Control.Monad.Trans.Class (class MonadTrans, lift)
import Control.Monad.Writer.Class (class MonadTell, class MonadWriter)
import Control.Plus (class Plus)
import Data.Function.Uncurried (Fn2, Fn6, mkFn2, mkFn3, mkFn6, runFn2, runFn3, runFn6)
import Data.Newtype (class Newtype)
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))
import Effect.Class (class MonadEffect, liftEffect)

newtype RWSET
  :: Type
  -> Type
  -> Type
  -> Type
  -> Type
  -> (Type -> Type)
  -> Type
  -> Type
newtype RWSET c r w s e m a = RWSET
  ( Fn6
      -- Environment
      r
      -- State
      s
      -- Trampoline
      ((Unit -> c) -> c)
      -- Lift
      (m (Unit -> c) -> c)
      -- Error
      (Fn2 s e c)
      -- Success
      (Fn2 s (Tuple a w) c)
      c
  )

derive instance Newtype (RWSET c r w s e m a) _

instance Functor (RWSET c r w s e m) where
  map f (RWSET k) = RWSET
    ( mkFn6 \environment state0 more lift' error done ->
        more \_ -> runFn6 k environment state0 more lift' error
          ( mkFn2 \state1 (a /\ w) ->
              more \_ -> runFn2 done state1 (f a /\ w)
          )
    )

instance Monoid w => Apply (RWSET c r w s e m) where
  apply (RWSET kf) (RWSET ka) = RWSET
    ( mkFn6 \environment state0 more lift' error done ->
        more \_ -> runFn6 kf environment state0 more lift' error
          ( mkFn2 \state1 (f /\ w0) ->
              more \_ -> runFn6 ka environment state1 more lift' error
                ( mkFn2 \state2 (a /\ w1) ->
                    more \_ -> runFn2 done state2 (f a /\ (w0 <> w1))
                )
          )
    )

instance Monoid w => Applicative (RWSET c r w s e m) where
  pure a = RWSET
    ( mkFn6 \_ state _ _ _ done ->
        runFn2 done state (a /\ mempty)
    )

instance Monoid w => Alt (RWSET c r w s e m) where
  alt (RWSET ka) (RWSET kb) = RWSET
    ( mkFn6 \environment state0 more lift error done ->
        more \_ -> runFn6 ka environment state0 more lift
          ( mkFn2 \state1 _ ->
              more \_ ->
                runFn6 kb environment state1 more lift error done
          )
          done
    )

instance (Monoid w, Monoid e) => Alternative (RWSET c r w s e m)

instance Monoid w => Bind (RWSET c r w s e m) where
  bind (RWSET kx) f = RWSET
    ( mkFn6 \environment state0 more lift' error done ->
        more \_ -> runFn6 kx environment state0 more lift' error
          ( mkFn2 \state1 (x /\ w0) ->
              more \_ -> case f x of
                RWSET ky ->
                  more \_ -> runFn6 ky environment state1 more lift' error
                    ( mkFn2 \state2 (y /\ w1) ->
                        more \_ -> runFn2 done state2 (y /\ (w0 <> w1))
                    )
          )
    )

instance Monoid w => Monad (RWSET c r w s e m)

instance (Monoid w, Monoid e) => Plus (RWSET c r w s e m) where
  empty = RWSET
    ( mkFn6 \_ state _ _ error _ ->
        runFn2 error state mempty
    )

instance Monoid w => MonadAsk r (RWSET c r w s e m) where
  ask = RWSET
    ( mkFn6 \environment state _ _ _ done ->
        runFn2 done state (environment /\ mempty)
    )

instance (Monoid w, MonadEffect m) => MonadEffect (RWSET c r w s e m) where
  liftEffect = lift <<< liftEffect

instance Monoid w => MonadError e (RWSET c r w s e m) where
  catchError (RWSET ka) f = RWSET
    ( mkFn6 \environment state0 more lift' error done ->
        more \_ -> runFn6 ka environment state0 more lift'
          ( mkFn2 \state1 a ->
              case f a of
                RWSET kb ->
                  runFn6 kb environment state1 more lift' error done
          )
          done
    )

instance Monoid w => MonadReader r (RWSET c r w s e m) where
  local f (RWSET ka) = RWSET
    ( mkFn6 \environment state0 more lift' error done ->
        more \_ -> runFn6 ka (f environment) state0 more lift' error
          ( mkFn2 \state1 aw ->
              more \_ -> runFn2 done state1 aw
          )
    )

instance Monoid w => MonadRec (RWSET c r w s e m) where
  tailRecM f a = RWSET
    ( mkFn6 \environment state0 more lift error done ->
        let
          loop = mkFn3 \state1 a' gas ->
            case f a' of
              RWSET k ->
                runFn6 k environment state1 more lift error
                  ( mkFn2 \state2 (s /\ w) ->
                      case s of
                        Loop n ->
                          if gas == 0 then
                            more \_ ->
                              runFn3 loop state2 n 30
                          else
                            runFn3 loop state2 n (gas - 1)
                        Done r ->
                          runFn2 done state2 (r /\ w)
                  )
        in
          runFn3 loop state0 a 30
    )

instance Monoid w => MonadState s (RWSET c r w s e m) where
  state f = RWSET
    ( mkFn6 \_ state0 _ _ _ done ->
        case f state0 of
          a /\ state1 ->
            runFn2 done state1 (a /\ mempty)
    )

instance Monoid w => MonadTell w (RWSET c r w s e m) where
  tell w = RWSET
    ( mkFn6 \_ state _ _ _ done ->
        runFn2 done state (unit /\ w)
    )

instance Monoid w => MonadThrow e (RWSET c r w s e m) where
  throwError e = RWSET
    ( mkFn6 \_ state _ _ error _ ->
        runFn2 error state e
    )

instance Monoid w => MonadTrans (RWSET c r w s e) where
  lift m = RWSET
    ( mkFn6 \_ state _ lift' _ done ->
        lift' (map (\a _ -> runFn2 done state (a /\ mempty)) m)
    )

instance Monoid w => MonadWriter w (RWSET c r w s e m) where
  listen (RWSET ka) = RWSET
    ( mkFn6 \environment state0 more lift' error done ->
        more \_ -> runFn6 ka environment state0 more lift' error
          ( mkFn2 \state1 (a /\ w) ->
              more \_ -> runFn2 done state1 ((a /\ w) /\ w)
          )
    )

  pass (RWSET kaf) = RWSET
    ( mkFn6 \environment state0 more lift' error done ->
        more \_ -> runFn6 kaf environment state0 more lift' error
          ( mkFn2 \state1 ((a /\ f) /\ w) ->
              more \_ -> runFn2 done state1 (a /\ f w)
          )
    )

instance (Monoid w, Semigroup a) => Semigroup (RWSET c r w s e m a) where
  append = lift2 (<>)

instance (Monoid w, Monoid a) => Monoid (RWSET c r w s e m a) where
  mempty = pure mempty

instance Monoid w => Lazy (RWSET c r w s e m a) where
  defer f = RWSET
    ( mkFn6 \environment state0 more lift' error done ->
        case f unit of
          RWSET k -> runFn6 k environment state0 more lift' error done
    )
