module Cps.RWSET where

import Prelude

import Control.Alt (class Alt)
import Control.Apply (lift2)
import Control.Lazy (class Lazy)
import Control.Monad.Error.Class (class MonadError, class MonadThrow)
import Control.Monad.Reader.Class (class MonadAsk, class MonadReader)
import Control.Monad.Rec.Class (class MonadRec, Step(..), tailRecM)
import Control.Monad.State.Class (class MonadState)
import Control.Monad.Trans.Class (class MonadTrans, lift)
import Control.Monad.Writer.Class (class MonadTell, class MonadWriter)
import Data.Either (Either(..))
import Data.Function.Uncurried (Fn3, Fn6, mkFn3, mkFn6, runFn3, runFn6)
import Data.Newtype (class Newtype)
import Data.Tuple.Nested (type (/\), (/\))
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
      (Fn3 s e w c)
      -- Success
      (Fn3 s a w c)
      c
  )

derive instance Newtype (RWSET c r w s e m a) _

instance Functor (RWSET c r w s e m) where
  map f (RWSET k) = RWSET
    ( mkFn6 \environment state0 more lift' error done ->
        more \_ -> runFn6 k environment state0 more lift' error
          ( mkFn3 \state1 a w ->
              more \_ -> runFn3 done state1 (f a) w
          )
    )

instance Monoid w => Apply (RWSET c r w s e m) where
  apply (RWSET kf) (RWSET ka) = RWSET
    ( mkFn6 \environment state0 more lift' error done ->
        more \_ -> runFn6 kf environment state0 more lift' error
          ( mkFn3 \state1 f w0 ->
              more \_ -> runFn6 ka environment state1 more lift' error
                ( mkFn3 \state2 a w1 ->
                    more \_ -> runFn3 done state2 (f a) (w0 <> w1)
                )
          )
    )

instance Monoid w => Applicative (RWSET c r w s e m) where
  pure a = RWSET
    ( mkFn6 \_ state _ _ _ done ->
        runFn3 done state a mempty
    )

instance Monoid w => Alt (RWSET c r w s e m) where
  alt (RWSET ka) (RWSET kb) = RWSET
    ( mkFn6 \environment state0 more lift error done ->
        more \_ -> runFn6 ka environment state0 more lift
          ( mkFn3 \state1 _ _ ->
              more \_ ->
                runFn6 kb environment state1 more lift error done
          )
          done
    )

instance Monoid w => Bind (RWSET c r w s e m) where
  bind (RWSET kx) f = RWSET
    ( mkFn6 \environment state0 more lift' error done ->
        more \_ -> runFn6 kx environment state0 more lift' error
          ( mkFn3 \state1 x w0 ->
              more \_ -> case f x of
                RWSET ky ->
                  more \_ -> runFn6 ky environment state1 more lift' error
                    ( mkFn3 \state2 y w1 ->
                        more \_ -> runFn3 done state2 y (w0 <> w1)
                    )
          )
    )

instance Monoid w => Monad (RWSET c r w s e m)

instance Monoid w => MonadAsk r (RWSET c r w s e m) where
  ask = RWSET
    ( mkFn6 \environment state _ _ _ done ->
        runFn3 done state environment mempty
    )

instance (Monoid w, MonadEffect m) => MonadEffect (RWSET c r w s e m) where
  liftEffect = lift <<< liftEffect

instance Monoid w => MonadError e (RWSET c r w s e m) where
  catchError (RWSET ka) f = RWSET
    ( mkFn6 \environment state0 more lift' error done ->
        more \_ -> runFn6 ka environment state0 more lift'
          ( mkFn3 \state1 e w0 ->
              case f e of
                RWSET kb ->
                  more \_ -> runFn6 kb environment state1 more lift' error
                    ( mkFn3 \state2 b w1 ->
                        runFn3 done state2 b (w0 <> w1)
                    )
          )
          done
    )

instance Monoid w => MonadReader r (RWSET c r w s e m) where
  local f (RWSET ka) = RWSET
    ( mkFn6 \environment state0 more lift' error done ->
        more \_ -> runFn6 ka (f environment) state0 more lift' error
          ( mkFn3 \state1 a w ->
              more \_ -> runFn3 done state1 a w
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
                  ( mkFn3 \state2 s w ->
                      case s of
                        Loop n ->
                          if gas == 0 then
                            more \_ ->
                              runFn3 loop state2 n 30
                          else
                            runFn3 loop state2 n (gas - 1)
                        Done r ->
                          runFn3 done state2 r w
                  )
        in
          runFn3 loop state0 a 30
    )

instance Monoid w => MonadState s (RWSET c r w s e m) where
  state f = RWSET
    ( mkFn6 \_ state0 _ _ _ done ->
        case f state0 of
          a /\ state1 ->
            runFn3 done state1 a mempty
    )

instance Monoid w => MonadTell w (RWSET c r w s e m) where
  tell w = RWSET
    ( mkFn6 \_ state _ _ _ done ->
        runFn3 done state unit w
    )

instance Monoid w => MonadThrow e (RWSET c r w s e m) where
  throwError e = RWSET
    ( mkFn6 \_ state _ _ error _ ->
        runFn3 error state e mempty
    )

instance Monoid w => MonadTrans (RWSET c r w s e) where
  lift m = RWSET
    ( mkFn6 \_ state _ lift' _ done ->
        lift' (map (\a _ -> runFn3 done state a mempty) m)
    )

instance Monoid w => MonadWriter w (RWSET c r w s e m) where
  listen (RWSET ka) = RWSET
    ( mkFn6 \environment state0 more lift' error done ->
        more \_ -> runFn6 ka environment state0 more lift' error
          ( mkFn3 \state1 a w ->
              more \_ -> runFn3 done state1 (a /\ w) w
          )
    )

  pass (RWSET kaf) = RWSET
    ( mkFn6 \environment state0 more lift' error done ->
        more \_ -> runFn6 kaf environment state0 more lift' error
          ( mkFn3 \state1 (a /\ f) w ->
              more \_ -> runFn3 done state1 a (f w)
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

--

data RunRWSET
  :: Type
  -> Type
  -> Type
  -> Type
  -> (Type -> Type)
  -> Type
  -> Type
data RunRWSET r w s e m a
  = More (Unit -> RunRWSET r w s e m a)
  | Lift (m (Unit -> RunRWSET r w s e m a))
  | Stop s w (Either e a)

runRWSET
  :: forall r w s e m a
   . Monoid w
  => MonadRec m
  => r
  -> s
  -> RWSET (RunRWSET r w s e m a) r w s e m a
  -> m (s /\ w /\ Either e a)
runRWSET r s (RWSET k) =
  let
    go step = case step unit of
      More n ->
        go n
      Lift m ->
        Loop <$> m
      Stop s' w a ->
        pure $ Done $ s' /\ w /\ a
  in
    tailRecM go \_ ->
      runFn6 k r s More Lift
        (mkFn3 \s' e w -> Stop s' w (Left e))
        (mkFn3 \s' a w -> Stop s' w (Right a))
