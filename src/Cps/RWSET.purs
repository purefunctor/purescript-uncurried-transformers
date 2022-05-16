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
import Data.Tuple (snd)
import Data.Tuple.Nested (type (/\), (/\))
import Effect.Class (class MonadEffect, liftEffect)

newtype RWSET
  :: Type
  -> Type
  -> Type
  -> Type
  -> (Type -> Type)
  -> Type
  -> Type
newtype RWSET r w s e m a = RWSET
  ( forall c
     . Fn6
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
         -- Continuation
         c
  )

instance Functor (RWSET r w s e m) where
  map f (RWSET k) = RWSET
    ( mkFn6 \environment state0 more lift' error done ->
        more \_ -> runFn6 k environment state0 more lift' error
          ( mkFn3 \state1 a w ->
              more \_ -> runFn3 done state1 (f a) w
          )
    )

instance Monoid w => Apply (RWSET r w s e m) where
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

instance Monoid w => Applicative (RWSET r w s e m) where
  pure a = RWSET
    ( mkFn6 \_ state _ _ _ done ->
        runFn3 done state a mempty
    )

instance Monoid w => Alt (RWSET r w s e m) where
  alt (RWSET ka) (RWSET kb) = RWSET
    ( mkFn6 \environment state0 more lift error done ->
        more \_ -> runFn6 ka environment state0 more lift
          ( mkFn3 \state1 _ _ ->
              more \_ ->
                runFn6 kb environment state1 more lift error done
          )
          done
    )

instance Monoid w => Bind (RWSET r w s e m) where
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

instance Monoid w => Monad (RWSET r w s e m)

instance Monoid w => MonadAsk r (RWSET r w s e m) where
  ask = RWSET
    ( mkFn6 \environment state _ _ _ done ->
        runFn3 done state environment mempty
    )

instance (Monoid w, MonadEffect m) => MonadEffect (RWSET r w s e m) where
  liftEffect = lift <<< liftEffect

instance Monoid w => MonadError e (RWSET r w s e m) where
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

instance Monoid w => MonadReader r (RWSET r w s e m) where
  local f (RWSET ka) = RWSET
    ( mkFn6 \environment state0 more lift' error done ->
        more \_ -> runFn6 ka (f environment) state0 more lift' error
          ( mkFn3 \state1 a w ->
              more \_ -> runFn3 done state1 a w
          )
    )

instance Monoid w => MonadRec (RWSET r w s e m) where
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

instance Monoid w => MonadState s (RWSET r w s e m) where
  state f = RWSET
    ( mkFn6 \_ state0 _ _ _ done ->
        case f state0 of
          a /\ state1 ->
            runFn3 done state1 a mempty
    )

instance Monoid w => MonadTell w (RWSET r w s e m) where
  tell w = RWSET
    ( mkFn6 \_ state _ _ _ done ->
        runFn3 done state unit w
    )

instance Monoid w => MonadThrow e (RWSET r w s e m) where
  throwError e = RWSET
    ( mkFn6 \_ state _ _ error _ ->
        runFn3 error state e mempty
    )

instance Monoid w => MonadTrans (RWSET r w s e) where
  lift m = RWSET
    ( mkFn6 \_ state _ lift' _ done ->
        lift' (map (\a _ -> runFn3 done state a mempty) m)
    )

instance Monoid w => MonadWriter w (RWSET r w s e m) where
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

instance (Monoid w, Semigroup a) => Semigroup (RWSET r w s e m a) where
  append = lift2 (<>)

instance (Monoid w, Monoid a) => Monoid (RWSET r w s e m a) where
  mempty = pure mempty

instance Monoid w => Lazy (RWSET r w s e m a) where
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
  | Stop s (Either e a) w

runRWSET
  :: forall r w s e m a
   . MonadRec m
  => r
  -> s
  -> RWSET r w s e m a
  -> m (s /\ Either e a /\ w)
runRWSET r s (RWSET k) =
  let
    go step = case step unit of
      More n ->
        go n
      Lift m ->
        Loop <$> m
      Stop s' a w ->
        pure $ Done $ s' /\ a /\ w
  in
    tailRecM go \_ ->
      runFn6 k r s More Lift
        (mkFn3 \s' e w -> Stop s' (Left e) w)
        (mkFn3 \s' a w -> Stop s' (Right a) w)

evalRWSET
  :: forall r w s e m a
   . MonadRec m
  => r
  -> s
  -> RWSET r w s e m a
  -> m (Either e a /\ w)
evalRWSET r s k = snd <$> runRWSET r s k

execRWSET
  :: forall r w s e m a
   . MonadRec m
  => r
  -> s
  -> RWSET r w s e m a
  -> m (s /\ w)
execRWSET r s k = (map snd) <$> runRWSET r s k

hoistRWSET :: forall r w s e m n a. (m ~> n) -> RWSET r w s e m a -> RWSET r w s e n a
hoistRWSET f (RWSET k) = RWSET
  ( mkFn6 \environment state more lift' error done ->
      runFn6 k environment state more (lift' <<< f) error done
  )

mapRWSET
  :: forall r w1 w2 s e m1 m2 a1 a2
   . MonadRec m1
  => Functor m2
  => (m1 (s /\ Either e a1 /\ w1) -> m2 (s /\ Either e a2 /\ w2))
  -> RWSET r w1 s e m1 a1
  -> RWSET r w2 s e m2 a2
mapRWSET f k = RWSET
  ( mkFn6 \environment state _ lift' error done ->
      lift' $ f (runRWSET environment state k) <#> \(s /\ ea /\ w) _ ->
        case ea of
          Left e ->
            runFn3 error s e w
          Right a ->
            runFn3 done s a w
  )

withRWSET
  :: forall r1 r2 w s e m a. (r2 -> s -> r1 /\ s) -> RWSET r1 w s e m a -> RWSET r2 w s e m a
withRWSET f (RWSET k) = RWSET
  ( mkFn6 \environment1 state1 more lift' error done ->
      case f environment1 state1 of
        (environment2 /\ state2) ->
          runFn6 k environment2 state2 more lift' error done
  )
