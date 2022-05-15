module Cps.State where

import Prelude

import Control.Monad.Rec.Class (class MonadRec, Step(..), tailRecM)
import Control.Monad.State.Class (class MonadState)
import Control.Monad.Trans.Class (class MonadTrans)
import Data.Function.Uncurried (Fn2, Fn4, mkFn2, mkFn4, runFn2, runFn4)
import Data.Newtype (class Newtype)
import Data.Tuple (fst, snd)
import Data.Tuple.Nested (type (/\), (/\))

newtype StateT r s m a = StateT
  ( Fn4
      -- State
      s
      -- Trampoline
      ((Unit -> r) -> r)
      -- Lift
      (m (Unit -> r) -> r)
      -- Result
      (Fn2 s a r)
      -- Continuation
      r
  )

derive instance Newtype (StateT r s m a) _

instance Functor (StateT r s m) where
  map f (StateT s) = StateT
    ( mkFn4 \state0 more lift done ->
        more \_ -> runFn4 s state0 more lift
          ( mkFn2 \state1 a ->
              more \_ -> runFn2 done state1 (f a)
          )
    )

instance Apply (StateT r s m) where
  apply (StateT f) (StateT a) = StateT
    ( mkFn4 \state0 more lift done ->
        more \_ -> runFn4 f state0 more lift
          ( mkFn2 \state1 f' ->
              more \_ -> runFn4 a state1 more lift
                ( mkFn2 \state2 a' ->
                    more \_ -> runFn2 done state2 (f' a')
                )
          )
    )

instance Applicative (StateT r s m) where
  pure a = StateT
    ( mkFn4 \state _ _ done ->
        runFn2 done state a
    )

instance Bind (StateT r s m) where
  bind (StateT x) f = StateT
    ( mkFn4 \state0 more lift done ->
        more \_ -> runFn4 x state0 more lift
          ( mkFn2 \state1 x' ->
              more \_ ->
                case f x' of
                  StateT x'' ->
                    runFn4 x'' state1 more lift done
          )
    )

instance Monad (StateT r s m)

instance MonadState s (StateT r s m) where
  state f = StateT
    ( mkFn4 \state0 _ _ done ->
        case f state0 of
          a /\ state1 ->
            runFn2 done state1 a
    )

instance MonadTrans (StateT r s) where
  lift m = StateT
    ( mkFn4 \state _ lift' done ->
        lift' $ map (\a _ -> runFn2 done state a) m
    )

--

data RunStateT s m a
  = More (Unit -> RunStateT s m a)
  | Lift (m (Unit -> RunStateT s m a))
  | Stop s a

runStateT
  :: forall s m a
   . MonadRec m
  => s
  -> StateT (RunStateT s m a) s m a
  -> m (a /\ s)
runStateT state0 (StateT stateT) =
  let
    go step = case step unit of
      More n ->
        go n
      Lift m ->
        Loop <$> m
      Stop s a ->
        pure $ Done $ a /\ s
  in
    tailRecM go \_ ->
      runFn4 stateT state0 More Lift (mkFn2 \s a -> Stop s a)

evalStateT
  :: forall s m a
   . MonadRec m
  => s
  -> StateT (RunStateT s m a) s m a
  -> m a
evalStateT s m = fst <$> runStateT s m

execStateT
  :: forall s m a
   . MonadRec m
  => s
  -> StateT (RunStateT s m a) s m a
  -> m s
execStateT s m = snd <$> runStateT s m
