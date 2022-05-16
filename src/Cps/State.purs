module Cps.State where

import Prelude

import Cps.StateT (StateT, evalStateT, execStateT, mapStateT, runStateT, withStateT)
import Data.Identity (Identity(..))
import Data.Tuple.Nested (type (/\))
import Safe.Coerce (coerce)

type State s = StateT s Identity

runState :: forall s a. s -> State s a -> (a /\ s)
runState s = coerce <<< runStateT s

evalState :: forall s a. s -> State s a -> a
evalState s = coerce <<< evalStateT s

execState :: forall s a. s -> State s a -> s
execState s = coerce <<< execStateT s

mapState :: forall s a b. ((a /\ s) -> (b /\ s)) -> State s a -> State s b
mapState f = mapStateT (coerce f)

withState :: forall s a. (s -> s) -> State s a -> State s a
withState = withStateT
