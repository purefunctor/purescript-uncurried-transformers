-- | This module defines the `State` monad.
module Uncurried.State where

import Prelude

import Data.Identity (Identity(..))
import Data.Tuple.Nested (type (/\))
import Safe.Coerce (coerce)
import Uncurried.StateT (StateT, evalStateT, execStateT, mapStateT, runStateT, stateT, withStateT)

-- | A type synonym for a `StateT` with `Identity` as its base monad.
type State s = StateT s Identity

-- | Construct a `State` given a function that treads through some state.
state :: forall s a. (s -> (a /\ s)) -> State s a
state = stateT <<< coerce

-- | Runs a computation inside of `State`.
runState :: forall s a. s -> State s a -> (a /\ s)
runState s = coerce <<< runStateT s

-- | Runs a computation inside of `State`, discarding the final state.
evalState :: forall s a. s -> State s a -> a
evalState s = coerce <<< evalStateT s

-- | Runs a computation inside of `State`, discarding the final result.
execState :: forall s a. s -> State s a -> s
execState s = coerce <<< execStateT s

-- | Modifies the result type of a `State`.
mapState :: forall s a b. ((a /\ s) -> (b /\ s)) -> State s a -> State s b
mapState f = mapStateT (coerce f)

-- | Modifies the state of a `State`.
withState :: forall s a. (s -> s) -> State s a -> State s a
withState = withStateT
