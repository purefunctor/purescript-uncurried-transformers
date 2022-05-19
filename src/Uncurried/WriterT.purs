-- | This module defines the writer monad transformer, `WriterT`.
module Uncurried.WriterT where

import Prelude

import Control.Lazy (class Lazy)
import Control.Monad.Error.Class (class MonadError, class MonadThrow, catchError, throwError)
import Control.Monad.Reader.Class (class MonadAsk, class MonadReader, ask, local)
import Control.Monad.Rec.Class (class MonadRec)
import Control.Monad.State.Class (class MonadState, state)
import Control.Monad.Trans.Class (class MonadTrans, lift)
import Control.Monad.Writer.Class (class MonadTell, class MonadWriter)
import Data.Either (Either(..))
import Data.Function.Uncurried (mkFn6, runFn3)
import Data.Tuple (fst, snd)
import Data.Tuple.Nested (type (/\), (/\))
import Effect.Class (class MonadEffect)
import Uncurried.RWSET (RWSET(..), hoistRWSET, runRWSET)

-- | The writer monad transformer, implemented as a newtype over
-- | `RWSET`. Note that it's not recommended to stack newtypes of
-- | `RWSET` together as it incurs an indeterminate performance
-- | penalty that would otherwise be solved by just using `RWSET`.
newtype WriterT w m a = WriterT (RWSET Unit w Unit Void m a)

-- | Create a `Writer` monad from a pair of a result and an accumulator.
writerT :: forall w m a. Functor m => Monoid w => m (a /\ w) -> WriterT w m a
writerT k = WriterT
  ( RWSET
      ( mkFn6 \_ _ more lift _ done ->
          more \_ -> lift $ k <#> \(a /\ w) _ ->
            runFn3 done unit a w
      )
  )

-- | Runs a computation inside of `WriterT`.
runWriterT :: forall w m a. Monoid w => MonadRec m => WriterT w m a -> m (a /\ w)
runWriterT (WriterT k) = go <$> runRWSET unit unit k
  where
  go (_ /\ ea /\ w) =
    case ea of
      Left e ->
        absurd e
      Right a ->
        a /\ w

-- | Runs a computation inside of `WriterT`, discarding the final accumulator.
evalWriterT :: forall w m a. Monoid w => MonadRec m => WriterT w m a -> m a
evalWriterT k = fst <$> runWriterT k

-- | Runs a computation inside of `WriterT`, discarding the final result.
execWriterT :: forall w m a. Monoid w => MonadRec m => WriterT w m a -> m w
execWriterT k = snd <$> runWriterT k

-- | Modifies the monadic context of a `WriterT`.
hoistWriterT :: forall w m n a. (m ~> n) -> WriterT w m a -> WriterT w n a
hoistWriterT f (WriterT k) = WriterT (hoistRWSET f k)

-- | Modifies the result and accumulator types of a `WriterT`.
mapWriterT
  :: forall w1 w2 m1 m2 a1 a2
   . Monoid w1
  => Monoid w2
  => MonadRec m1
  => Functor m2
  => (m1 (a1 /\ w1) -> m2 (a2 /\ w2))
  -> WriterT w1 m1 a1
  -> WriterT w2 m2 a2
mapWriterT f k = WriterT
  ( RWSET
      ( mkFn6 \_ _ _ lift' _ done ->
          lift' $ f (runWriterT k) <#> \(a /\ w) _ ->
            runFn3 done unit a w
      )
  )

derive newtype instance Monoid w => Functor (WriterT w m)
derive newtype instance Monoid w => Apply (WriterT w m)
derive newtype instance Monoid w => Applicative (WriterT w m)
derive newtype instance Monoid w => Bind (WriterT w m)
derive newtype instance Monoid w => Monad (WriterT w m)
derive newtype instance (Monoid w, MonadEffect m) => MonadEffect (WriterT w m)
derive newtype instance Monoid w => MonadRec (WriterT w m)
derive newtype instance Monoid w => MonadTell w (WriterT w m)
derive newtype instance Monoid w => MonadWriter w (WriterT w m)
derive newtype instance Monoid w => MonadTrans (WriterT w)
derive newtype instance (Monoid w, Semigroup a) => Semigroup (WriterT w m a)
derive newtype instance (Monoid w, Monoid a) => Monoid (WriterT w m a)
derive newtype instance Monoid w => Lazy (WriterT w m a)

instance (Monoid w, MonadAsk r m) => MonadAsk r (WriterT w m) where
  ask = lift ask

instance (Monoid w, MonadRec m, MonadReader r m) => MonadReader r (WriterT w m) where
  local f (WriterT k) = WriterT
    ( RWSET
        ( mkFn6 \environment state _ lift' error done ->
            lift' $ local f (runRWSET environment state k) <#> \(s /\ ea /\ w) _ ->
              case ea of
                Left e ->
                  runFn3 error s e w
                Right a ->
                  runFn3 done s a w
        )
    )

instance (Monoid w, MonadState s m) => MonadState s (WriterT w m) where
  state = lift <<< state

instance (Monoid w, MonadThrow e m) => MonadThrow e (WriterT w m) where
  throwError = lift <<< throwError

instance (Monoid w, MonadRec m, MonadError e m) => MonadError e (WriterT w m) where
  catchError (WriterT k) f = WriterT
    ( RWSET
        ( mkFn6 \environment state _ lift' error done ->
            lift' $
              catchError (runRWSET environment state k)
                ( \e ->
                    case f e of
                      WriterT fk ->
                        runRWSET environment state fk
                ) <#> \(s /\ ea /\ w) _ ->
                case ea of
                  Left e ->
                    runFn3 error s e w
                  Right a ->
                    runFn3 done s a w
        )
    )
