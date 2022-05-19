-- | This module defines the reader monad transformer, `ReaderT`.
module Uncurried.ReaderT where

import Prelude

import Control.Lazy (class Lazy)
import Control.Monad.Error.Class (class MonadError, class MonadThrow, catchError, throwError)
import Control.Monad.Reader.Class (class MonadAsk, class MonadReader)
import Control.Monad.Rec.Class (class MonadRec)
import Control.Monad.State.Class (class MonadState, state)
import Control.Monad.Trans.Class (class MonadTrans, lift)
import Control.Monad.Writer.Class (class MonadTell, class MonadWriter, listen, pass, tell)
import Data.Either (Either(..))
import Data.Function.Uncurried (mkFn6, runFn3, runFn6)
import Data.Tuple.Nested ((/\))
import Effect.Class (class MonadEffect)
import Uncurried.RWSET (RWSET(..), hoistRWSET, runRWSET)

-- | The reader monad transformer, implemented as a newtytpe over
-- | `RWSET`. Note that it's not recommended to stack newtypes of
-- | `RWSET` together as it incurs an indeterminate performance
-- | penalty that would otherwise be solved by just using `RWSET`.
newtype ReaderT r m a = ReaderT (RWSET r Unit Unit Void m a)

-- | Construct a `ReaderT` given a function that requires some context.
readerT :: forall r m a. Functor m => (r -> m a) -> ReaderT r m a
readerT k = ReaderT
  ( RWSET
      ( mkFn6 \environment _ more lift _ done ->
          more \_ -> lift $ k environment <#> \a _ ->
            runFn3 done unit a unit
      )
  )

-- | Runs a computation inside of `ReaderT`.
runReaderT :: forall r m a. MonadRec m => r -> ReaderT r m a -> m a
runReaderT r (ReaderT k) = go <$> runRWSET r unit k
  where
  go (_ /\ ea /\ _) =
    case ea of
      Left e ->
        absurd e
      Right a ->
        a

-- | Modifies the monadic context of a `ReaderT`.
hoistReaderT :: forall r m n a. (m ~> n) -> ReaderT r m a -> ReaderT r n a
hoistReaderT f (ReaderT k) = ReaderT (hoistRWSET f k)

-- | Modifies the result type of a `ReaderT`.
mapReaderT
  :: forall r m1 m2 a1 a2
   . MonadRec m1
  => Functor m2
  => (m1 a1 -> m2 a2)
  -> ReaderT r m1 a1
  -> ReaderT r m2 a2
mapReaderT f k = ReaderT
  ( RWSET
      ( mkFn6 \environment _ _ lift' _ done ->
          lift' $ f (runReaderT environment k) <#> \a _ ->
            runFn3 done unit a unit
      )
  )

-- | Modifies the environment type of a `ReaderT`.
withReaderT
  :: forall r1 r2 m a
   . (r2 -> r1)
  -> ReaderT r1 m a
  -> ReaderT r2 m a
withReaderT f (ReaderT (RWSET k)) = ReaderT
  ( RWSET
      ( mkFn6 \environment state more lift' error done ->
          runFn6 k (f environment) state more lift' error done
      )
  )

derive newtype instance Functor (ReaderT r m)
derive newtype instance Apply (ReaderT r m)
derive newtype instance Applicative (ReaderT r m)
derive newtype instance Bind (ReaderT r m)
derive newtype instance Monad (ReaderT r m)
derive newtype instance MonadEffect m => MonadEffect (ReaderT r m)
derive newtype instance MonadRec (ReaderT r m)
derive newtype instance MonadAsk r (ReaderT r m)
derive newtype instance MonadReader r (ReaderT r m)
derive newtype instance MonadTrans (ReaderT r)
derive newtype instance Semigroup a => Semigroup (ReaderT r m a)
derive newtype instance Monoid a => Monoid (ReaderT r m a)
derive newtype instance Lazy (ReaderT r m a)

instance (Monoid w, MonadTell w m) => MonadTell w (ReaderT r m) where
  tell = lift <<< tell

instance (Monoid w, MonadRec m, MonadWriter w m) => MonadWriter w (ReaderT r m) where
  listen (ReaderT k) = ReaderT
    ( RWSET
        ( mkFn6 \environment state _ lift' error done -> do
            lift' $ (listen $ runRWSET environment state k) <#> \((s /\ ea /\ _) /\ w) _ ->
              case ea of
                Left e ->
                  runFn3 error s e unit
                Right a ->
                  runFn3 done s (a /\ w) unit
        )
    )

  pass (ReaderT kf) = ReaderT
    ( RWSET
        ( mkFn6 \environment state _ lift' error done -> do
            lift' $ pass $ runRWSET environment state kf <#> \(s /\ eaf /\ w) ->
              case eaf of
                Left e ->
                  (\_ -> runFn3 error s e w) /\ identity
                Right (a /\ f) ->
                  (\_ -> runFn3 done s a w) /\ f
        )
    )

instance MonadState s m => MonadState s (ReaderT r m) where
  state = lift <<< state

instance MonadThrow e m => MonadThrow e (ReaderT r m) where
  throwError = lift <<< throwError

instance (MonadRec m, MonadError e m) => MonadError e (ReaderT r m) where
  catchError (ReaderT k) f = ReaderT
    ( RWSET
        ( mkFn6 \environment state _ lift' error done ->
            lift' $
              catchError (runRWSET environment state k)
                ( \e ->
                    case f e of
                      ReaderT fk ->
                        runRWSET environment state fk
                ) <#> \(s /\ ea /\ w) _ ->
                case ea of
                  Left e ->
                    runFn3 error s e w
                  Right a ->
                    runFn3 done s a w
        )
    )
