module Cps.StateT where

import Prelude

import Control.Lazy (class Lazy)
import Control.Monad.Error.Class (class MonadError, class MonadThrow, catchError, throwError)
import Control.Monad.Trans.Class (class MonadTrans, lift)
import Control.Monad.Reader.Class (class MonadAsk, class MonadReader, ask, local)
import Control.Monad.State.Class (class MonadState)
import Control.Monad.Rec.Class (class MonadRec)
import Control.Monad.Writer.Class (class MonadTell, class MonadWriter, listen, pass, tell)
import Cps.RWSET (RWSET(..), hoistRWSET, runRWSET)
import Data.Either (Either(..))
import Data.Function.Uncurried (mkFn6, runFn3, runFn6)
import Data.Tuple (fst, snd)
import Data.Tuple.Nested (type (/\), (/\))
import Effect.Class (class MonadEffect)

newtype StateT s m a = StateT (RWSET Unit Unit s Void m a)

runStateT :: forall s m a. MonadRec m => s -> StateT s m a -> m (a /\ s)
runStateT s (StateT k) = go <$> runRWSET unit s k
  where
  go (s' /\ ea /\ _) =
    case ea of
      Left e ->
        absurd e
      Right a ->
        (a /\ s')

evalStateT :: forall s m a. MonadRec m => s -> StateT s m a -> m a
evalStateT s k = map fst $ runStateT s k

execStateT :: forall s m a. MonadRec m => s -> StateT s m a -> m s
execStateT s k = map snd $ runStateT s k

hoistStateT :: forall s m n a. (m ~> n) -> StateT s m a -> StateT s n a
hoistStateT f (StateT k) = StateT (hoistRWSET f k)

mapStateT
  :: forall s m1 m2 a1 a2
   . MonadRec m1
  => Functor m2
  => (m1 (a1 /\ s) -> m2 (a2 /\ s))
  -> StateT s m1 a1
  -> StateT s m2 a2
mapStateT f k = StateT
  ( RWSET
    ( mkFn6 \_ state _ lift' _ done ->
        lift' $ f (runStateT state k) <#> \(a /\ s) _ ->
          runFn3 done s a unit
    )
  )

withStateT
  :: forall s m a
   . (s -> s)
  -> StateT s m a
  -> StateT s m a
withStateT f (StateT (RWSET k)) = StateT
  ( RWSET
    ( mkFn6 \environment state more lift' error done ->
        runFn6 k environment (f state) more lift' error done
    )
  )

derive newtype instance Functor (StateT s m)
derive newtype instance Apply (StateT s m)
derive newtype instance Applicative (StateT s m)
derive newtype instance Bind (StateT s m)
derive newtype instance Monad (StateT s m)
derive newtype instance MonadEffect m => MonadEffect (StateT s m)
derive newtype instance MonadRec (StateT s m)
derive newtype instance MonadState s (StateT s m)
derive newtype instance MonadTrans (StateT s)
derive newtype instance Semigroup a => Semigroup (StateT s m a)
derive newtype instance Monoid a => Monoid (StateT s m a)
derive newtype instance Lazy (StateT s m a)

instance MonadAsk r m => MonadAsk r (StateT s m) where
  ask = lift ask

instance (MonadRec m, MonadReader r m) => MonadReader r (StateT s m) where
  local f (StateT k) = StateT
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

instance (Monoid w, MonadTell w m) => MonadTell w (StateT s m) where
  tell = lift <<< tell

instance (Monoid w, MonadRec m, MonadWriter w m) => MonadWriter w (StateT s m) where
  listen (StateT k) = StateT
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

  pass (StateT kf) = StateT
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


instance MonadThrow e m => MonadThrow e (StateT s m) where
  throwError = lift <<< throwError

instance (MonadRec m, MonadError e m) => MonadError e (StateT s m) where
  catchError (StateT k) f = StateT
    ( RWSET
      ( mkFn6 \environment state _ lift' error done ->
          lift' $ catchError (runRWSET environment state k)
            ( \e ->
                case f e of
                  StateT fk ->
                    runRWSET environment state fk
            ) <#> \(s /\ ea /\ w) _ ->
                case ea of
                  Left e ->
                    runFn3 error s e w
                  Right a ->
                    runFn3 done s a w
      )
    )
