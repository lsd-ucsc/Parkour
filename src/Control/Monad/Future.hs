-- | This module defines `MonadFuture`, a general interface for computations which supports
-- spawning futures (tasks running in parallel) and waiting for their results.
module Control.Monad.Future where

import Control.Concurrent.Async (Async)
import Control.Concurrent.Async qualified as Async
import Control.Monad.Reader

class (Monad m) => MonadFuture fut m where
  spawn :: m a -> m (fut a)
  wait :: fut a -> m a

-- some common instance(s)

instance MonadFuture Async IO where
  spawn = Async.async
  wait = Async.wait

instance (MonadFuture fut m) => MonadFuture fut (ReaderT r m) where
  spawn m = ReaderT $ \r -> spawn (runReaderT m r)
  wait fut = ReaderT $ \_ -> wait fut

-- TODO: define instances for `Par` and `ParIO`?
