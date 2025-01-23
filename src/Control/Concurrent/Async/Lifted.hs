module Control.Concurrent.Async.Lifted
  ( Async
  , MonadAsync(async, wait)
  )
  where

import Control.Concurrent.Async (Async)
import Control.Concurrent.Async qualified as Original
import Control.Monad.Reader

class (Monad m) => MonadAsync m where
  async :: m a -> m (Async a)
  wait :: Async a -> m a

instance MonadAsync IO where
  async = Original.async
  wait = Original.wait

instance (MonadAsync m) => MonadAsync (ReaderT r m) where
  async f = ReaderT $ \r -> async (runReaderT f r)
  wait a = ReaderT $ \_ -> wait a
