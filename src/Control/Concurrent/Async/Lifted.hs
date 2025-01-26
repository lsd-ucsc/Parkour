module Control.Concurrent.Async.Lifted
  ( Async
  , async
  , wait
  )
  where

import Control.Concurrent.Async (Async)
import Control.Concurrent.Async qualified as Original
import Control.Monad.IO.Class

async :: (MonadIO m) => IO a -> m (Async a)
async = liftIO . Original.async

wait :: (MonadIO m) => Async a -> m a
wait = liftIO . Original.wait
