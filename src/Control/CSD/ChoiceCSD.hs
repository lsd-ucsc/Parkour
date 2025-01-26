{-# LANGUAGE LambdaCase #-}

module Control.CSD.ChoiceCSD where

import Control.Concurrent.Async.Lifted
import Control.CSD.Network
import Control.CSD.Site
import Control.CSD.CSD
import Control.Monad.State hiding (join)

class (CSD f) => ChoiceCSD f where
  split :: f (Site (Either a b), Site c) (Either (Site a, Site c) (Site b, Site c))
--  split :: f (Site (Either a b), c) (Either (Site a, c) (Site b, c))
  branch :: f a c -> f b d -> f (Either a b) (Either c d)
  idem :: f (Either a a) a

---------------------------------------------------------------------------------------------------
-- * Interpretation

-- ** Centralized Semantics

instance ChoiceCSD (Interp Async IO) where
  split = Interp $ \(ab, c) -> do
    ab <- wait ab -- TODO: figure out is it ok to block here and the same below
    case ab of
      (Left a) -> Left . (,c) <$> async (return a)
      (Right b) -> Right . (,c) <$> async (return b)
  branch f g = Interp $ \case
    (Left a) -> Left <$> runInterp f a
    (Right b) -> Right <$> runInterp g b
  idem = Interp $ \case
    (Left a) -> return a
    (Right a) -> return a

-- ** Distributed Semantics

instance ChoiceCSD (InterpT Located Http (StateT Id)) where
  split = InterpT $ \case
    (Self ab, Self c) -> inc >>= \_ -> lift $ do
      ab <- wait ab -- TODO: figure out is it ok to block here
      case ab of
        (Left a) -> Left <$> ((,) <$> (Self <$> async (return a)) <*> pure (Self c))
        (Right b) -> Right <$> ((,) <$> (Self <$> async (return b)) <*> pure (Self c))
    (Self ab, Peer c loc) -> inc >>= \x -> lift $ do
      ab <- wait ab
      case ab of
        (Left a) -> do
          a <- async (return a)
          c <- async (send loc x True)
          return (Left (Self a, Peer c loc))
        (Right b) -> do
          b <- async (return b)
          c <- async (send loc x False)
          return (Right (Self b, Peer c loc))
    (Peer ab loc, Self c) -> inc >>= \x -> do
      cond <- lift (recv loc x)
      case cond of
        True -> return (Left (Peer ab loc, Self c))
        False -> return (Right (Peer ab loc, Self c))
    (Peer ab loc1, Peer c loc2) -> inc >> return (Left (Peer ab loc1, Peer c loc2)) -- Left or Right doesn't matter
  branch f g = InterpT $ \case
    (Left a) -> Left <$> runInterpT f a
    (Right b) -> Right <$> runInterpT g b
  idem = InterpT $ \case
    (Left a) -> return a
    (Right a) -> return a
