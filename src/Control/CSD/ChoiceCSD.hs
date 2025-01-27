{-# LANGUAGE LambdaCase #-}

module Control.CSD.ChoiceCSD where

import Data.HashMap.Strict (HashMap)
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

instance ChoiceCSD (InterpT Located Http (StateT (HashMap Loc Id))) where
  split = InterpT $ \case
    (Self loc1 ab, Self loc2 c) -> lift $ do
      ab <- wait ab -- TODO: figure out is it ok to block here
      case ab of
        (Left a) -> Left <$> ((,) <$> (Self loc1 <$> async (return a)) <*> pure (Self loc2 c))
        (Right b) -> Right <$> ((,) <$> (Self loc1 <$> async (return b)) <*> pure (Self loc2 c))
    (Self src ab, Peer dst) -> inc dst >>= \id -> lift $ do
      ab <- wait ab
      case ab of
        (Left a) -> do
          a <- async (return a)
          c <- send dst src id True
          return (Left (Self src a, Peer dst))
        (Right b) -> do
          b <- async (return b)
          c <- send dst src id False
          return (Right (Self src b, Peer dst))
    (Peer src, Self dst c) -> inc src >>= \id -> lift $ do
      cond <- recv src id
      cond <- wait cond
      case cond of
        True -> return (Left (Peer src, Self dst c))
        False -> return (Right (Peer src, Self dst c))
    (Peer loc1, Peer loc2) -> return (Left (Peer loc1, Peer loc2)) -- Left or Right doesn't matter

  branch f g = InterpT $ \case
    (Left a) -> Left <$> runInterpT f a
    (Right b) -> Right <$> runInterpT g b

  idem = InterpT $ \case
    (Left a) -> return a
    (Right a) -> return a
