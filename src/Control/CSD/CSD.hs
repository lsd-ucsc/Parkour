{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}

module Control.CSD.CSD where

import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Control.Concurrent.Async.Lifted
import Control.CSD.Site
import Control.CSD.Network
import Control.Monad.State hiding (join)

class (Perm f) => CSD f where

  -- Sequence Composition
  perf :: (a -> IO b) -> f (Site a) (Site b)
  seq :: f a b -> f b c -> f a c

  -- Parallel Composition
  par :: f a c -> f b d -> f (a, b) (c, d)
  -- fork/join are right-biased, meaning the right one is the outcoming/incoming site. In some
  -- cases, which one is the outcoming/incoming site has no effect on the semantics, but if it
  -- does, consider use directful join/fork and give them specific semantics rather than using
  -- the default one.
  fork :: f (Site (a, b)) (Site a, Site b)
  join :: (Show b, Read b) => f (Site a, Site b) (Site (a, b))

  -- Directful fork/join
  forkL :: f (Site (a, out)) (Site out, Site a)
  forkL = fork >>> swap

  forkR :: f (Site (a, out)) (Site a, Site out)
  forkR = fork

  joinL :: (Show in', Read in') => f (Site in', Site b) (Site (b, in'))
  joinL = swap >>> join

  joinR :: (Show in', Read in') => f (Site a, Site in') (Site (a, in'))
  joinR = join

-- the following operators are named after arrow operators share the same behavior

infixr 1 >>>
infixr 3 ***

(>>>) :: (CSD f) => f a b -> f b c -> f a c
a >>> b = Control.CSD.CSD.seq a b -- there's a name conflict

(***) :: (CSD f) => f a c -> f b d -> f (a, b) (c, d)
a *** b = par a b

---------------------------------------------------------------------------------------------------
-- * Interpretation

-- ** Centralized Semantics

instance CSD (Interp Async IO) where
  perf act = Interp $ \a -> async (wait a >>= act)
  seq f g  = Interp $ \a -> runInterp f a >>= runInterp g
  par f g  = Interp $ \(a, b) -> (,) <$> runInterp f a <*> runInterp g b
  fork     = Interp $ \ab -> (,) <$> async (fst <$> wait ab) <*> async (snd <$> wait ab)
  join     = Interp $ \(a, b) -> async ((,) <$> wait a <*> wait b)

-- a special case of the above instance
runCSD :: Interp Async IO a b -> I Async a -> IO (I Async b)
runCSD = runInterp

-- ** Distributed Semantics

data Located a where
  Self :: Loc -> Async a -> Located a
  Peer :: Loc -> Located a

inc :: (Monad m) => Loc -> StateT (HashMap Loc Id) m Id
inc loc = do
  m <- get
  let v = HM.findWithDefault 0 loc m
  put (HM.insert loc (v + 1) m)
  return v

instance CSD (InterpT Located Http (StateT (HashMap Loc Id))) where
  perf act = InterpT $ \case
    (Self loc a) -> do
      a <- lift (async (wait a >>= act))
      return (Self loc a)
    (Peer loc) -> return (Peer loc)

  -- the following two cases are exactly the same as the centralized semantics
  seq f g = InterpT $ \a -> runInterpT f a >>= runInterpT g
  par  f g = InterpT $ \(a, b) -> (,) <$> runInterpT f a <*> runInterpT g b

  fork = InterpT $ \case
    (Self loc ab) -> do
      a <- lift (async (fst <$> wait ab))
      b <- lift (async (snd <$> wait ab))
      return (Self loc a, Self loc b)
    (Peer loc) -> return (Peer loc, Peer loc)

  -- remember join is left biased
  join = InterpT $ \case
    (Self loc a, Self _ b) -> do
      ab <- async ((,) <$> wait a <*> wait b)
      return (Self loc ab)
    (Self dst a, Peer src) -> do
      id <- inc src
      b <- lift (recv src id)
      ab <- async ((,) <$> wait a <*> wait b)
      return (Self dst ab)
    (Peer dst, Self src b) -> do
      id <- inc dst
      lift (send' dst src id b) -- there's a dangling Async here
      return (Peer dst)
    (Peer a, Peer _) -> return (Peer a)

project :: InterpT Located Http (StateT (HashMap Loc Id)) a b -> I Located a -> Http (I Located b)
project f a = evalStateT (runInterpT f a) HM.empty
