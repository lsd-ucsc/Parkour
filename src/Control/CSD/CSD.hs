{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}

module Control.CSD.CSD where

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
runCSD :: (forall f. (CSD f) => f a b) -> I Async a -> IO (I Async b)
runCSD f = runInterp (f @(Interp Async IO))

-- ** Distributed Semantics

data Located a where
  Self :: Async a -> Located a
  Peer :: Async () -> Loc -> Located a

inc :: (Monad m) => StateT Int m Int
inc = do { x <- get; put (x + 1); return x }

instance CSD (InterpT Located Http (StateT Id)) where
  perf act = InterpT $ \case
    (Self a) -> Self <$> lift (async (wait a >>= liftIO . act))
    (Peer a loc) -> return (Peer a loc)
  -- the following two cases are exactly the same as the centralized semantics
  seq f g = InterpT $ \a -> runInterpT f a >>= runInterpT g
  par  f g = InterpT $ \(a, b) -> (,) <$> runInterpT f a <*> runInterpT g b
  fork = InterpT $ \case
    (Self ab) -> (,) <$> (Self <$> lift (async (fst <$> wait ab))) <*> (Self <$> lift (async (snd <$> wait ab)))
    (Peer a loc) -> return (Peer a loc, Peer a loc)
  join = InterpT $ \case
    (Self a, Self b) -> Self <$> lift (async ((,) <$> wait a <*> wait b))
    (Self a, Peer _ loc) -> Self <$> do { x <- inc; lift (async ((,) <$> wait a <*> recv loc x)) }
    (Peer a loc, Self b) -> Peer <$> do { x <- inc; lift (async (wait a >> wait b >>= send loc x)) } <*> pure loc
    (Peer a loc, Peer _ _) -> inc >> return (Peer a loc) -- the right one is the incoming site

project :: (forall f. (CSD f) => f a b) -> I Located a -> Http (I Located b)
project f a = evalStateT (runInterpT (f @(InterpT Located Http (StateT Int))) a) 0
