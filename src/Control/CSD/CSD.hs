{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FunctionalDependencies #-}

module Control.CSD.CSD where

import Control.Concurrent.Async.Lifted
import Control.CSD.Site
import Control.CSD.Network
import Control.Monad.State hiding (join)

class (Perm f) => CSD f m | f -> m where
  -- Sequence Composition
  perf :: (a -> m b) -> f (Site a) (Site b)
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

(>>>) :: (CSD f m) => f a b -> f b c -> f a c
a >>> b = Control.CSD.CSD.seq a b -- there's a name conflict

(***) :: (CSD f m) => f a c -> f b d -> f (a, b) (c, d)
a *** b = par a b

---------------------------------------------------------------------------------------------------
-- * Interpretation

-- ** Centralized Semantics

instance (MonadAsync m) => CSD (Tmap Async m) m where
  perf act = Tmap $ \a -> async (wait a >>= act)
  seq f g  = Tmap $ \a -> runTmap f a >>= runTmap g
  par f g  = Tmap $ \(a, b) -> (,) <$> runTmap f a <*> runTmap g b
  fork     = Tmap $ \ab -> (,) <$> async (fst <$> wait ab) <*> async (snd <$> wait ab)
  join     = Tmap $ \(a, b) -> async ((,) <$> wait a <*> wait b)

runCSD :: Tmap Async IO a b  -> T Async a -> IO (T Async b)
runCSD = runTmap

-- ** Distributed Semantics

data Located a where
  Here  :: Async a -> Located a
  There :: Async () -> Url -> Located a

inc :: (Monad m) => StateT Int m Int
inc = do { x <- get; put (x + 1); return x }

instance (MonadAsync m, Network m) => CSD (Tmap Located (StateT Int m)) m where
  perf act = Tmap $ \case
    (Here a) -> Here <$> lift (async (wait a >>= act))
    (There a loc) -> return (There a loc)
  -- the following two cases are exactly the same as the centralized semantics, which might mean
  -- we can futher refactor out things
  seq f g = Tmap $ \a -> runTmap f a >>= runTmap g
  par  f g = Tmap $ \(a, b) -> (,) <$> runTmap f a <*> runTmap g b
  fork = Tmap $ \case
    (Here ab) -> (,) <$> (Here <$> lift (async (fst <$> wait ab))) <*> (Here <$> lift (async (snd <$> wait ab)))
    (There a loc) -> return (There a loc, There a loc)
  join = Tmap $ \case
    (Here a, Here b) -> Here <$> lift (async ((,) <$> wait a <*> wait b))
    (Here a, There _ loc) -> Here  <$> do { x <- inc; lift (async ((,) <$> wait a <*> recv loc x)) }
    (There a loc, Here b) -> There <$> do { x <- inc; lift (async (wait a >> wait b >>= send loc x)) } <*> pure loc
    (There a loc, There _ _) -> inc >> return (There a loc)

project :: forall m a b. (MonadAsync m, Network m) =>
  (forall f. (CSD f m) => f a b) -> T Located a -> m (T Located b)
project f x = evalStateT (runTmap (f @(Tmap Located (StateT Int m))) x) 0
