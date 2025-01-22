{-# LANGUAGE TypeFamilies #-}

module Control.CSD.CSD where

import Control.Concurrent.Async
import Control.CSD.Site

class (Perm f) => CSD f where
  -- Sequence Composition
  perf :: (a -> IO b) -> f (Site a) (Site b)
  seqq :: f a b -> f b c -> f a c -- there's name conclit

  -- Parallel Composition
  par :: f a c -> f b d -> f (a, b) (c, d)
  fork :: f (Site (a, b)) (Site a, Site b)
  join :: f (Site a, Site b) (Site (a, b))

  -- Directful fork/join
  forkL :: f (Site (a, b)) (Site a, Site b)
  forkL = fork

  forkR :: f (Site (a, b)) (Site a, Site b)
  forkR = fork

  joinL :: f (Site a, Site b) (Site (a, b))
  joinL = join

  joinR :: f (Site a, Site b) (Site (a, b))
  joinR = join

-- the following operators are named after arrow operators share the same behavior

infixr 1 >>>
infixr 3 ***

(>>>) :: (CSD f) => f a b -> f b c -> f a c
a >>> b = seqq a b

(***) :: (CSD f) => f a c -> f b d -> f (a, b) (c, d)
a *** b = par a b

---------------------------------------------------------------------------------------------------
-- * Interpretation

-- ** Centralized Semantics

instance (CSD (Tmap Async IO)) where
  perf act = Tmap $ \a -> async (wait a >>= act)
  seqq f g = Tmap $ \a -> runTmap f a >>= runTmap g
  par f g  = Tmap $ \(a, b) -> (,) <$> runTmap f a <*> runTmap g b
  fork     = Tmap $ \ab -> (,) <$> async (fst <$> wait ab) <*> async (snd <$> wait ab)
  join     = Tmap $ \(a, b) -> async ((,) <$> wait a <*> wait b)

-- I really want to type this `(CSD f) => f a b -> T Async a -> IO (T Async b)`
runCSD :: Tmap Async IO a b -> T Async a -> IO (T Async b)
runCSD = runTmap
