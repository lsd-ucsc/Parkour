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

(>>>) :: (CSD f) => f a b -> f b c -> f a c
a >>> b = seqq a b

(***) :: (CSD f) => f a c -> f b d -> f (a, b) (c, d)
a *** b = par a b

-------------------------------------------------------------------------------
-- * Interpretation

instance (CSD (Tmap Async IO)) where
