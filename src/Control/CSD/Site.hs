{-# LANGUAGE TypeFamilies #-}

module Control.CSD.Site where

import Data.Kind

data Site (a :: Type)

class WellFormed a
instance WellFormed (Site a)
instance (WellFormed a, WellFormed b) => WellFormed (a, b)
-- instance (WellFormed a, WellFormed b) => WellFormed (Either a b)

class Perm f where
  swap  :: f (a, b) (b, a)
  assocL :: f (a, (b, c)) ((a, b), c)
  assocR :: f ((a, b), c) (a, (b, c))
  congL  :: f a b -> f (ctx, a) (ctx, b)
  congR  :: f a b -> f (a, ctx) (b, ctx)
  trans  :: f a b -> f b c -> f a c

instance Perm (->) where
  swap      = \(a, b) -> (b, a)
  assocL    = \(a, (b, c)) -> ((a, b), c)
  assocR    = \((a, b), c) -> (a, (b, c))
  congL f   = \(ctx, a) -> (ctx, f a)
  congR f   = \(a, ctx) -> (f a, ctx)
  trans f g = \a -> g (f a)

type family T f a where
  T f (Site a) = f a
  T f (a, b)   = (T f a, T f b)

newtype Tmap f a b = Tmap { runTmap :: (T f a -> T f b) }

instance (Perm (Tmap f)) where
  swap      = Tmap $ \(a, b) -> (b, a)
  assocL    = Tmap $ \(a, (b, c)) -> ((a, b), c)
  assocR    = Tmap $ \((a, b), c) -> (a, (b, c))
  congL f   = Tmap $ \(ctx, a) -> (ctx, runTmap f a)
  congR f   = Tmap $ \(a, ctx) -> (runTmap f a, ctx)
  trans f g = Tmap $ \a -> runTmap g (runTmap f a)
