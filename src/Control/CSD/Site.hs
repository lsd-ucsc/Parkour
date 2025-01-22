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

type family T t a where
  T t (Site a) = t a
  T t (a, b)   = (T t a, T t b)

newtype Tmap t m a b = Tmap { runTmap :: T t a -> m (T t b) }

instance (Monad m) => (Perm (Tmap t m)) where
  swap      = Tmap $ \(a, b) -> return (b, a)
  assocL    = Tmap $ \(a, (b, c)) -> return ((a, b), c)
  assocR    = Tmap $ \((a, b), c) -> return (a, (b, c))
  congL f   = Tmap $ \(ctx, a) -> (ctx,) <$> runTmap f a
  congR f   = Tmap $ \(a, ctx) -> (,ctx) <$> runTmap f a
  trans f g = Tmap $ \a -> runTmap f a >>= runTmap g
