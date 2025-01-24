{-# LANGUAGE TypeFamilies #-}

module Control.CSD.Site where

import Data.Kind

data Site (a :: Type)

class WellFormed a
instance WellFormed (Site a)
instance (WellFormed a, WellFormed b) => WellFormed (a, b)
-- instance (WellFormed a, WellFormed b) => WellFormed (Either a b)

class Perm f where
  noop   :: f a a
  swap   :: f (a, b) (b, a)
  assocL :: f (a, (b, c)) ((a, b), c)
  assocR :: f ((a, b), c) (a, (b, c))
  congL  :: f a b -> f (ctx, a) (ctx, b)
  congR  :: f a b -> f (a, ctx) (b, ctx)
  trans  :: f a b -> f b c -> f a c

-- interpretations of a site configuration
type family I i a where
  I i (Site a) = i a
  I i (a, b)   = (I i a, I i b)

-- interpretations of a CSD
newtype Interp i m a b = Interp { runInterp :: I i a -> m (I i b) }
newtype InterpT i m t a b = InterpT { runInterpT :: I i a -> t m (I i b) }

-- TODO: is it possible to de-duplicate the following code?

instance (Monad m) => (Perm (Interp i m)) where
  noop      = Interp $ \a -> return a
  swap      = Interp $ \(a, b) -> return (b, a)
  assocL    = Interp $ \(a, (b, c)) -> return ((a, b), c)
  assocR    = Interp $ \((a, b), c) -> return (a, (b, c))
  congL f   = Interp $ \(ctx, a) -> (ctx,) <$> runInterp f a
  congR f   = Interp $ \(a, ctx) -> (,ctx) <$> runInterp f a
  trans f g = Interp $ \a -> runInterp f a >>= runInterp g

instance (Monad (t m)) => (Perm (InterpT i m t)) where
  noop      = InterpT $ \a -> return a
  swap      = InterpT $ \(a, b) -> return (b, a)
  assocL    = InterpT $ \(a, (b, c)) -> return ((a, b), c)
  assocR    = InterpT $ \((a, b), c) -> return (a, (b, c))
  congL f   = InterpT $ \(ctx, a) -> (ctx,) <$> runInterpT f a
  congR f   = InterpT $ \(a, ctx) -> (,ctx) <$> runInterpT f a
  trans f g = InterpT $ \a -> runInterpT f a >>= runInterpT g
