{-# LANGUAGE TypeFamilies #-}

module Control.CSD where

import Control.Concurrent.Async
import Data.Kind
import Control.Arrow

infix 0 ≃
infixr 1 >>>
infixr 3 ***

-------------------------------------------------------------------------------
-- * Site Configurations

data Local (a :: Type)

-- Configuration equality
data a ≃ b where
  -- conjunction rules
  Swap :: (a, b) ≃ (b, a)
  AssocL :: (a, (b, c)) ≃ ((a, b), c)
  AssocR :: ((a, b), c) ≃ (a, (b, c))
  CongL :: a ≃ b -> (ctx, a) ≃ (ctx, b)
  CongR :: a ≃ b -> (a, ctx) ≃ (b, ctx)

  -- Disjunction rules
  Idem :: Either a a ≃ a

  Trans :: a ≃ b -> b ≃ c -> a ≃ c

-------------------------------------------------------------------------------
-- * CSDs

data CSD f a b where
  -- sequence composition
  Perf :: f a b -> CSD f (Local a) (Local b)
  Seq :: CSD f a b -> CSD f b c -> CSD f a c

  -- parallel composition
  Par :: CSD f a c -> CSD f b d -> CSD f (a, b) (c, d)
  Fork :: CSD f (Local (a, b)) (Local a, Local b)
  Join :: CSD f (Local a, Local b) (Local (a, b))
  Perm :: a ≃ b -> CSD f a b

  -- Conditional execution
  Split :: CSD f (Local (Either a b)) (Either (Local a) (Local b))
  Notify :: CSD f (Either a b, c) (Either (a, c) (b, c))
  Branch :: CSD f a c -> CSD f b d -> CSD f (Either a b) (Either c d)

noop :: (Arrow f) => CSD f (Local a) (Local a)
noop = Perf (arr id)

perf :: (Monad m) => (a -> m b) -> CSD (Kleisli m) (Local a) (Local b)
perf f = Perf (Kleisli f)

-- the following operators are named after arrow operators share the same behavior

(>>>) :: CSD f a b -> CSD f b c -> CSD f a c
a >>> b = Seq a b

(***) :: CSD f a c -> CSD f b d -> CSD f (a, b) (c, d)
a *** b = Par a b

-------------------------------------------------------------------------------
-- An interpreations to `Async`

-- There should be a more general way to interpret CSDs, and the `Async`ed
-- interpretation is an instance of it

type family Asynced cfg where
  Asynced (a, b) = (Asynced a, Asynced b)
  Asynced (Either a b) = (Either (Asynced a) (Asynced b))
  Asynced (Local a) = Async a

interpAsynced :: (forall a b. f a b -> a -> IO b) -> CSD f a b -> Asynced a -> IO (Asynced b)
interpAsynced hdl (Perf eff) a = do
  async $ do
    a' <- wait a
    hdl eff a'
interpAsynced hdl (Seq f g) a = do
  b <- interpAsynced hdl f a
  interpAsynced hdl g b
interpAsynced hdl (Par f g) (a, b) = do
  c <- interpAsynced hdl f a
  d <- interpAsynced hdl g b
  return (c, d)
interpAsynced _ Fork ab = do
  a' <- async $ do
    (a, _) <- wait ab
    return a
  b' <- async $ do
    (_, b) <- wait ab
    return b
  return (a', b')
interpAsynced _ Join (a, b) = do
  async $ do
    a' <- wait a
    b' <- wait b
    return (a', b')
interpAsynced _ Split input = do
  i <- wait input
  case i of
    (Left x) -> Left <$> async (return x)
    (Right y) -> Right <$> async (return y)
interpAsynced _ Notify (input1, input2) = do
  case input1 of
    (Left x)  -> return (Left (x, input2))
    (Right y) -> return (Right (y, input2))
interpAsynced hdl (Branch f g) input = do
  case input of
    (Left x) -> Left <$> interpAsynced hdl f x
    (Right y) -> Right <$> interpAsynced hdl g y
-- structural rules
interpAsynced _ (Perm Swap) (a, b) = return (b, a)
interpAsynced hdl (Perm (CongL e)) (ctx, a) = (ctx,) <$> interpAsynced hdl (Perm e) a
interpAsynced hdl (Perm (CongR e)) (a, ctx) = (,ctx) <$> interpAsynced hdl (Perm e) a
interpAsynced _ (Perm AssocL) (a, (b, c)) = return ((a, b), c)
interpAsynced _ (Perm AssocR) ((a, b), c) = return (a, (b, c))
interpAsynced _ (Perm Idem) (Left a) = return a
interpAsynced _ (Perm Idem) (Right a) = return a
interpAsynced hdl (Perm (Trans e1 e2)) input = interpAsynced hdl (Perm e1) input >>= interpAsynced hdl (Perm e2)
