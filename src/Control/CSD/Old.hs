{-# LANGUAGE TypeFamilies #-}

module Control.CSD.Old where

import Control.Arrow
import Control.Concurrent.Async
import Data.Kind
import Data.Tuple

infix 0 ≃
infixr 1 >>>
infixr 3 ***

-------------------------------------------------------------------------------
-- * Site Configurations

data Local (a :: Type)

-- Configuration equality
data a ≃ b where
  Swap :: (a, b) ≃ (b, a)
  AssocL :: (a, (b, c)) ≃ ((a, b), c)
  AssocR :: ((a, b), c) ≃ (a, (b, c))
  CongL :: a ≃ b -> (ctx, a) ≃ (ctx, b)
  CongR :: a ≃ b -> (a, ctx) ≃ (b, ctx)
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
  JoinL :: (Show a, Read a) => CSD f (Local a, Local b) (Local (a, b))
  JoinR :: (Show b, Read b) => CSD f (Local a, Local b) (Local (a, b))
  Perm :: a ≃ b -> CSD f a b

  -- Conditional execution
  Splt :: CSD f (Local (Either a b)) (Either (Local a) (Local b))
  -- this Ntfy is overly constrained
  Ntfy :: CSD f (Either (Local a) (Local b), Local c) (Either (Local a, Local c) (Local b, Local c))
  Brch :: CSD f a c -> CSD f b d -> CSD f (Either a b) (Either c d)
  Idem :: CSD f (Either a a) a

noop :: (Arrow f) => CSD f (Local a) (Local a)
noop = Perf (arr id)

perf :: (Monad m) => (a -> m b) -> CSD (Kleisli m) (Local a) (Local b)
perf f = Perf (Kleisli f)

-- the following operators are named after arrow operators share the same behavior

(>>>) :: CSD f a b -> CSD f b c -> CSD f a c
a >>> b = Seq a b

(***) :: CSD f a c -> CSD f b d -> CSD f (a, b) (c, d)
a *** b = Par a b

(+++) :: CSD f a c -> CSD f b d -> CSD f (Either a b) (Either c d)
a +++ b = Brch a b

-------------------------------------------------------------------------------
-- An interpreations to `Async`

-- There should be a more general way to interpret CSDs, and the `Async`ed
-- interpretation is an instance of it

type family Asynced cfg where
  Asynced (a, b) = (Asynced a, Asynced b)
  Asynced (Either a b) = (Either (Asynced a) (Asynced b))
  Asynced (Local a) = Async a

swapAsynced :: Async (a, b) -> IO (Async (b, a))
swapAsynced ab = async (swap <$> wait ab)

interpAsynced :: (forall c d. f c d -> c -> IO d) -> CSD f a b -> Asynced a -> IO (Asynced b)
interpAsynced hdl (Perf eff) a =
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
interpAsynced _ JoinL (a, b) = do
  async $ do
    a' <- wait a
    b' <- wait b
    return (a', b')
interpAsynced hdl JoinR (a, b) = do
  ba <- interpAsynced hdl JoinL (b, a)
  swapAsynced ba
interpAsynced _ Splt input = do
  i <- wait input
  case i of
    (Left x) -> Left <$> async (return x)
    (Right y) -> Right <$> async (return y)
interpAsynced _ Ntfy (input1, input2) = do
  case input1 of
    (Left x)  -> return (Left (x, input2))
    (Right y) -> return (Right (y, input2))
interpAsynced hdl (Brch f g) input = do
  case input of
    (Left x) -> Left <$> interpAsynced hdl f x
    (Right y) -> Right <$> interpAsynced hdl g y
interpAsynced _ Idem (Left a) = return a
interpAsynced _ Idem (Right a) = return a
-- structural rules
interpAsynced _ (Perm Swap) (a, b) = return (b, a)
interpAsynced hdl (Perm (CongL e)) (ctx, a) = (ctx,) <$> interpAsynced hdl (Perm e) a
interpAsynced hdl (Perm (CongR e)) (a, ctx) = (,ctx) <$> interpAsynced hdl (Perm e) a
interpAsynced _ (Perm AssocL) (a, (b, c)) = return ((a, b), c)
interpAsynced _ (Perm AssocR) ((a, b), c) = return (a, (b, c))
interpAsynced hdl (Perm (Trans e1 e2)) input = interpAsynced hdl (Perm e1) input >>= interpAsynced hdl (Perm e2)

-------------------------------------------------------------------------------
-- Projection

-- Backend function
type Addr = String

send :: (Show a) => Addr -> a -> IO ()
send addr a = putStrLn (">>> send " ++ show a ++ " to " ++ addr)

recv :: (Read a) => Addr -> IO a
recv addr = putStrLn (">>> recv from " ++ addr) >> (read <$> getLine)

-- Site Selector (coincidentally, backend configuration)
data Selector a where
  Self :: Selector (Local a)
  Peer :: Addr -> Selector (Local a)
  Conj :: Selector a -> Selector b -> Selector (a, b)
  Disj :: Selector a -> Selector b -> Selector (Either a b)

empty :: a
empty = error "the value is elsewhere"

project :: CSD f a b -> Selector a -> (forall x y. f x y -> x -> IO y) -> Asynced a -> IO (Selector b, Asynced b)
-- Perf
project (Perf eff) Self hdl a = do
  b <- async $ do
    a' <- wait a
    hdl eff a'
  return (Self, b)
project (Perf _) (Peer addr) _ _ =
  return (Peer addr, empty)
-- Seq
project (Seq f g) s hdl a = do
  (s', b) <- project f s hdl a
  project g s' hdl b
-- Par
project (Par f g) (Conj s1 s2) hdl (a, b) = do
  (s1', c) <- project f s1 hdl a
  (s2', d) <- project g s2 hdl b
  return (Conj s1' s2', (c, d))
-- Fork
project Fork Self _ ab = do
  a' <- async $ do
    (a, _) <- wait ab
    return a
  b' <- async $ do
    (_, b) <- wait ab
    return b
  return (Conj Self Self, (a', b'))
project Fork (Peer addr) _ _ = do
  return (Conj (Peer addr) (Peer addr), (empty, empty))
-- Join
project JoinL (Conj Self Self) _ (a, b) = do
  ab <- async $ do
    a' <- wait a
    b' <- wait b
    return (a', b')
  return (Self, ab)
project JoinL (Conj Self (Peer addr)) _ (a, _) = do
  putStrLn "123"
  _ <- async $ do
    a' <- wait a
    send addr a'
    return empty
  return (Peer addr, empty)
project JoinL (Conj (Peer addr) Self) _ (_, b) = do
  ab <- async $ do
    b' <- wait b
    a' <- recv addr
    return (a', b')
  return (Self, ab)
project JoinL (Conj (Peer _) (Peer addr2)) _ _ = do
  return (Peer addr2, empty)
project JoinR (Conj s1 s2) hdl (a, b) = do
  (x, y) <- project JoinL (Conj s2 s1) hdl (b, a)
  case x of
    Self -> do
      t <- async (swap <$> wait y)
      return (Self, t)
    (Peer addr) -> return (Peer addr, empty)
-- Perm
project (Perm Swap) (Conj s1 s2) _ (a, b) = return (Conj s2 s1, (b, a))
project (Perm AssocL) (Conj s1 (Conj s2 s3)) _ (a, (b, c)) = return (Conj (Conj s1 s2) s3, ((a, b), c))
project (Perm AssocR) (Conj (Conj s1 s2) s3) _ ((a, b), c) = return (Conj s1 (Conj s2 s3), (a, (b, c)))
project (Perm (CongL x)) (Conj s1 s2) hdl (ctx, a) = (\(u, v) -> (Conj s1 u, (ctx, v))) <$> project (Perm x) s2 hdl a
project (Perm (CongR x)) (Conj s1 s2) hdl (a, ctx) = (\(u, v) -> (Conj u s2, (v, ctx))) <$> project (Perm x) s1 hdl a
project (Perm (Trans x y)) s hdl a = do
  (s', b) <- project (Perm x) s hdl a
  project (Perm y) s' hdl b
-- Splt
project Splt Self _ a = do
  a' <- wait a -- does this add extra blocking?
  case a' of
    (Left x) -> (\u -> (Disj Self Self, Left u)) <$> async (return x)
    (Right y) -> (\u -> (Disj Self Self , Right u)) <$> async (return y)
project Splt (Peer addr) _ _ = return (Disj (Peer addr) (Peer addr), empty)
-- Ntfy
project Ntfy (Conj (Disj Self Self) Self) _ (ab, c) =
  return (Disj (Conj Self Self) (Conj Self Self), either (Left . (,c)) (Right . (,c)) ab)
project Ntfy (Conj (Disj Self (Peer _)) Self) _ _ = undefined
project Ntfy (Conj (Disj (Peer _) Self) Self) _ _ = undefined
project Ntfy (Conj (Disj (Peer addr1) (Peer addr2)) Self) _ (_, c) = do
  (ab :: Either () ()) <- recv addr1
  return (Disj (Conj (Peer addr1) Self) (Conj (Peer addr2) Self), either (Left . (,c) . const empty) (Right . (,c) . const empty) ab)
project Ntfy (Conj (Disj Self Self) (Peer addr)) _ (ab, _) = do
  send addr (either (const ()) (const ()) ab)
  return (Disj (Conj Self (Peer addr)) (Conj Self (Peer addr)), either (Left . (,empty)) (Right . (,empty)) ab)
project Ntfy (Conj (Disj Self (Peer _)) (Peer _)) _ _ = undefined
project Ntfy (Conj (Disj (Peer _) Self) (Peer _)) _ _ = undefined
project Ntfy (Conj (Disj (Peer addr1) (Peer addr2)) (Peer addr3)) _ (ab, _) =
  return (Disj (Conj (Peer addr1) (Peer addr3)) (Conj (Peer addr2) (Peer addr3)), either (Left . (,empty)) (Right . (,empty)) ab)
-- Brch
project (Brch f g) (Disj s1 s2) hdl ab =
  case ab of
    (Left a) -> (\(u, v) -> (Disj u empty, Left v)) <$> project f s1 hdl a
    (Right b) -> (\(u, v) -> (Disj empty u, Right v)) <$> project g s2 hdl b
-- Idem
project Idem (Disj s1 _) _ (Left x) = return (s1, x)
project Idem (Disj _ s2) _ (Right y) = return (s2, y)

-- data Self
-- data Skip

-- type family Projected i a where
--   Projected (ia, ib) (a, b) = (Projected ia a, Projected ib b)
--   Projected (Either ia ib) (Either a b) = Either (Projected ia a) (Projected ib b)
--   Projected Skip a = ()
--   Projected Target (Local a) = Async a

-- class Project t a t' b where
--   project :: (forall a b. f a b -> a -> IO b) -> CSD f a b -> Projected t a -> IO (Projected t' b)

-- instance Project Skip (Local a) Skip b where
--   project _ (Perf _) _ = return ()
--   project _ (Seq _ _) _ = return ()
--   project _ Fork _ = return ()
--   project _ (Perm _) _ = return ()
--   project _ Splt _ = return ()

-- instance Project Target (Local a) t' b where
--   project hdl (Perf f) a =
--     async $ do
--       a' <- wait a
--       hdl f a'
--   project hdl (Seq @x f g) a = undefined
--   project hdl Fork a = _
