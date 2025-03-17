{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Control.CSD.CSD where

import Prelude hiding ((||))
import Data.Kind
import Data.Proxy
import Data.Typeable
import Control.Concurrent.Async.Lifted
import Control.Arrow
import Control.CSD.Network
    ( Http, LocTm, Network(recv, send', send) )
import Control.Monad.State hiding (join)

---------------------------------------------------------------------------------------------------
-- * Sites

infix  3 @
infixr 2 *
infixr 1 +

data (x :: Type) @ (l :: Type)
data (a :: Type) * (b ::Type)
data (a :: Type) + (b :: Type)

---------------------------------------------------------------------------------------------------
-- * Locations

reify :: forall l. (Typeable l) => LocTm
reify = show (typeRep (Proxy :: Proxy l))

eqLoc :: forall l l'. (Typeable l, Typeable l') => Bool
eqLoc = reify @l == reify @l'

---------------------------------------------------------------------------------------------------
-- * CSDs

infix 1 ≃

data a ≃ b where
  Id        :: a ≃ a
  Swap      :: a * b ≃ b * a
  AssocL    :: a * (b * c) ≃ (a * b) * c
  AssocR    :: (a * b) * c ≃ a * (b * c)
  Distrib   :: ((a + b) * c) ≃ (a * c + b * c)
  Undistrib :: (a * c + b * c) ≃ ((a + b) * c)

data CSD f a b where
  Perf :: (Typeable l) => f x y -> CSD f (x @ l) (y @ l)
  Comm :: (Typeable l', Typeable l, Show x, Read x) => CSD f (x @ l) (x @ l')
  Seq  :: CSD f a b -> CSD f b c -> CSD f a c
  Par  :: CSD f a c -> CSD f b d -> CSD f (a * b) (c * d)
  Fork :: (Typeable l) => CSD f ((x, y) @ l) (x @ l * y @ l)
  Join :: (Typeable l) => CSD f (x @ l * y @ l) ((x, y) @ l)
  Perm :: a ≃ b -> CSD f a b
  -- Conditionals
  Split    :: (Typeable l) => CSD f (Either x y @ l) (x @ l + y @ l)
  Coalesce :: (Typeable l) => CSD f (x @ l + y @ l) (Either x y @ l)
  Branch   :: CSD f a c -> CSD f b d -> CSD f (a + b) (c + d)

-- Derived operations and syntax sugar

-- perform an monadic action
perf :: (Typeable l) => (x -> m y) -> CSD (Kleisli m) (x @ l) (y @ l)
perf m = Perf (Kleisli m)

-- this version works nicer with |>~
noop :: CSD f (a @ l) (a @ l)
noop = Perm Id

infixr 1 ||>
infixr 1 |>
infixr 3 ||

(||>) :: CSD f a b -> CSD f b c -> CSD f a c
f ||> g = Seq f g

(||) :: CSD f a c -> CSD f b d -> CSD f (a * b) (c * d)
f || g = Par f g

(|>) :: (Normalizable b, Normalizable b', Norm b ~ Norm b') =>
        CSD f a b -> CSD f b' c -> CSD f a c
f |> g = f ||> toNorm ||> fromNorm ||> g

-- right-associative normal forms for configurations
type family Norm a where
  Norm (x @ l) = x @ l
  Norm (x @ l * c) = x @ l * Norm c
  Norm ((a * b) * c) = Norm a * Norm (b * c)
  Norm (a + b) = a + b
  Norm ((a + b) * c) = (a + b) * Norm c

class Normalizable a where
  fromNorm :: CSD f (Norm a) a
  toNorm :: CSD f a (Norm a)

instance Normalizable (x @ l) where
  fromNorm = Perm Id
  toNorm = Perm Id

instance (Normalizable c) => Normalizable (x @ l * c) where
  fromNorm = Par (Perm Id) fromNorm
  toNorm = Par (Perm Id) toNorm

instance (Normalizable a, Normalizable (b * c)) => Normalizable ((a * b) * c) where
  fromNorm = Par fromNorm fromNorm ||> Perm AssocL
  toNorm = Perm AssocR ||> Par toNorm toNorm

instance Normalizable (a + b) where
  fromNorm = Perm Id
  toNorm = Perm Id

instance (Normalizable c) => Normalizable ((a + b) * c) where
  fromNorm = Par (Perm Id) fromNorm
  toNorm = Par (Perm Id) toNorm

---------------------------------------------------------------------------------------------------
-- * Interpreations

type family Asynced a where
  Asynced (x @ l) = Async x
  Asynced (a * b) = (Asynced a, Asynced b)
  Asynced (a + b) = Either (Asynced a) (Asynced b)

type CentralF a b = Asynced a -> IO (Asynced b)

-- ** Centralized Semantics

runCSDPerm :: a ≃ b -> CentralF a b
runCSDPerm Id = \a -> return a
runCSDPerm Swap = \(a, b) -> return (b, a)
runCSDPerm AssocL = \(a, (b, c)) -> return ((a, b), c)
runCSDPerm AssocR = \((a, b), c) -> return (a, (b, c))
runCSDPerm Distrib = \(ab, c) -> return (either (Left . (,c)) (Right . (,c)) ab)
runCSDPerm Undistrib = \acbc -> return (either (\(a, c) -> (Left a, c)) (\(b, c) -> (Right b, c)) acbc)

runCSD :: (forall x y. f x y -> x -> IO y) -> CSD f a b -> CentralF a b
runCSD hdl (Perf act) = \x -> async (wait x >>= hdl act)
runCSD _   Comm       = \x -> return x
runCSD hdl (Seq f g)  = \a -> runCSD hdl f a >>= runCSD hdl g
runCSD hdl (Par f g)  = \(a, b) -> (,) <$> runCSD hdl f a <*> runCSD hdl g b
runCSD _   Fork       = \xy -> (,) <$> async (fst <$> wait xy) <*> async (snd <$> wait xy)
runCSD _   Join       = \(x, y) -> async ((,) <$> wait x <*> wait y)
runCSD _   (Perm p)   = runCSDPerm p
-- conditionals
runCSD _ Split = \xy -> do
  xy' <- wait xy
  case xy' of
    (Left x)  -> Left <$> async (return x)
    (Right y) -> Right <$> async (return y)
runCSD _ Coalesce = \case
  Left x -> async (Left <$> wait x)
  Right y -> async (Right <$> wait y)
runCSD hdl (Branch f g) = \case
  (Left a)  -> Left <$> runCSD hdl f a
  (Right b) -> Right <$> runCSD hdl g b

-- ** Distributed Semantics

type ProjectedF a b =
  forall (t :: Type). (Typeable t) => Proxy t -> Asynced a -> StateT Int Http (Asynced b)

absent :: Async a
absent = error "Trying to access a value that is elsewhere."

inc :: (Monad m) => StateT Int m Int
inc = do
  v <- get
  put (v + 1)
  return v

project1Perm :: a ≃ b -> ProjectedF a b
project1Perm Id _ = \a -> return a
project1Perm Swap _ = \(a, b) -> return (b, a)
project1Perm AssocL _ = \(a, (b, c)) -> return ((a, b), c)
project1Perm AssocR _ = \((a, b), c) -> return (a, (b, c))
project1Perm Distrib _ = \(ab, c) -> return (either (Left . (,c)) (Right . (,c)) ab)
project1Perm Undistrib _ = \acbc -> return (either (\(a, c) -> (Left a, c)) (\(b, c) -> (Right b, c)) acbc)

project1 :: (forall x y. f x y -> x -> IO y) -> CSD f a b -> ProjectedF a b
project1 hdl (Perf @l act) (_ :: Proxy t)
  | reify @l == reify @t = \x -> async (wait x >>= hdl act)
  | otherwise = \_ -> return absent
project1 _ (Comm @s @r) (_ :: Proxy t)
  | reify @s == reify @r = return
  | reify @t == reify @s = \x -> do
    i <- inc
    lift $ send' (reify @r) (reify @s) i x -- there's dangling Async there
    return absent
  | reify @t == reify @r = \_ -> do
    i <- inc
    lift $ recv (reify @s) i
  | otherwise = \_ -> inc >> return absent
project1 hdl (Seq f g) t = \a -> project1 hdl f t a >>= project1 hdl g t
project1 hdl (Par f g) t = \(a, b) -> (,) <$> project1 hdl f t a <*> project1 hdl g t b
project1 _ (Fork @l) (_ :: Proxy t)
  | reify @l == reify @t = \xy -> (,) <$> async (fst <$> wait xy) <*> async (snd <$> wait xy)
  | otherwise = \_ -> return (absent, absent)
project1 _ (Join @l) (_ :: Proxy t)
  | reify @l == reify @t = \(x, y) -> async ((,) <$> wait x <*> wait y)
  | otherwise = \_ -> return absent
project1 _ (Perm p) t = project1Perm p t
-- conditionals
project1 _ (Split @s) (_ :: Proxy t)
  | reify @t == reify @s = \xy -> do
    xy' <- wait xy
    i <- inc
    case xy' of
      (Left a) -> do
        bcast i (Left ())
        Left <$> async (return a)
      (Right b) -> do
        bcast i (Right ())
        Right <$> async (return b)
  | otherwise = \_ -> do
    i <- inc
    c <- recv (reify @s) i
    c' <- wait c
    case c' of
      (Left ()) -> return (Left absent)
      (Right ()) -> return (Right absent)
project1 _ (Coalesce @l) (_ :: Proxy t)
  | reify @t == reify @l = \case
    (Left a) -> return (Left <$> a)
    (Right b) -> return (Right <$> b)
  | otherwise = \_ -> return absent
project1 hdl (Branch f g) (t :: Proxy t) = \case
    (Left a)  -> Left <$> project1 hdl f t a
    (Right b) -> Right <$> project1 hdl g t b

project :: forall (l :: Type) f a b. (Typeable l) =>
           (forall a b. f a b -> a -> IO b) -> CSD f a b -> Asynced a -> Http (Asynced b)
project hdl c a =
  let c' = project1 hdl c (Proxy :: Proxy l)
  in evalStateT (c' a) 0

---------------------------------------------------------------------------------------------------
-- * Tests

foo :: (Typeable l, Typeable l') =>
       CSD (Kleisli IO) (() @ l * () @ l') (String @ l * (String, Int) @ l')
foo =
     perf (\_ -> getLine) || perf (\_ -> return 42)
  ||> (perf (\s -> return (s, s)) |> Fork) || noop
  -- ||> (noop || Comm) || noop
  -- ||> Perm AssocR
  -- ||> noop || Join
  |> noop || Comm || noop
  |> noop || Join
