{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Control.CSD.CSD where

import Prelude hiding ((||), pure, id)
import Data.Kind
import Data.Proxy
import Data.Typeable
import Control.Concurrent.Async.Lifted
import Control.Arrow hiding ((>>>), (***), (|||))
import Control.CSD.Network
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

data Perm a b where
  Id     :: Perm a a
  Swap   :: Perm (a * b) (b * a)
  AssocL :: Perm (a * (b * c)) ((a * b) * c)
  AssocR :: Perm ((a * b) * c) (a * (b * c))

data Distrib a b where
  In  :: Distrib ((a + b) * c) (a * c + b * c)
  Out :: Distrib (a * c + b * c) ((a + b) * c)

data CSD f a b where
  -- Sequential Compositions
  Perf :: (Typeable l) => f x y -> CSD f (x @ l) (y @ l)
  Comm :: (Typeable l, Typeable l', Show x, Read x) => CSD f (x @ l) (x @ l')
  Seq  :: CSD f a b -> CSD f b c -> CSD f a c
  -- Parallel Compositions
  Par  :: CSD f a c -> CSD f b d -> CSD f (a * b) (c * d)
  Fork :: (Typeable l) => CSD f ((x, y) @ l) (x @ l * y @ l)
  Join :: (Typeable l) => CSD f (x @ l * y @ l) ((x, y) @ l)
  Perm :: Perm a b -> CSD f a b
  -- Conditionals
  Split   :: (Typeable l) => CSD f (Either x y @ l) (x @ l + y @ l)
  Merge   :: (Typeable l) => CSD f (x @ l + y @ l) (Either x y @ l)
  Branch  :: CSD f a c -> CSD f b d -> CSD f (a + b) (c + d)
  Distrib :: Distrib a b -> CSD f a b

-- Syntax sugar and derived operations
infixr 1 >>>
infixr 1 >>>~
infixr 3 ***
infixr 3 |||

perf :: (Typeable l) => f x y -> CSD f (x @ l) (y @ l)
perf = Perf

perfM :: (Typeable l) => (x -> m y) -> CSD (Kleisli m) (x @ l) (y @ l)
perfM m = perf (Kleisli m)

pure :: (Typeable l, Arrow f) => (x -> y) -> CSD f (x @ l) (y @ l)
pure f = perf (arr f)

comm :: (Typeable l, Typeable l', Show x, Read x) => CSD f (x @ l) (x @ l')
comm = Comm

(>>>) :: CSD f a b -> CSD f b c -> CSD f a c
f >>> g = Seq f g

(***) :: CSD f a c -> CSD f b d -> CSD f (a * b) (c * d)
f *** g = Par f g

fork :: (Typeable l) => CSD f ((x, y) @ l) (x @ l * y @ l)
fork = Fork

join :: (Typeable l) => CSD f (x @ l * y @ l) ((x, y) @ l)
join = Join

id :: forall a f. CSD f a a
id  = Perm Id

swap :: CSD f (a * b) (b * a)
swap = Perm Swap

assocL :: CSD f (a * (b * c)) ((a * b) * c)
assocL = Perm AssocL

assocR :: CSD f ((a * b) * c) (a * (b * c))
assocR = Perm AssocR

split :: (Typeable l) => CSD f (Either x y @ l) (x @ l + y @ l)
split = Split

merge :: (Typeable l) => CSD f (x @ l + y @ l) (Either x y @ l)
merge = Merge

distribIn :: CSD f ((a + b) * c) (a * c + b * c)
distribIn = Distrib In

distribOut :: CSD f (a * c + b * c) ((a + b) * c)
distribOut = Distrib Out

(|||) :: CSD f a c -> CSD f b d -> CSD f (a + b) (c + d)
f ||| g = Branch f g

-- this version works nicer with >>>~
noop :: CSD f (x @ l) (x @ l)
noop = Perm Id

(>>>~) :: (Normalizable b, Normalizable b', Norm b ~ Norm b') =>
        CSD f a b -> CSD f b' c -> CSD f a c
f >>>~ g = f >>> toNorm >>> fromNorm >>> g

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
  fromNorm = Par fromNorm fromNorm >>> Perm AssocL
  toNorm = Perm AssocR >>> Par toNorm toNorm

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

runCSDPerm :: Perm a b -> CentralF a b
runCSDPerm Id = \a -> return a
runCSDPerm Swap = \(a, b) -> return (b, a)
runCSDPerm AssocL = \(a, (b, c)) -> return ((a, b), c)
runCSDPerm AssocR = \((a, b), c) -> return (a, (b, c))

runCSDDistrib :: Distrib a b -> CentralF a b
runCSDDistrib In = \(ab, c) -> return (either (Left . (,c)) (Right . (,c)) ab)
runCSDDistrib Out = \acbc -> return (either (\(a, c) -> (Left a, c)) (\(b, c) -> (Right b, c)) acbc)

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
runCSD _ Merge = \case
  Left x -> async (Left <$> wait x)
  Right y -> async (Right <$> wait y)
runCSD hdl (Branch f g) = \case
  (Left a)  -> Left <$> runCSD hdl f a
  (Right b) -> Right <$> runCSD hdl g b
runCSD _ (Distrib d) = runCSDDistrib d

-- ** Distributed Semantics

type ProjectedF a b = forall (t :: Type) m. (Typeable t, Network m, MonadIO m) => Proxy t -> Asynced a -> StateT Int m (Asynced b)

absent :: Async a
absent = error "Trying to access a value that is elsewhere."

inc :: (Monad m) => StateT Int m Int
inc = do
  v <- get
  put (v + 1)
  return v

project1Perm :: Perm a b  -> ProjectedF a b
project1Perm Id _ = \a -> return a
project1Perm Swap _ = \(a, b) -> return (b, a)
project1Perm AssocL _ = \(a, (b, c)) -> return ((a, b), c)
project1Perm AssocR _ = \((a, b), c) -> return (a, (b, c))

project1Distrib :: Distrib a b -> ProjectedF a b
project1Distrib In _ = \(ab, c) -> return (either (Left . (,c)) (Right . (,c)) ab)
project1Distrib Out _ = \acbc -> return (either (\(a, c) -> (Left a, c)) (\(b, c) -> (Right b, c)) acbc)

project1 :: (forall x y. f x y -> x -> IO y) -> CSD f a b -> ProjectedF a b
project1 hdl (Perf @l act) (_ :: Proxy t)
  | reify @l == reify @t = \x -> async (wait x >>= hdl act)
  | otherwise = \_ -> return absent
project1 _ (Comm @s @r) (_ :: Proxy t)
  | reify @s == reify @r = return
  | reify @t == reify @s = \x -> do
    i <- inc
    lift $ send (reify @r) i x -- there's dangling Async there
    return absent
  | reify @t == reify @r = \_ -> do
    i <- inc; lift $ recv i
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
        lift $ bcast i True
        Left <$> async (return a)
      (Right b) -> do
        lift $ bcast i False
        Right <$> async (return b)
  | otherwise = \_ -> do
    i <- inc
    c <- lift $ recv i
    c' <- wait c
    if c'
    then return (Left absent)
    else return (Right absent)
project1 _ (Merge @l) (_ :: Proxy t)
  | reify @t == reify @l = \case
    (Left a) -> return (Left <$> a)
    (Right b) -> return (Right <$> b)
  | otherwise = \_ -> return absent
project1 hdl (Branch f g) (t :: Proxy t) = \case
    (Left a)  -> Left <$> project1 hdl f t a
    (Right b) -> Right <$> project1 hdl g t b
project1 _ (Distrib d) t = project1Distrib d t

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
  perfM (\_ -> getLine) *** pure (\_ -> 42) >>>~
  (pure (\s -> (s, s)) >>> Fork) *** noop >>>~
  -- (noop *** Comm) *** noop >>>
  -- Perm AssocR >>>
  -- noop *** Join
  noop *** Comm *** noop >>>~
  noop *** Join
