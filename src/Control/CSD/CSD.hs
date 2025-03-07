{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Control.CSD.CSD where

import Prelude hiding ((||))
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.Kind
import Data.Proxy
import Data.Typeable
import Control.Concurrent.Async.Lifted
import Control.Arrow
import Control.CSD.Network
    ( Http, LocTm, Network(recv, send', send) )
import Control.Monad.State hiding (join)

-- * Sites

infix  3 @
infixr 2 *
infixr 1 +

data (a :: Type) @ (l :: Type)
data (a :: Type) * (b ::Type)
data (a :: Type) + (b :: Type)

-- * Locations

reify :: forall l. (Typeable l) => LocTm
reify = show (typeRep (Proxy :: Proxy l))

eqLoc :: forall l l'. (Typeable l, Typeable l') => Bool
eqLoc = reify @l == reify @l'

type family In (l :: Type) (ls :: [Type]) :: Constraint where
  In x (x : xs) = ()
  In x (y : xs) = In x xs

-- * CSDs

infix 1 ≃ 

data a ≃ b where
  Noop :: a ≃ a
  Swap :: a * b ≃ b * a
  AssocL :: a * (b * c) ≃ (a * b) * c
  AssocR :: (a * b) * c ≃ a * (b * c)

data CSD f a b where
  Perf :: (Typeable l) => f x y -> CSD f (x @ l) (y @ l)
  Comm :: (Typeable l', Typeable l, Show x, Read x) => CSD f (x @ l) (x @ l')
  Seq  :: CSD f a b -> CSD f b c -> CSD f a c
  Par  :: CSD f a c -> CSD f b d -> CSD f (a * b) (c * d)
  Fork :: (Typeable l) => CSD f ((x, y) @ l) (x @ l * y @ l)
  Join :: (Typeable l) => CSD f (x @ l * y @ l) ((x, y) @ l)
  Perm :: a ≃ b -> CSD f a b

  -- Conditionals  
  Split  :: (Typeable l) => CSD f (Either a b @ l) (a @ l + b @ l)
  Branch :: CSD f a c -> CSD f b d -> CSD f (a + b) (c + d)
  Idem   :: CSD f (a + a) a

-- Derived operations and syntax sugar

perf :: (Typeable l) => (a -> m b) -> CSD (Kleisli m) (a @ l) (b @ l)
perf m = Perf (Kleisli m)

-- this version works nicer with |>~
noop :: CSD f (a @ l) (a @ l)
noop = Perm Noop

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

instance Normalizable (a @ l) where
  fromNorm = Perm Noop
  toNorm = Perm Noop

instance (Normalizable c) => Normalizable (x @ l * c) where
  fromNorm = Par (Perm Noop) fromNorm
  toNorm = Par (Perm Noop) toNorm

instance (Normalizable a, Normalizable (b * c)) => Normalizable ((a * b) * c) where
  fromNorm = Par fromNorm fromNorm ||> Perm AssocL
  toNorm = Perm AssocR ||> Par toNorm toNorm

instance Normalizable (a + b) where
  fromNorm = Perm Noop
  toNorm = Perm Noop

instance (Normalizable c) => Normalizable ((a + b) * c) where
  fromNorm = Par (Perm Noop) fromNorm
  toNorm = Par (Perm Noop) toNorm

---------------------------------------------------------------------------------------------------
-- * Interpreations

type family Asynced a where
  Asynced (a @ l) = Async a
  Asynced (a * b) = (Asynced a, Asynced b)
  Asynced (a + b) = Either (Asynced a) (Asynced b)

type CentralF a b = Asynced a -> IO (Asynced b)

-- The Centralized Semantics

runCSD :: (forall a b. f a b -> a -> IO b) -> CSD f a b -> CentralF a b
runCSD hdl (Perf act)    = \a -> async (wait a >>= hdl act)
runCSD _   Comm          = \a -> return a
runCSD hdl (Seq f g)     = \a -> runCSD hdl f a >>= runCSD hdl g
runCSD hdl (Par f g)     = \(a, b) -> (,) <$> runCSD hdl f a <*> runCSD hdl g b
runCSD _   Fork          = \ab -> (,) <$> async (fst <$> wait ab) <*> async (snd <$> wait ab)
runCSD _   Join          = \(a, b) -> async ((,) <$> wait a <*> wait b)
runCSD _   (Perm Noop)   = \a -> return a
runCSD _   (Perm Swap)   = \(a, b) -> return (b, a)
runCSD _   (Perm AssocL) = \(a, (b, c)) -> return ((a, b), c)
runCSD _   (Perm AssocR) = \((a, b), c) -> return (a, (b, c))
-- conditionals
runCSD _ Split = \ab -> do
  ab' <- wait ab
  case ab' of
    (Left a)  -> Left <$> async (return a)
    (Right b) -> Right <$> async (return b)
runCSD hdl (Branch f g) = \case 
  (Left a)  -> Left <$> runCSD hdl f a
  (Right b) -> Right <$> runCSD hdl g b
runCSD _ Idem = \case
  (Left a) -> return a
  (Right b) -> return b

-- ** The Distributed Semantics

absent :: a
absent = error "Trying to access a value that is elsewhere."

inc :: (Monad m) => LocTm -> StateT (HashMap LocTm Int) m Int
inc loc = do
  m <- get
  let v = HM.findWithDefault 0 loc m
  put (HM.insert loc (v + 1) m)
  return v

type ProjectedF a b = 
  forall (t :: Type). (Typeable t) => Proxy t -> Asynced a -> StateT (HashMap LocTm Int) Http (Asynced b)  

project1 :: (forall a b. f a b -> a -> IO b) -> CSD f a b -> ProjectedF a b
project1 hdl (Perf @l act) (_ :: Proxy t)
  | reify @l == reify @t = \a -> async (wait a >>= hdl act)
  | otherwise = \_ -> return absent
project1 _ (Comm @s @r) (_ :: Proxy t)
  | reify @s == reify @r = return 
  | reify @t == reify @s = \a -> do
    x <- inc (reify @r)
    lift $ send' (reify @r) (reify @s) x a -- there's dangling Async there
    return absent
  | reify @t == reify @r = \_ -> do  
    x <- inc (reify @s) 
    lift $ recv (reify @s) x
  | otherwise = \_ -> return absent
project1 hdl (Seq f g) t = \a -> project1 hdl f t a >>= project1 hdl g t
project1 hdl (Par f g) t = \(a, b) -> (,) <$> project1 hdl f t a <*> project1 hdl g t b
project1 _ (Fork @l) (_ :: Proxy t) 
  | reify @l == reify @t = \ab -> (,) <$> async (fst <$> wait ab) <*> async (snd <$> wait ab)
  | otherwise = \_ -> return (absent, absent)
project1 _ (Join @l) (_ :: Proxy t)
  | reify @l == reify @t = \(a, b) -> async ((,) <$> wait a <*> wait b)
  | otherwise = \_ -> return absent
project1 _ (Perm Noop)   _ = \a -> return a
project1 _ (Perm Swap)   _ = \(a, b) -> return (b, a)
project1 _ (Perm AssocL) _ = \(a, (b, c)) -> return ((a, b), c)
project1 _ (Perm AssocR) _ = \((a, b), c) -> return (a, (b, c))  
-- conditionals
project1 _ (Split @l) (_ :: Proxy t)
  | reify @t == reify @l = \ab -> do
    ab' <- wait ab
    case ab' of
      (Left a)  -> Left <$> async (return a)
      (Right b) -> Right <$> async (return b)
  | otherwise = \_ -> return (Left absent)
project1 hdl (Branch f g) (t :: Proxy t) = \case 
    (Left a)  -> Left <$> project1 hdl f t a 
    (Right b) -> Right <$> project1 hdl g t b
project1 _ Idem (_ :: Proxy t) = \case
    (Left a) -> return a
    (Right b) -> return b

project :: forall (l :: Type) f a b. (Typeable l) =>
           (forall a b. f a b -> a -> IO b) -> CSD f a b -> Asynced a -> Http (Asynced b)
project hdl c a = 
  let c' = project1 hdl c (Proxy :: Proxy l)
  in evalStateT (c' a) HM.empty

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