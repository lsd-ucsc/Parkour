{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Control.CSD.CSD where

import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.Kind
import Data.Proxy
import Data.Typeable
import Control.Concurrent.Async.Lifted
import Control.Arrow (Kleisli(..))
import Control.CSD.Network hiding (Loc)
import Control.Monad.State hiding (join)

-- * Sites

data (a :: Type) @ (l :: Type)

data Either' ls a b where
  Left' :: a -> Either' ls a b
  Right' :: b -> Either' ls a b

-- * Locations

type Loc = Type

reify :: forall l. (Typeable l) => LocTm
reify = show (typeRep (Proxy :: Proxy l))

eqLoc :: forall l l'. (Typeable l, Typeable l') => Bool
eqLoc = reify @l == reify @l'

type family In (l :: Type) (ls :: [Type]) :: Constraint where
  In x (x : xs) = ()
  In x (y : xs) = In x xs

-- * CSDs

data CSD f a b where
  -- Permutations
  Noop   :: CSD f a a
  Swap   :: CSD f (a, b) (b, a)
  AssocL :: CSD f (a, (b, c)) ((a, b), c)
  AssocR :: CSD f ((a, b), c) (a, (b, c))
  CongL  :: CSD f a b -> CSD f (ctx, a) (ctx, b)
  CongR  :: CSD f a b -> CSD f (a, ctx) (b, ctx)  

  -- Sequence Composition
  Perf :: (Typeable l) => f a b -> CSD f (a @ l) (b @ l)
  Seq  :: CSD f a b -> CSD f b c -> CSD f a c

  -- Parallel Composition (also subsumes arrows)
  Par  :: CSD f a c -> CSD f b d -> CSD f (a, b) (c, d)
  Fork :: (Typeable l) => CSD f ((a, b) @ l) (a @ l, b @ l)
  Join :: (Typeable l) => CSD f (a @ l, b @ l) ((a, b) @ l)

  -- Communication
  To :: forall l l' f a. (Typeable l, Typeable l', Show a, Read a) => 
        CSD f (a @ l) (a @ l')

  -- Conditionals
  Split  :: (Typeable l) => CSD f (Either a b @ l) (Either' '[l] (a @ l) (b @ l))
  Notify :: (Typeable l, Typeable l', In l ls) => 
            CSD f (Either' ls a b, c @ l') (Either' (l':ls) (a, c @ l') (b, c @ l'))
  Branch :: CSD f a c -> CSD f b d -> CSD f (Either' ls a b) (Either' ls c d)
  Idem   :: CSD f (Either' ls a a) a

-- Derived operations and syntax sugar

perf :: (Typeable l) => (a -> m b) -> CSD (Kleisli m) (a @ l) (b @ l)
perf m = Perf (Kleisli m)

-- the following operators are named after arrow operators with similar behaviors

infixr 1 >>>
infixr 3 ***

(>>>) :: CSD f a b -> CSD f b c -> CSD f a c
a >>> b = Seq a b

(***) :: CSD f a c -> CSD f b d -> CSD f (a, b) (c, d)
a *** b = Par a b

first :: (Typeable l) => 
         CSD f (b @ l) (c @ l) -> CSD f ((b, d) @ l) ((c, d) @ l)
first c = 
  Fork       >>>
  c *** Noop >>>
  Join 

---------------------------------------------------------------------------------------------------
-- * Interpreations

type family Asynced a where
  Asynced (a @ l) = Async a
  Asynced (a, b) = (Asynced a, Asynced b)
  Asynced (Either' ls a b) = Either (Asynced a) (Asynced b)

type CentralF a b = Asynced a -> IO (Asynced b)

-- The Centralized Semantics

runCSD :: (forall a b. f a b -> a -> IO b) -> CSD f a b -> CentralF a b
-- permutations
runCSD _   Noop      = \a -> return a
runCSD _   Swap      = \(a, b) -> return (b, a)
runCSD _   AssocL    = \(a, (b, c)) -> return ((a, b), c)
runCSD _   AssocR    = \((a, b), c) -> return (a, (b, c))
runCSD hdl (CongL x) = \(ctx, a) -> (ctx,) <$> runCSD hdl x a
runCSD hdl (CongR x) = \(a, ctx) -> (,ctx) <$> runCSD hdl x a
-- sequential composition
runCSD hdl (Perf act) = \a -> async (wait a >>= hdl act)
runCSD hdl (Seq f g)  = \a -> runCSD hdl f a >>= runCSD hdl g
-- parallel composition
runCSD hdl (Par f g)  = \(a, b) -> (,) <$> runCSD hdl f a <*> runCSD hdl g b
runCSD _   Fork       = \ab -> (,) <$> async (fst <$> wait ab) <*> async (snd <$> wait ab)
runCSD _   Join       = \(a, b) -> async ((,) <$> wait a <*> wait b)
-- communication
runCSD _ To = \a -> return a
-- conditionals
runCSD _ Split = \ab -> do
  ab' <- wait ab
  case ab' of
    (Left a)  -> Left <$> async (return a)
    (Right b) -> Right <$> async (return b)
runCSD _ Notify = \(ab, c) -> do
  case ab of 
    (Left a)  -> return (Left (a, c))
    (Right b) -> return (Right (b, c))
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
-- permutations
project1 _   Noop      _ = \a -> return a
project1 _   Swap      _ = \(a, b) -> return (b, a)
project1 _   AssocL    _ = \(a, (b, c)) -> return ((a, b), c)
project1 _   AssocR    _ = \((a, b), c) -> return (a, (b, c))
project1 hdl (CongL x) t = \(ctx, a) -> (ctx,) <$> project1 hdl x t a
project1 hdl (CongR x) t = \(a, ctx) -> (,ctx) <$> project1 hdl x t a
-- sequential composition
project1 hdl (Perf @l act) (_ :: Proxy t)
  | reify @l == reify @t = \a -> async (wait a >>= hdl act)
  | otherwise = \_ -> return absent
project1 hdl (Seq f g) t = \a -> project1 hdl f t a >>= project1 hdl g t
-- parallel composition
project1 hdl (Par f g) t = \(a, b) -> (,) <$> project1 hdl f t a <*> project1 hdl g t b
project1 _ (Fork @l) (_ :: Proxy t) 
  | reify @l == reify @t = \ab -> (,) <$> async (fst <$> wait ab) <*> async (snd <$> wait ab)
  | otherwise = \_ -> return (absent, absent)
project1 _ (Join @l) (_ :: Proxy t)
  | reify @l == reify @t = \(a, b) -> async ((,) <$> wait a <*> wait b)
  | otherwise = \_ -> return absent
-- communication  
project1 _ (To @s @r) (_ :: Proxy t)
  | reify @s == reify @r = return 
  | reify @t == reify @s = \a -> do
    x <- inc (reify @r)
    lift $ send' (reify @r) (reify @s) x a -- there's dangling Async there
    return absent
  | reify @t == reify @r = \_ -> do  
    x <- inc (reify @s) 
    lift $ recv (reify @s) x
  | otherwise = \_ -> return absent
-- conditionals
project1 _ (Split @l) (_ :: Proxy t)
  | reify @t == reify @l = \ab -> do
    ab' <- wait ab
    case ab' of
      (Left a)  -> Left <$> async (return a)
      (Right b) -> Right <$> async (return b)
  | otherwise = \_ -> return (Left absent)
project1 _ (Notify @s @r) (_ :: Proxy t)
  | reify @s == reify @r = \(ab, c) -> do
    case ab of 
      (Left a)  -> return (Left (a, c))
      (Right b) -> return (Right (b, c))
  | reify @t == reify @s = \(ab, _) -> do
    x <- inc (reify @r)
    case ab of 
      (Left a)  -> do 
        lift $ send (reify @r) (reify @s) x True
        return (Left (a, absent))
      (Right b) -> do 
        lift $ send (reify @r) (reify @s) x False
        return (Right (b, absent))
  | reify @t == reify @r = \(_, c) -> do
    x <- inc (reify @s)
    ab' <- lift $ recv (reify @s) x
    ab <- wait ab'
    case ab of
      True -> return (Left (absent, c))
      False -> return (Right (absent, c))
  | otherwise = \_ -> return (Left (absent, absent))
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

foo :: (Typeable l) => CSD (Kleisli IO) (() @ l) (String @ l)
foo =
  perf (\_ -> getLine)

foo2 :: (Typeable l) => CSD (Kleisli IO) (() @ l, () @ l) (String @ l, Int @ l)
foo2 =
  perf (\_ -> getLine) *** perf (\_ -> return 42)

foo3 :: (Typeable l, Typeable l') => CSD (Kleisli IO) (() @ l, () @ l') (String @ l, (String, Int) @ l')
foo3 =
  perf (\_ -> getLine) *** perf (\_ -> return 42) >>>
  (perf (\s -> return (s, s)) >>> Fork) *** Noop  >>>
  AssocR                                          >>>
  Noop *** To *** Noop                            >>>
  Noop *** Join