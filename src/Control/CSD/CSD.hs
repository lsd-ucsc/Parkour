{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}

module Control.CSD.CSD where

import Control.Concurrent.Async
import Control.CSD.Site

class (Perm f) => CSD f where
  -- Sequence Composition
  perf :: (a -> IO b) -> f (Site a) (Site b)
  seqq :: f a b -> f b c -> f a c -- there's name conclit

  -- Parallel Composition
  par :: f a c -> f b d -> f (a, b) (c, d)
  -- fork/join are right-biased, meaning the right one is the outcoming/incoming site. In some
  -- cases, which one is the outcoming/incoming site has no effect on the semantics, but if it
  -- does, consider use directful join/fork and give them specific semantics rather than using
  -- the default one.
  fork :: f (Site (a, b)) (Site a, Site b)
  join :: (Show b, Read b) => f (Site a, Site b) (Site (a, b))

  -- Directful fork/join
  forkL :: f (Site (a, out)) (Site out, Site a)
  forkL = fork >>> swap

  forkR :: f (Site (a, out)) (Site a, Site out)
  forkR = fork

  joinL :: (Show in', Read in') => f (Site in', Site b) (Site (b, in'))
  joinL = swap >>> join

  joinR :: (Show in', Read in') => f (Site a, Site in') (Site (a, in'))
  joinR = join

-- the following operators are named after arrow operators share the same behavior

infixr 1 >>>
infixr 3 ***

(>>>) :: (CSD f) => f a b -> f b c -> f a c
a >>> b = seqq a b

(***) :: (CSD f) => f a c -> f b d -> f (a, b) (c, d)
a *** b = par a b

---------------------------------------------------------------------------------------------------
-- * Interpretation

-- ** Centralized Semantics

instance CSD (Tmap Async IO) where
  perf act = Tmap $ \a -> async (wait a >>= act)
  seqq f g = Tmap $ \a -> runTmap f a >>= runTmap g
  par  f g = Tmap $ \(a, b) -> (,) <$> runTmap f a <*> runTmap g b
  fork     = Tmap $ \ab -> (,) <$> async (fst <$> wait ab) <*> async (snd <$> wait ab)
  join     = Tmap $ \(a, b) -> async ((,) <$> wait a <*> wait b)

-- I really want to type this `(CSD f) => f a b -> T Async a -> IO (T Async b)`
runCSD :: Tmap Async IO a b -> T Async a -> IO (T Async b)
runCSD = runTmap

-- ** Distributed Semantics

-- Backend function
type Addr = String

send :: (Show a) => Addr -> a -> IO ()
send addr a = putStrLn (">>> send " ++ show a ++ " to " ++ addr)

recv :: (Read a) => Addr -> IO a
recv addr = putStrLn (">>> recv from " ++ addr) >> (read <$> getLine)

data Located a where
  Here :: Async a -> Located a
  There :: Addr -> Async () -> Located a

instance CSD (Tmap Located IO) where
  perf act = Tmap $ \case
    (Here a) -> Here <$> async (wait a >>= act)
    (There addr a) -> return (There addr a)
  -- the following two cases are exactly the same as the centralized semantics, which might mean
  -- we can futher refactor out things
  seqq f g = Tmap $ \a -> runTmap f a >>= runTmap g
  par  f g = Tmap $ \(a, b) -> (,) <$> runTmap f a <*> runTmap g b
  fork = Tmap $ \case
    (Here ab) -> (,) <$> (Here <$> async (fst <$> wait ab)) <*> (Here <$> async (snd <$> wait ab))
    (There addr a) -> return (There addr a, There addr a)
  join = Tmap $ \case
    (Here a, Here b) -> Here <$> async ((,) <$> wait a <*> wait b)
    (Here a, There addr _) -> Here <$> async ((,) <$> wait a <*> recv addr)
    (There addr a, Here b) -> There addr <$> async (wait a >> wait b >>= send addr)
    (There addr1 a, There _ _) -> return (There addr1 a)

-- same as above, I don't like the type signature
project :: Tmap Located IO a b -> T Located a -> IO (T Located b)
project = runTmap
