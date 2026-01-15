{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module SepArrow where

import Data.Kind (Type)

infixr 2 *
infixr 1 >>>
infixr 3 ***

-- Shapes of states

data Local a
data (a :: Type) * (b :: Type)

-- we could have written the following, but the above is a bit easier to work with
-- data State where
--   Local :: Type -> State
--   Star  :: State -> State -> State

-- Shallowly-embedded permutations

type Perm a b = forall f. Interp f a %1 -> Interp f b

type family Interp (f ::Type -> Type) a where
    Interp f (Local a) = f a
    Interp f (a * b)   = (Interp f a, Interp f b)

perm1 :: Perm (Local a) (Local a)
perm1 = \a -> a

perm2 :: Perm (Local a * (Local b * Local c)) ((Local b * Local c) * Local a)
perm2 = \(a, (b, c)) -> ((b, c), a)

-- perm3 :: Perm (Local a * Local b) (Local a)
-- perm3 = \(a, b) -> a

-- perm4 :: Perm (Local Int * Local Int) (Local Int * Local Int)
-- perm4 = \(a, b) -> (plus3 a, b)
--   where
--     plus3 :: Int %1 -> Int
--     plus3 = undefined

class SepArrow (a :: Type -> Type -> Type) where
  locally :: (b -> c) -> a (Local b) (Local c)
  (>>>)   :: a b c -> a c d -> a b d
  (***)   :: a b d -> a c e -> a (b * c) (d * e)
  fork    :: a (Local (b, c)) (Local b * Local c)
  join    :: a (Local b * Local c) (Local (b, c))
  perm    :: Perm b c -> a b c

ex1 :: (SepArrow a) => a (b * c) (f * g)
ex1 = f1 *** g1 >>> f2 *** g2 -- (f1 >>> f2) *** (g1 >>> g2)
  where
    f1 :: a b d
    f1 = undefined
    f2 :: a d f
    f2 = undefined
    g1 :: a c e
    g1 = undefined
    g2 :: a e g
    g2 = undefined
