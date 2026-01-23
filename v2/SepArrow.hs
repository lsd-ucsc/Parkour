{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}

module SepArrow where

import Data.Kind (Type)
import Control.Arrow
import Data.Functor.Identity

infixr 1 >>>>
infixr 3 ****

data State where
  Local :: Type -> State
  Star  :: State -> State -> State

type a * b = Star a b

type Perm a b = forall f. Interp f a -> Interp f b

type family Interp (f :: Type -> Type) (a :: State) where
  Interp f (Local a)  = f a
  Interp f (Star a b) = (Interp f a, Interp f b)

perm1 :: Perm a a
perm1 = \a -> a

perm2 :: Perm (Local a * (Local b * Local c)) ((Local b * Local c) * Local a)
perm2 = \(a, (b, c)) -> ((b, c), a)

-- perm3 :: Perm (Local a * Local b) (Local a)
-- perm3 = \(a, b) -> a

-- perm4 :: Perm (Local Int * Local Int) (Local Int * Local Int)
-- perm4 = \(a, b) -> (plus a b, b)
--   where
--     plus :: Int %1 -> Int %1 -> Int
--     plus = undefined -- we couldn't define it because functions in base are non-linear by default

--     plus3 :: Int %1 -> Int
--     plus3 = undefined

class SepArrow (a :: State -> State -> Type) where
  locally :: (b -> c) -> a (Local b) (Local c)
  (>>>>)  :: a b c -> a c d -> a b d
  (****)  :: a b d -> a c e -> a (b * c) (d * e)
  fork    :: a (Local (b, c)) (Local b * Local c)
  join    :: a (Local b * Local c) (Local (b, c))
  perm    :: Perm b c -> a b c

-- Derived combinators

noop :: (SepArrow a) => a b b
noop = perm (\x -> x)

assocR :: (SepArrow a) => a ((b * c) * d) (b * (c * d))
assocR = perm (\((b, c), d) -> (b, (c, d)))

forkL :: (SepArrow a) => a (Local b) (Local b * Local ())
forkL = locally (\b -> (b, ())) >>>> fork

forkR :: (SepArrow a) => a (Local b) (Local () * Local b)
forkR = locally (\b -> ((), b)) >>>> fork

ex1 :: (SepArrow a) => a (b * c) (f * g)
ex1 = f1 **** g1 >>>> f2 **** g2 -- (f1 >>> f2) *** (g1 >>> g2)
  where
    f1 :: a b d
    f1 = undefined
    f2 :: a d f
    f2 = undefined
    g1 :: a c e
    g1 = undefined
    g2 :: a e g
    g2 = undefined

-- The relationship between arrows and separation arrows
--
-- In one sense, separation arrows are a subset of arrows that prohibits arbitrary state sharing.
-- With arrows, communication is implicit behind `arr`

ex2 :: (Arrow a) => a ((), ()) Int
ex2 = (f *** g) >>> arr (\(x, y) -> x + y)
  where
    f :: a () Int
    f = undefined
    g :: a () Int
    g = undefined

-- There's a lot going on/obfuscated behind the `arr \(x, y) -> x + y`: how does x and y come together; what happened to the existing two arrows...
-- And `arr` also makes writing interpreter hard because functions are uninspectable.
-- Separation arrows prohibit this behavior and enforce one to specify how infomration flows

ex3 :: (SepArrow a) => a (Local () * Local ()) (Local () * Local Int)
ex3 = (f **** g) >>>>
      (forkR **** noop) >>>>
      assocR >>>>
      (noop **** join) >>>>
      (noop **** locally (\(x, y) -> x + y))
  where
    f :: a (Local ()) (Local Int)
    f = undefined
    g :: a (Local ()) (Local Int)
    g = undefined

-- Notice the similarity and difference between the two programs.

-- In another sense, separation arrows can be implemented with arrows.
-- In other words, separation arrows do no introduce new behaviors to arrows and merely add additional type displicines and pritimives that manifest behaviors that previously were behind `arr`.
-- If we strip away the additional type displicine, i.e., remove `Local` and treat * as regular products then
-- each separation arrow corresponds to an arrow, which is evident by the following instance definition:

instance (Arrow a) => SepArrow (Forget a) where
  locally f = Forget (arr (fmap f))
  (Forget a) >>>> (Forget b) = Forget (a >>> b)
  (Forget a) **** (Forget b) = Forget (a *** b)
  fork = Forget (arr $ \(Identity (a, b)) -> (Identity a, Identity b))
  join = Forget (arr $ \(Identity a, Identity b) -> Identity (a, b))
  perm f = Forget (arr (f @Identity))

newtype Forget a b c = Forget (a (Strip b) (Strip c))

type Strip a = Interp Identity a
