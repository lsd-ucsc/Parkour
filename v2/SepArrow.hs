{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE LinearTypes #-}

module SepArrow where

import Control.Arrow
import Data.Kind (Type)
import Data.Typeable

infixr 1 >>>>
infixr 3 ****
infixr 2 *
infix  3 @

-- TODO: see if using singletons can make this simpler
-- reference: https://discourse.haskell.org/t/small-rewrite-of-haschor-using-singletons/13237
type Loc = Type
type LocTm = String

reify :: forall l. (Typeable l) => LocTm
reify = show (typeRep (Proxy :: Proxy l))

eqLoc :: forall l l'. (Typeable l, Typeable l') => Bool
eqLoc = reify @l == reify @l'

data State where
  At :: Type -> Loc -> State
  Star :: State -> State -> State

type a * b = Star a b

type a @ l = a `At` l

type Perm a b = forall f. Interp f a %1 -> Interp f b

type family Interp (f :: Type -> Loc -> Type) (a :: State) where
  Interp f (At a l) = f a l
  Interp f (Star a b) = (Interp f a, Interp f b)

perm1 :: Perm a a
perm1 = \a -> a

perm2 :: Perm (a @ l1 * (b @ l2 * c @ l3)) ((b @ l2 * c @ l3) * a @ l1)
perm2 = \(a, (b, c)) -> ((b, c), a)

-- perm3 :: Perm (a @ l1 * b @ l2) (a @ l1)
-- perm3 = \(a, b) -> a

-- perm4 :: Perm (Int @ l1 * Int @ l2) (Int @ l1 * Int @ l2)
-- perm4 = \(a, b) -> (a + 3, b)

class SepArrow (a :: State -> State -> Type) where
  locally :: (b -> c) -> a (b @ l) (c @ l)
  (>>>>)  :: a b c -> a c d -> a b d
  (****)  :: a b d -> a c e -> a (b * c) (d * e)
  comm    :: a (b @ l1) (b @ l2)
  fork    :: a ((b, c) @ l) (b @ l * c @ l)
  join    :: a (b @ l * c @ l) ((b, c) @ l)
  perm    :: Perm b c -> a b c

-- Derived combinators

noop :: (SepArrow a) => a b b
noop = perm (\x -> x)

assocR :: (SepArrow a) => a ((b * c) * d) (b * (c * d))
assocR = perm (\((b, c), d) -> (b, (c, d)))

forkL :: (SepArrow a) => a (b @ l) (b @ l * () @ l)
forkL = locally (\b -> (b, ())) >>>> fork

forkR :: (SepArrow a) => a (b @ l) (() @ l * b @ l)
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

ex3 :: (SepArrow a) => a (() @ l1 * () @ l2) (() @ l1 * Int @ l2)
ex3 =
  (f **** g)
    >>>> (forkR **** noop)
    >>>> assocR
    >>>> (noop **** (comm **** noop))
    >>>> (noop **** join)
    >>>> (noop **** locally (\(x, y) -> x + y))
  where
    f :: a (() @ l1) (Int @ l1)
    f = undefined
    g :: a (() @ l2) (Int @ l2)
    g = undefined

-- Notice the similarity and difference between the two programs.

-- In another sense, separation arrows can be implemented with arrows. In other words, separation
-- arrows do no introduce new behaviors to arrows and merely add additional type displicines and
-- pritimives that manifest behaviors that previously were behind `arr`. If we strip away the
-- additional type displicine, i.e., remove `Local` and treat * as regular products then each
-- separation arrow corresponds to an arrow, which is evident by the following instance definition:
instance (Arrow a) => SepArrow (ToSep a) where
  locally f = ToSep (arr $ \(Forget a) -> Forget (f a))
  (ToSep a) >>>> (ToSep b) = ToSep (a >>> b)
  (ToSep a) **** (ToSep b) = ToSep (a *** b)
  comm = ToSep (arr $ \(Forget a) -> Forget a)
  fork = ToSep (arr $ \(Forget (a, b)) -> (Forget a, Forget b))
  join = ToSep (arr $ \(Forget a, Forget b) -> Forget (a, b))
  perm f = ToSep (arr _)

newtype ToSep a b c = ToSep (a (Interp Forget b) (Interp Forget c))

newtype Forget a (l :: Loc) = Forget a

-- Projection

newtype Proj a t b c = Proj (a (Interp ProjF b) (Interp ProjF c))

newtype ProjF a l = ProjF a

instance (Arrow a, Typeable t) => SepArrow (Proj a t) where
  locally :: forall l b c. (b -> c) -> Proj a t (b @ l) (c @ l)
  locally
    | eqLoc @t @l = _
