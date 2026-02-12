{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE UnicodeSyntax #-}

module SepArrow where

import Control.Arrow
import Data.Kind (Type)
import Data.Typeable

infixr 1 >>>>
infixr 3 ****
infixr 2 *
infix  3 @

type Party = Type
type PartyTm = String

reify :: ∀ p. (Typeable p) => PartyTm
reify = show (typeRep (Proxy :: Proxy p))

eqParty :: ∀ p q. (Typeable p, Typeable q) => Bool
eqParty = reify @p == reify @q

data State where
  At :: Type -> Party -> State
  Star :: State -> State -> State

type a * b = Star a b
type a @ p = a `At` p

type Perm a b = ∀ f. Interp f a %1 -> Interp f b

type family Interp (f :: Type -> Party -> Type) (a :: State) where
  Interp f (At a p) = f a p
  Interp f (Star a b) = (Interp f a, Interp f b)

perm1 :: Perm a a
perm1 = \a -> a

perm2 :: Perm (a @ p * (b @ q * c @ r)) ((b @ q * c @ r) * a @ p)
perm2 = \(a, (b, c)) -> ((b, c), a)

-- perm3 :: Perm (a @ p * b @ q) (a @ p)
-- perm3 = \(a, b) -> a

-- perm4 :: Perm (Int @ p * Int @ q) (Int @ p * Int @ q)
-- perm4 = \(a, b) -> (a + 3, b)

-- class Arrow (a :: Type -> Type -> Type) where
--   arr   :: (b -> c) -> a b c
--   (>>>) :: a b c -> a c d -> a b d
--   (***) :: a b c -> a d e -> a (b, d) (c, e)

-- TODO: add information flow to locations
-- TODO: connection to graded/parameterized monads
class SepArrow (a :: State -> State -> Type) where
  locally :: (Typeable p) => (b -> c) -> a (b @ p) (c @ p)
  (>>>>)  :: a b c -> a c d -> a b d
  (****)  :: a b d -> a c e -> a (b * c) (d * e)
  comm    :: (Typeable p, Typeable q) => a (b @ p) (b @ q)
  fork    :: (Typeable p) => a ((b, c) @ p) (b @ p * c @ p)
  join    :: (Typeable p) => a (b @ p * c @ p) ((b, c) @ p)
  perm    :: Perm b c -> a b c

-- Derived combinators

noop :: (SepArrow a) => a b b
noop = perm (\x -> x)

assocR :: (SepArrow a) => a ((b * c) * d) (b * (c * d))
assocR = perm (\((b, c), d) -> (b, (c, d)))

forkL :: (SepArrow a, Typeable p) => a (b @ p) (b @ p * () @ p)
forkL = locally (\b -> (b, ())) >>>> fork

forkR :: (SepArrow a, Typeable p) => a (b @ p) (() @ p * b @ p)
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

ex3 :: (SepArrow a, Typeable p, Typeable q) => a (() @ p * () @ q) (() @ p * Int @ q)
ex3 =
  (f **** g)                   >>>>
  (forkR **** noop)            >>>>
  assocR                       >>>>
  (noop **** (comm **** noop)) >>>>
  (noop **** join)             >>>>
  (noop **** locally (\(x, y) -> x + y))
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
-- instance (Arrow a) => SepArrow (ToSep a) where
--   locally f = ToSep (arr $ \(Forget a) -> Forget (f a))
--   (ToSep a) >>>> (ToSep b) = ToSep (a >>> b)
--   (ToSep a) **** (ToSep b) = ToSep (a *** b)
--   comm = ToSep (arr $ \(Forget a) -> Forget a)
--   fork = ToSep (arr $ \(Forget (a, b)) -> (Forget a, Forget b))
--   join = ToSep (arr $ \(Forget a, Forget b) -> Forget (a, b))
--   perm f = ToSep (arr undefined)

-- newtype ToSep a b c = ToSep (a (Interp Forget b) (Interp Forget c))

-- Forget :: a -> Forget a l
-- newtype Forget a (l :: Loc) = Forget a

-- Projection

-- newtype Proj a t b c = Proj (a (Interp ProjF b) (Interp ProjF c))

-- newtype ProjF a l = ProjF a

-- SepArrow -> Target -> Arrow
-- instance (SepArrow Target Arrow)
-- instance (SepArrow s) => (Arrow (Proj s t))

-- implementation SepArrow => implementation Arrow
-- Prog SepArrow -> Prog Arrow

-- (SepArrow a) => a b c -> Proj a t b c
--
-- foo :: (SepArrow s) => s b c
-- alice :: Typeable alice
--
-- fooAlice :: (Arrow a) => a (Interp b) (Interp c)

-- instance (Arrow a, Typeable t) => SepArrow (Proj a t) where
--   locally :: forall l b c. (b -> c) -> Proj a t (b @ l) (c @ l)
--   locally = undefined
    -- | eqLoc @t @l = undefined
