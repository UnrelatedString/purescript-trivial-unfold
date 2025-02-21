-- | This module provides the `Trivial` type as an existentially quantified
-- | dumb wrapper around `unfold`, which can be inspected and manipulated
-- | to implement various typeclasses and the utilities in Data.Unfoldable.Trivial.Adapter.

module Data.Unfoldable.Trivial
 ( Trivial(..)
 , UnfoldrCall(..)
 , trivial
 , turbofish
 , (::<*>)
 , unfoldr1Default
 , uncons
 , head
 , tail
 , runTrivial
 , cons
 , snoc
 ) where

import Prelude

import Data.Foldable (class Foldable, foldrDefault, foldMapDefaultL)
import Data.Unfoldable (class Unfoldable, class Unfoldable1, unfoldr, unfoldr1, none)
import Data.Tuple (snd, uncurry)
import Data.Tuple.Nested ((/\), type (/\))
import Data.Maybe (Maybe(..), maybe)
import Data.Exists (Exists, mkExists, runExists)
import Data.Newtype (class Newtype, unwrap)
import Data.Bifunctor (lmap)
import Test.QuickCheck.Arbitrary (class Arbitrary, class Coarbitrary, arbitrary)
import Test.QuickCheck.Gen (sized)

-- | A constructor taking the same arguments as `unfoldr`.
data UnfoldrCall a b = UnfoldrCall (b -> Maybe (a /\ b)) b

-- | A newtype wrapping `UnfoldrCall a b`, existentially quantified over the "seed" type `b`.
newtype Trivial a = Trivial (Exists (UnfoldrCall a))
derive instance Newtype (Trivial a) _

-- | Specializes its argument to `Trivial`.
trivial :: forall a. Trivial a -> Trivial a
trivial = identity

-- | Function application specialized to a `Trivial` argument,
-- | at the same precedence as `($)`.
-- |
-- | Inspired by the Rust syntax of the same name, often used in the
-- | analogous context of collecting from an iterator.
turbofish :: forall a b. (Trivial a -> b) -> Trivial a -> b
turbofish = identity

infixr 0 turbofish as ::<*>

-- | Internal helper for implementing functions on Trivial.
untrivial :: forall a c. (forall b. UnfoldrCall a b -> c) -> Trivial a -> c
untrivial f = runExists f <<< unwrap

-- | Wraps both arguments to `unfoldr` in an `UnfoldrCall`.
instance trivialUnfoldable :: Unfoldable Trivial where
  unfoldr f seed = Trivial $ mkExists $ UnfoldrCall f seed

instance trivialUnfoldable1 :: Unfoldable1 Trivial where
  unfoldr1 = unfoldr1Default

instance trivialFunctor :: Functor Trivial where
  map :: forall a c. (a -> c) -> Trivial a -> Trivial c
  map f = untrivial eMap
    where eMap :: forall b. UnfoldrCall a b -> Trivial c
          eMap (UnfoldrCall g seed) = Trivial
                                    $ mkExists
                                    $ UnfoldrCall (
                                      map (lmap f) <<< g
                                    ) seed

-- | Provides a default implementation of `unfoldr1` using `unfoldr` to satisfy
-- | the superclass bound on `Unfoldable`.
unfoldr1Default :: forall a b t. Unfoldable t => (b -> a /\ Maybe b) -> b -> t a
unfoldr1Default f = unfoldr (map f) <<< Just

-- | Returns the first element and a new `Unfoldable` generating the remaining elements,
-- | or `Nothing` if there are no elements.
uncons :: forall a u. Unfoldable u => Trivial a -> Maybe (a /\ u a)
uncons = untrivial eUncons
  where eUncons :: forall b. UnfoldrCall a b -> Maybe (a /\ u a)
        eUncons (UnfoldrCall f seed) = f seed <#> map (unfoldr f)

-- | Returns the first element, if present.
-- |
-- | Not particularly useful, because this is just the `Unfoldable`
-- | instance for `Maybe`. Included by analogy with `head1`.
-- AND because it took me like. MULTIPLE DAYS to realize this LMAO.
-- Polymorphic return types kinda mess with my head
head :: forall a. Trivial a -> Maybe a
head = runTrivial

-- | Removes the first element, if present.
tail :: forall a u. Unfoldable u => Trivial a -> u a
tail = maybe none snd <<< uncons

-- | Converts to any other `Unfoldable`.
-- | Can also be seen as evaluating the inner `UnfoldrCall`.
runTrivial :: forall a u. Unfoldable u => Trivial a -> u a
runTrivial = untrivial eRunTrivial
  where eRunTrivial :: forall b. UnfoldrCall a b -> u a
        eRunTrivial (UnfoldrCall f seed) = unfoldr f seed

-- | The *raison d'être* for `Trivial`.
-- | Allows folding polymorphic `Unfoldable`s as they generate.
-- |
-- | `foldr` uses a default implementation and may be inefficient.
instance trivialFoldable :: Foldable Trivial where
  foldl :: forall a c. (c -> a -> c) -> c -> Trivial a -> c
  foldl f foldInit = untrivial eFoldl
    where eFoldl :: forall b. UnfoldrCall a b -> c
          eFoldl (UnfoldrCall g unfoldSeed) = lockstep unfoldSeed foldInit
            where lockstep :: b -> c -> c
                  lockstep seed acc
                    | Just (a /\ seed') <- g seed = lockstep seed' $ f acc a
                    | otherwise = acc

  foldr f = foldrDefault f
  foldMap f = foldMapDefaultL f

-- | Guaranteed finite.
instance trivialArbitrary :: (Arbitrary a, Coarbitrary a) => Arbitrary (Trivial a) where
  arbitrary = sized \size -> do
    (f :: a -> Maybe (a /\ a)) <- arbitrary 
    seed <- arbitrary
    pure $ unfoldr (uncurry \i b -> 
      if i >= size
      then Nothing
      else map ((i + 1) /\ _) <$> f b
    ) $ 0 /\ seed

-- | Prepend an element.
-- |
-- | Do not use this to create a data structure. Please use Data.List.Lazy instead.
cons :: forall a u. Unfoldable1 u => a -> Trivial a -> u a
cons h t = untrivial eCons t
  where eCons :: forall b. UnfoldrCall a b -> u a
        eCons (UnfoldrCall f seed) = unfoldr1 hilbertHotel $ h /\ seed
          where hilbertHotel :: a /\ b -> a /\ Maybe (a /\ b)
                hilbertHotel = map f

-- | Append an element.
-- |
-- | Do not use this to create a data structure. Please use Data.List.Lazy instead.
snoc :: forall a u. Unfoldable1 u => Trivial a -> a -> u a
snoc t l = untrivial eSnoc t
  where eSnoc :: forall b. UnfoldrCall a b -> u a
        eSnoc (UnfoldrCall f seed) = unfoldr1 failsafed seed
          where failsafed :: b -> a /\ Maybe b
                failsafed b
                  | Just (a /\ b') <- f b = a /\ Just b'
                  | otherwise = l /\ Nothing
