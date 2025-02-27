-- | This module provides the `Trivial` type as an existentially quantified
-- | dumb wrapper around `unfold`, which can be inspected and manipulated
-- | to implement various typeclasses and the utilities in Data.Unfoldable.Trivial.Adapter.
-- | 
-- | This module also contains the implementations of utilities which rely on directly
-- | inspecting `Trivial` values and are re-exported by Data.Unfoldable.Trivial.Adapter.
-- | Use this module directly only if you intend to directly inspect `Trivial` values yourself.
-- | The public API of this module may change dramatically in the next major version, or
-- | even sooner within unstable 0.x.x versions.

module Data.Unfoldable.Trivial.Internal
 ( Trivial(..)
 , UnfoldrCall(..)
 , trivial
 , untrivial
 , turbofish
 , (::<*>)
 , unfoldr1Default
 , runTrivial
 ) where

import Prelude

import Data.Foldable (class Foldable, foldrDefault, foldMapDefaultL)
import Data.Unfoldable (class Unfoldable, class Unfoldable1, unfoldr)
import Data.Tuple (uncurry)
import Data.Tuple.Nested ((/\), type (/\))
import Data.Maybe (Maybe(..))
import Data.Exists (Exists, mkExists, runExists)
import Data.Newtype (class Newtype, unwrap)
import Data.Bifunctor (lmap)
import Test.QuickCheck.Arbitrary (class Arbitrary, class Coarbitrary, arbitrary)
import Test.QuickCheck.Gen (sized)

-- | A constructor taking the same arguments as `unfoldr`.
-- |
-- | Although this is part of the public API, you almost certainly do not want to use it
-- | directly, and it may be removed from the public API in the near future.
-- | Use `untrivial` if none of the existing utilities match your use case.
data UnfoldrCall a b = UnfoldrCall (b -> Maybe (a /\ b)) b

-- | A newtype wrapping `UnfoldrCall a b`, existentially quantified over the "seed" type `b`.
-- | Not meant to specifically be constructed directly--its `Unfoldable` instance
-- | is meant to be used to "intercept" `unfoldr` calls in other functions.
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

-- | Convenience function for inspecting `Trivial` values.
-- | Calls the function argument on the inner `UnfoldrCall`.
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

-- | Converts to any other `Unfoldable`.
-- | Can also be seen as evaluating the inner `UnfoldrCall`.
-- | 
-- | This is only useful in implementing utility functions.
-- | In all other cases, simply use the desired type directly.
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
