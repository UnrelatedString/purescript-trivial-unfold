-- | This module provides the `Trivial` type as an existentially quantified
-- | dumb wrapper around `unfold`, which can be inspected and manipulated
-- | to implement various typeclasses and the utilities in Data.Unfoldable.Trivial.
-- | 
-- | This module also contains the implementations of utilities which rely on directly
-- | inspecting `Trivial` values and are re-exported by Data.Unfoldable.Trivial.
-- | Use this module directly only if you intend to directly inspect `Trivial` values yourself.

module Data.Unfoldable.Trivial.Internal
  ( Trivial
  , Generator
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
import Data.Maybe (Maybe(..), maybe')
import Data.Exists (Exists, mkExists, runExists)
import Data.Bifunctor (lmap)
import Data.Functor.Invariant (class Invariant, imapF)
import Data.Compactable (class Compactable, separateDefault)
import Control.Lazy (class Lazy)
import Test.QuickCheck.Arbitrary (class Arbitrary, class Coarbitrary, arbitrary)
import Test.QuickCheck.Gen (sized)

-- | Alias for the generator function passed to `unfoldr`.
type Generator a b = b -> Maybe (a /\ b)

-- | A constructor taking the same arguments as `unfoldr`.
-- |
-- | Implementation detail. No longer part of the public API.
data UnfoldrCall a b = UnfoldrCall (Generator a b) b

-- | A type wrapping `unfoldr` calls, existentially quantified over the seed type
-- | so that it can be ignored in the type constructor. Its `Unfoldable` instance
-- | means that it can directly be constructed by calling `unfoldr`.
newtype Trivial a = Trivial (Exists (UnfoldrCall a))

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
-- | Calls the function argument on the contents of the inner `Exists`.
untrivial :: forall a c. (forall b. Generator a b -> b -> c) -> Trivial a -> c
untrivial f (Trivial e) = runExists (\(UnfoldrCall g seed) -> f g seed) e

-- | Wraps both arguments to `unfoldr` in an `UnfoldrCall`.
instance trivialUnfoldable :: Unfoldable Trivial where
  unfoldr f seed = Trivial $ mkExists $ UnfoldrCall f seed

instance trivialUnfoldable1 :: Unfoldable1 Trivial where
  unfoldr1 = unfoldr1Default

instance trivialFunctor :: Functor Trivial where
  map :: forall a c. (a -> c) -> Trivial a -> Trivial c
  map f = untrivial eMap
    where eMap :: forall b. Generator a b -> b -> Trivial c
          eMap g seed = Trivial
                        $ mkExists
                        $ UnfoldrCall (
                          map (lmap f) <<< g
                        ) seed

instance trivialInvariant :: Invariant Trivial where
  imap = imapF

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
  where eRunTrivial :: forall b. Generator a b -> b -> u a
        eRunTrivial f seed = unfoldr f seed

-- | The *raison d'être* for `Trivial`.
-- | Allows folding polymorphic `Unfoldable`s as they generate.
-- |
-- | `foldr` uses a default implementation and may be inefficient.
-- | Note also that this enables the use of `sequence_`
-- | despite the lack of a `Traversable` instance, which is not provided because
-- | it would require forcing and accumulating every value, at which point
-- | any would-be user is better off with an actual container.
instance trivialFoldable :: Foldable Trivial where
  foldl :: forall a c. (c -> a -> c) -> c -> Trivial a -> c
  foldl f foldInit = untrivial eFoldl
    where eFoldl :: forall b. Generator a b -> b -> c
          eFoldl g unfoldSeed = lockstep unfoldSeed foldInit
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

instance trivialLazy :: Lazy (Trivial a) where
  defer = flip identity unit

instance trivialCompactable :: Compactable Trivial where
  compact :: forall a. Trivial (Maybe a) -> Trivial a
  compact = untrivial eCompact
    where eCompact :: forall b. Generator (Maybe a) b -> Trivial a
          eCompact f seed = unfoldr filtering seed
            where filtering :: b -> Maybe (a /\ b)
                  filtering b
                    | Just (a /\ b') <- f b =
                        maybe'
                          (\_ -> filtering b')
                          (Just <<< (_ /\ b'))
                          a
                    | Nothing <- f b = Nothing
  separate = separateDefault
