-- | This module provides the `Trivial1` type as an existentially quantified
-- | dumb wrapper around `unfold1`, which can be inspected and manipulated
-- | to implement various typeclasses and the utilities in Data.Unfoldable1.Trivia1.
-- | 
-- | This module also contains the implementations of utilities which rely on directly
-- | inspecting `Trivial1` values and are re-exported by Data.Unfoldable.Trivial1.
-- | Use this module directly only if you intend to directly inspect `Trivial1` values yourself.

module Data.Unfoldable1.Trivial1.Internal
 ( Trivial1
 , Generator1
 , untrivial1
 , trivial1
 , turbofish1
 , (::<+>)
 , uncons1
 --, take1
 , runTrivial1
 ) where

import Prelude

import Data.Foldable (class Foldable, foldrDefault, foldMapDefaultL, foldl)
import Data.Semigroup.Foldable (class Foldable1, foldr1Default, foldMap1DefaultL)
import Data.Unfoldable1 (class Unfoldable1, unfoldr1)
import Data.Unfoldable (class Unfoldable, none)
import Data.Tuple (fst, uncurry)
import Data.Tuple.Nested ((/\), type (/\))
import Data.Maybe (Maybe(..), maybe)
import Data.Exists (Exists, mkExists, runExists)
import Data.Bifunctor (lmap)
import Test.QuickCheck.Arbitrary (class Arbitrary, class Coarbitrary, arbitrary)
import Test.QuickCheck.Gen (sized)

import Data.Unfoldable.Trivial.Internal ((::<*>))

-- | Alias for the generator function passed to `unfoldr1`.
type Generator1 a b = b -> a /\ Maybe b

-- | A constructor taking the same arguments as `unfoldr1`.
-- |
-- | Implementation detail. No longer part of the public API.
data Unfoldr1Call a b = Unfoldr1Call (Generator1 a b) b

-- | A type wrapping `unfoldr1` calls, existentially quantified over the seed type
-- | so that it can be ignored in the type constructor. Its `Unfoldable1` instance
-- | means that it can directly be constructed by calling `unfoldr1`.
newtype Trivial1 a = Trivial1 (Exists (Unfoldr1Call a))

-- | Specializes its argument to `Trivial1`.
trivial1 :: forall a. Trivial1 a -> Trivial1 a
trivial1 = identity

-- | Function application specialized to a `Trivial1` argument,
-- | at the same precedence as `($)`.
-- |
-- | Inspired by the Rust syntax of the same name, often used in the
-- | analogous context of collecting from an iterator.
turbofish1 :: forall a b. (Trivial1 a -> b) -> Trivial1 a -> b
turbofish1 = identity

infixr 0 turbofish1 as ::<+>

-- | Convenience function for inspecting `Trivial` values.
-- | Calls the function argument on the contents of the inner `Exists`.
untrivial1 :: forall a c. (forall b. Generator1 a b -> b -> c) -> Trivial1 a -> c
untrivial1 f (Trivial1 e) = runExists (\(Unfoldr1Call g seed) -> f g seed) e

-- | Wraps both arguments to `unfoldr1` in an `Unfoldr1Call`.
instance trivial1Unfoldable1 :: Unfoldable1 Trivial1 where
  unfoldr1 f seed = Trivial1 $ mkExists $ Unfoldr1Call f seed

instance trivial1Functor :: Functor Trivial1 where
  map :: forall a c. (a -> c) -> Trivial1 a -> Trivial1 c
  map f = untrivial1 eMap
    where eMap :: forall b. Generator1 a b -> b -> Trivial1 c
          eMap g seed = Trivial1
                        $ mkExists
                        $ Unfoldr1Call (
                          lmap f <<< g
                        ) seed

-- | Converts to any other `Unfoldable1`.
-- | Can also be seen as evaluating the inner `Unfoldr1Call`.
-- | 
-- | This is only useful in implementing utility functions.
-- | In all other cases, simply use the desired type directly.
runTrivial1 :: forall a u. Unfoldable1 u => Trivial1 a -> u a
runTrivial1 = untrivial1 eRunTrivial1
  where eRunTrivial1 :: forall b. Generator1 a b -> b -> u a
        eRunTrivial1 f seed = unfoldr1 f seed

-- | Returns the first element, and an `Unfoldable` of the remaining elements.
uncons1 :: forall a u. Unfoldable u => Trivial1 a -> a /\ u a
uncons1 = untrivial1 eUncons1
  where eUncons1 :: forall b. Generator1 a b -> b -> a /\ u a
        eUncons1 f seed = f seed <#> maybe none (unfoldr1 f)

instance trivial1Foldable :: Foldable Trivial1 where
  foldl :: forall a c. (c -> a -> c) -> c -> Trivial1 a -> c
  foldl f foldInit = untrivial1 eFoldl
    where eFoldl :: forall b. Generator1 a b -> b -> c
          eFoldl g unfoldSeed = lockstep unfoldSeed foldInit
            where lockstep :: b -> c -> c
                  lockstep seed acc
                    | a /\ Just seed' <- g seed = lockstep seed' $ f acc a
                    | otherwise = f acc $ fst $ g seed

  foldr f = foldrDefault f
  foldMap f = foldMapDefaultL f

-- | The *raison d'être* for `Trivial1`.
-- | Allows folding polymorphic `Unfoldable1`s as they generate
-- | with no explicit starting value. In particular, `foldMap1`
-- | needs map only into a `Semigroup` rather than a `Monoid`.
-- |
-- | `foldr1` uses a default implementation and may be inefficient.
instance trivial1Foldable1 :: Foldable1 Trivial1 where
  -- I feel like there might be a cleaner way to do this that's still elegant but eh
  foldl1 :: forall a. (a -> a -> a) -> Trivial1 a -> a
  foldl1 f t = foldl f first ::<*> rest
    where first /\ rest = uncons1 t

  foldr1 f = foldr1Default f
  foldMap1 f = foldMap1DefaultL f

-- | Guaranteed finite.
instance trivialArbitrary :: (Arbitrary a, Coarbitrary a) => Arbitrary (Trivial1 a) where
  arbitrary = sized \size -> do
    (f :: a -> a /\ Maybe a) <- arbitrary 
    seed <- arbitrary
    pure $ unfoldr1 (uncurry \i b ->
      f b <#> \b' ->
        if i >= size
        then Nothing
        else ((i + 1) /\ _) <$> b'
    ) $ 0 /\ seed
