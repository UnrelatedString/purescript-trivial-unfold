-- | This module provides various adapters and other such utilities
-- | for `Unfoldable1`.

module Data.Unfoldable1.Trivial1
 ( module Reexports
 , refoldl1
 , refoldr1
 , refoldMap1
 , refold1
 , foldEnum
 , unfoldrInf
 , iterate
 , head1
 , tail1
 --, take1
 ) where

import Data.Unfoldable1.Trivial1.Internal
  ( Trivial1
  , trivial1
  , turbofish1
  , (::<+>)
  , uncons1) as Reexports

import Prelude

import Data.Unfoldable1.Trivial1.Internal
 ( Trivial1
 , (::<+>)
 , untrivial1
 , Unfoldr1Call(..)
 , uncons1
 )

import Data.Unfoldable (class Unfoldable)
import Data.Unfoldable1 (class Unfoldable1, unfoldr1)
import Data.Semigroup.Foldable (foldl1, foldr1, foldMap1, fold1)
import Data.Maybe (Maybe(..))
import Data.Enum (class BoundedEnum, upFromIncluding)
import Data.Tuple (fst, snd)
import Data.Tuple.Nested ((/\), type (/\))

-- | Returns the first element.
head1 :: forall a. Trivial1 a -> a
head1 = untrivial1 eHead1
  where eHead1 :: forall b. Unfoldr1Call a b -> a
        eHead1 (Unfoldr1Call f seed) = fst $ f seed

-- | Removes the first element.
tail1 :: forall a u. Unfoldable u => Trivial1 a -> u a
tail1 = snd <<< uncons1

-- -- | Keep only a strictly positive number of elements from the start.
-- take1 :: forall a u. Unfoldable1 u => Int -> Trivial1 a -> u a

-- | `foldl1` specialized to `Trivial1`. "Re-fold" a polymorphic `Unfoldable1`.
-- | Usually cleaner and more convenient than `turbofish`, when applicable.
refoldl1 :: forall a. (a -> a -> a) -> Trivial1 a -> a
refoldl1 = foldl1

-- | `foldr1` specialized to `Trivial1`. "Re-fold" a polymorphic `Unfoldable1`.
-- | Usually cleaner and more convenient than `turbofish`, when applicable.
refoldr1 :: forall a. (a -> a -> a) -> Trivial1 a -> a
refoldr1 = foldr1

-- | `foldMap` specialized to `Trivial1`. "Re-fold" a polymorphic `Unfoldable1`.
-- | Usually cleaner and more convenient than `turbofish1`, when applicable.
refoldMap1 :: forall a c. Semigroup c => (a -> c) -> Trivial1 a -> c
refoldMap1 = foldMap1

-- | `fold` specialized to `Trivial1`. "Re-fold" a polymorphic `Unfoldable1`.
-- | Usually cleaner and more convenient than `turbofish1`, when applicable.
refold1 :: forall a. Semigroup a => Trivial1 a -> a
refold1 = fold1

-- | Map each element of a `BoundedEnum` into a semigroup,
-- | and combine the results through `refold1`.
foldEnum :: forall a b. BoundedEnum a => Semigroup b => (a -> b) -> b
foldEnum = flip foldMap1 ::<+> upFromIncluding bottom

-- | Unfold an infinite `Unfoldable1`.
-- | Analogous to `unfold1` and `unfold`, but with no way to signal termination;
-- | `unfoldInf f b` consists of `fst $ f b` appended to `unfoldInf f $ snd $ f b`.
-- |
-- | This should only be used to produce either lazy types (like `Trivial`) or
-- | types with truncating `Unfoldable1` instances (like `Maybe`).
unfoldrInf :: forall a b u. Unfoldable1 u => (b -> a /\ b) -> b -> u a
unfoldrInf = unfoldr1 <<< (map Just <<< _)

-- | Create an infinite `Unfoldable1` by repeated application of a function to a seed value. 
-- | Analogous to `iterateN`, but with no iteration limit.
-- |
-- | This should only be used to produce either lazy types (like `Trivial`) or
-- | types with truncating `Unfoldable1` instances (like `Maybe`).
iterate :: forall a u. Unfoldable1 u => (a -> a) -> a -> u a
iterate f seed = unfoldr1 (map \a -> Just (f a /\ f a)) $ seed /\ seed
