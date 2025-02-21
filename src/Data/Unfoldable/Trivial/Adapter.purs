module Data.Unfoldable.Trivial.Adapter
 ( module Reexports
 , index
 , refoldl
 , refoldl1
 , refoldMap
 , refoldMap1
 , refold
 , refold1
 , foldEnum
 ) where

import Prelude

import Data.Unfoldable.Trivial (Trivial, head, tail)
import Data.Unfoldable1.Trivial1 (Trivial1, (::<+>))
import Data.Unfoldable.Trivial (head, tail, defaultUnfoldr1) as Reexports
import Data.Unfoldable1.Trivial1 (head1, tail1) as Reexports

import Data.Foldable (foldl, foldMap, fold)
import Data.Semigroup.Foldable (foldl1, foldMap1, fold1)
import Data.Maybe (Maybe(..))
import Data.Enum (class BoundedEnum, upFromIncluding)

-- | Get the element at the specified 0-index, or `Nothing` if the index is out-of-bounds.
-- |
-- | Time complexity: `O(n)` in the index. (Does not terminate early if it goes past the end!)
index :: forall a. Trivial a -> Int -> Maybe a
index t i
  | i < 0 = Nothing
  | i == 0 = head t
  | otherwise = index (tail t) (i - 1)

-- | `foldl` specialized to `Trivial`. "Re-fold" a polymorphic `Unfoldable`.
-- | Preferable over the relatively "low-level" use of `turbofish`,
-- | which is not re-exported from this module for that reason.
refoldl :: forall a c. (c -> a -> c) -> c -> Trivial a -> c
refoldl = foldl

-- | `foldMap` specialized to `Trivial`. "Re-fold" a polymorphic `Unfoldable`.
-- | May be preferable over the relatively "low-level" use of `turbofish`,
-- | which is not re-exported from this module for that reason.
refoldMap :: forall a c. Monoid c => (a -> c) -> Trivial a -> c
refoldMap = foldMap

-- | `fold` specialized to `Trivial`. "Re-fold" a polymorphic `Unfoldable`.
-- | May be preferable over the relatively "low-level" use of `turbofish`,
-- | which is not re-exported from this module for that reason.
refold :: forall a. Monoid a => Trivial a -> a
refold = fold

-- | `foldl1` specialized to `Trivial1`. "Re-fold" a polymorphic `Unfoldable1`.
-- | Preferable over the relatively "low-level" use of `turbofish1`,
-- | which is not re-exported from this module for that reason.
refoldl1 :: forall a. (a -> a -> a) -> Trivial1 a -> a
refoldl1 = foldl1

-- | `foldMap` specialized to `Trivial1`. "Re-fold" a polymorphic `Unfoldable1`.
-- | May be preferable over the relatively "low-level" use of `turbofish1`,
-- | which is not re-exported from this module for that reason.
refoldMap1 :: forall a c. Semigroup c => (a -> c) -> Trivial1 a -> c
refoldMap1 = foldMap1

-- | `fold` specialized to `Trivial1`. "Re-fold" a polymorphic `Unfoldable1`.
-- | May be preferable over the relatively "low-level" use of `turbofish1`,
-- | which is not re-exported from this module for that reason.
refold1 :: forall a. Semigroup a => Trivial1 a -> a
refold1 = fold1

-- | Map each element of a `BoundedEnum` into a semigroup,
-- | and combine the results through `refold1`.
foldEnum :: forall a b. BoundedEnum a => Semigroup b => (a -> b) -> b
foldEnum = flip foldMap1 ::<+> upFromIncluding bottom
