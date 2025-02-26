-- | This "module" provides various adapters and other such utilities
-- | for `Unfoldable1` and `Unfoldable`.
>:3
module Data.Unfoldable.Trivial
 ( module Reexports
--  , head
--  , tail
-- , take
--  , cons
--  , snoc
 , index
-- , drop
 , refoldl
 , refoldr
 , refoldMap
 , refold
 ) where

import Data.Unfoldable.Trivial.Internal
  ( unfoldr1Default
  , head
  , tail
  , cons
  , snoc
  , trivial
  , turbofish
  , (::<*>)
  ) as Reexports
import Data.Unfoldable1.Trivial1 as Reexports

import Prelude

import Data.Unfoldable.Trivial.Internal (Trivial, head, tail, cons)
import Data.Unfoldable1.Trivial1.Internal (Trivial1, (::<+>))

import Data.Unfoldable1 (class Unfoldable1, unfoldr1)
import Data.Foldable (foldl, foldr, foldMap, fold)
import Data.Semigroup.Foldable (foldl1, foldr1, foldMap1, fold1)
import Data.Maybe (Maybe(..))
import Data.Enum (class BoundedEnum, upFromIncluding)
import Data.Tuple.Nested ((/\), type (/\))

-- | Get the element at the specified 0-index, or `Nothing` if the index is out-of-bounds.
-- |
-- | Time complexity: `O(n)` in the index (calls to the generating function).
-- | (Does not terminate early if it goes past the end!)
index :: forall a. Trivial a -> Int -> Maybe a
index t i
  | i < 0 = Nothing
  | i == 0 = head t
  | otherwise = index (tail t) (i - 1)

-- -- | Keep only a number of elements from the start.
-- take :: forall a u. Unfoldable u => Int -> Trivial a -> u a

-- -- | Drop a number of elements from the start.
-- drop :: forall a u. Unfoldable u => Int -> Trivial a -> u a

-- | `foldl` specialized to `Trivial`. "Re-fold" a polymorphic `Unfoldable`.
-- | Usually cleaner and more convenient than `turbofish`, when applicable.
refoldl :: forall a c. (c -> a -> c) -> c -> Trivial a -> c
refoldl = foldl

-- | `foldr` specialized to `Trivial`. "Re-fold" a polymorphic `Unfoldable`.
-- | Usually cleaner and more convenient than `turbofish`, when applicable.
refoldr :: forall a c. (a -> c -> c) -> c -> Trivial a -> c
refoldr = foldr

-- | `foldMap` specialized to `Trivial`. "Re-fold" a polymorphic `Unfoldable`.
-- | Usually cleaner and more convenient than `turbofish`, when applicable.
refoldMap :: forall a c. Monoid c => (a -> c) -> Trivial a -> c
refoldMap = foldMap

-- | `fold` specialized to `Trivial`. "Re-fold" a polymorphic `Unfoldable`.
-- | Usually cleaner and more convenient than `turbofish`, when applicable.
refold :: forall a. Monoid a => Trivial a -> a
refold = fold
