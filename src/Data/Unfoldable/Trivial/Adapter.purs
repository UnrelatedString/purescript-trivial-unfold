-- | This module provides various adapters and other such utilities
-- | for `Unfoldable1` and `Unfoldable`.

module Data.Unfoldable.Trivial.Adapter
 ( module Reexports
 , index
 , drop
 , refoldl
 , refoldl1
 , refoldr
 , refoldr1
 , refoldMap
 , refoldMap1
 , refold
 , refold1
 , foldEnum
 , unfoldrInf
 , iterate
 ) where

import Data.Unfoldable.Trivial
  ( head
  , tail
  , take
  , unfoldr1Default
  , cons
  , snoc
  , turbofish
  , (::<*>)
  ) as Reexports
import Data.Unfoldable1.Trivial1
  (head1
  , tail1
  , take1
  , turbofish1
  , (::<+>)) as Reexports

import Prelude

import Data.Unfoldable.Trivial (Trivial, head, tail, cons) -- TODO: add take and drop for a more motivated example LMAO
import Data.Unfoldable1.Trivial1 (Trivial1, (::<+>))

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

-- | Keep only a number of elements from the start.
take :: forall a u. Unfoldable u => Int -> Trivial a -> u a

-- | Drop a number of elements from the start.
drop :: forall a u. Unfoldable u => Int -> Trivial a -> u a

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
iterate f seed = cons seed $ unfoldrInf (\a -> f a /\ f a) seed
