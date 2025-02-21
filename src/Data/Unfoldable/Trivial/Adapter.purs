module Data.Unfoldable.Trivial.Adapter
 ( module Reexports
 , index
 , foldEnum
 ) where

import Prelude

import Data.Unfoldable.Trivial (Trivial, head, tail)
import Data.Unfoldable1.Trivial1 (turbofish1)
import Data.Unfoldable.Trivial (head, tail, defaultUnfoldr1) as Reexports
import Data.Unfoldable1.Trivial1 (head1, tail1) as Reexports

import Data.Semigroup.Foldable (foldMap1)
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

-- | Map each element of a `BoundedEnum` into a semigroup through `Trivial1`,
-- | and combine the results.
foldEnum :: forall a b. BoundedEnum a => Semigroup b => (a -> b) -> b
foldEnum = flip foldMap1 $ turbofish1 $ upFromIncluding bottom
