module Data.Unfoldable.Trivial.Adapter
 ( index
 , foldEnum
 ) where

import Prelude

import Data.Unfoldable.Trivial (Trivial, head, tail)
import Data.Unfoldable1.Trivial1 (turbofish1)

import Data.Semigroup.Foldable (foldMap1)
import Data.Maybe (Maybe(Nothing))
import Data.Enum (class BoundedEnum, upFromIncluding)


-- | Get the element at the specified 0-index, or `Nothing` if the index is out-of-bounds.
-- |
-- | Time complexity: `O(n)` in the index.
index :: forall a. Trivial a -> Int -> Maybe a
index t i
  | i < 0 = Nothing
  | otherwise = index (tail t) (i - 1)

-- | Map each element of a `BoundedEnum` into a semigroup through `Trivial1`,
-- | and combine the results.
foldEnum :: forall a b. BoundedEnum a => Semigroup b => (a -> b) -> b
foldEnum = flip foldMap1 $ turbofish1 $ upFromIncluding bottom
