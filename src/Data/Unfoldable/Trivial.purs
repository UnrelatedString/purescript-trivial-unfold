-- | This "module" provides various adapters and other such utilities
-- | for `Unfoldable1` and `Unfoldable`.

module Data.Unfoldable.Trivial
 ( module Reexports
 , head
 , tail
 , take
 , cons
 , snoc
 , uncons
 , index
 , drop
 , refoldl
 , refoldr
 , refoldMap
 , refold
 ) where

import Data.Unfoldable.Trivial.Internal
  ( unfoldr1Default
  , trivial
  , turbofish
  , (::<*>)
  ) as Reexports
import Data.Unfoldable1.Trivial1
 ( Trivial1
 , trivial1
 , turbofish1
 , (::<+>)
 , uncons1
 , refoldl1
 , refoldr1
 , refoldMap1
 , refold1
 , foldEnum
 , unfoldrInf
 , iterate
 , head1
 , tail1
 , take1
 , index1
 ) as Reexports

import Prelude

import Data.Unfoldable.Trivial.Internal (Trivial, Generator, untrivial, runTrivial)

import Data.Unfoldable(class Unfoldable, unfoldr, none)
import Data.Unfoldable1 (class Unfoldable1, unfoldr1)
import Data.Foldable (foldl, foldr, foldMap, fold)
import Data.Maybe (Maybe(..), maybe, isNothing)
import Data.Tuple (snd)
import Data.Tuple.Nested ((/\), type (/\))

-- | Returns the first element and a new `Unfoldable` generating the remaining elements,
-- | or `Nothing` if there are no elements.
uncons :: forall a u. Unfoldable u => Trivial a -> Maybe (a /\ u a)
uncons = untrivial eUncons
  where eUncons :: forall b. Generator a b -> b -> Maybe (a /\ u a)
        eUncons f seed = f seed <#> map (unfoldr f)

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

-- | Get the element at the specified 0-index, or `Nothing` if the index is out-of-bounds.
-- |
-- | Time complexity: `O(n)` in the index (calls to the generating function).
index :: forall a. Trivial a -> Int -> Maybe a
index t i
  | i < 0 = Nothing
  | i == 0 || isNothing (head t) = head t
  | otherwise = index (tail t) (i - 1)

-- | Keep only a number of elements from the start.
take :: forall a u. Unfoldable u => Int -> Trivial a -> u a
take n = untrivial eTake
  where eTake :: forall b. Generator a b -> b -> u a
        eTake f seed = unfoldr taker $ n /\ seed
          where taker :: Generator a (Int /\ b)
                taker (m /\ b)
                  | m <= 0 = Nothing
                  | otherwise = map ((/\) (m - 1)) <$> f b

-- | Drop a number of elements from the start.
-- Surprised the old version didn't tail call optimize but this is smarter/lazier anyways
-- TODO: nicer kinda-maybe-lazier-feeling impl with compact when I add that
drop :: forall a u. Unfoldable u => Int -> Trivial a -> u a
drop n = untrivial eDrop 
  where eDrop :: forall b. Generator a b -> b -> u a
        eDrop f seed = unfoldr dropper $ n /\ seed
          where dropper :: Generator a (Int /\ b)
                dropper (m /\ b)
                  | m <= 0 = map (m /\ _) <$> f b
                  | otherwise = dropper =<< ((/\) (m - 1)) <$> snd <$> (f b)

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

-- | Prepend an element.
-- |
-- | Do not use this to create a data structure. Please use Data.List.Lazy instead.
cons :: forall a u. Unfoldable1 u => a -> Trivial a -> u a
cons h t = untrivial eCons t
  where eCons :: forall b. Generator a b -> b -> u a
        eCons f seed = unfoldr1 hilbertHotel $ h /\ seed
          where hilbertHotel :: a /\ b -> a /\ Maybe (a /\ b)
                hilbertHotel = map f

-- | Append an element.
-- |
-- | Do not use this to create a data structure. Please use Data.List.Lazy instead.
snoc :: forall a u. Unfoldable1 u => Trivial a -> a -> u a
snoc t l = untrivial eSnoc t
  where eSnoc :: forall b. Generator a b -> b -> u a
        eSnoc f seed = unfoldr1 failsafed seed
          where failsafed :: b -> a /\ Maybe b
                failsafed b
                  | Just (a /\ b') <- f b = a /\ Just b'
                  | otherwise = l /\ Nothing
