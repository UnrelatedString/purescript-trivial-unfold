module Data.Unfoldable.Trivial
 ( Trivial(..)
 , UnfoldrCall(..)
 , defaultUnfoldr1
 , uncons
 , head
 , tail
 , runTrivial
 ) where

import Prelude

import Data.Foldable (class Foldable, foldrDefault, foldMapDefaultL)
import Data.Unfoldable (class Unfoldable, class Unfoldable1, unfoldr, none)
import Data.Tuple (fst, snd)
import Data.Tuple.Nested ((/\), type (/\))
import Data.Maybe (Maybe(..), maybe)
import Data.Exists (Exists, mkExists, runExists)
import Data.Newtype (class Newtype, unwrap)
import Data.Bifunctor (lmap)

-- | A constructor taking the same arguments as `unfoldr`.
data UnfoldrCall a b = UnfoldrCall (b -> Maybe (a /\ b)) b

-- | A newtype wrapping `UnfoldrCall a b`, existentially quantified over the "seed" type `b`.
newtype Trivial a = Trivial (Exists (UnfoldrCall a))
derive instance Newtype (Trivial a) _

-- | Internal helper for implementing functions on Trivial.
untrivial :: forall a c. (forall b. UnfoldrCall a b -> c) -> Trivial a -> c
untrivial f = runExists f <<< unwrap

-- | Wraps both arguments to `unfoldr` in an `UnfoldrCall`.
instance trivialUnfoldable :: Unfoldable Trivial where
  unfoldr f seed = Trivial $ mkExists $ UnfoldrCall f seed

instance trivialUnfoldable1 :: Unfoldable1 Trivial where
  unfoldr1 = defaultUnfoldr1

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
defaultUnfoldr1 :: forall a b t. Unfoldable t => (b -> a /\ Maybe b) -> b -> t a
defaultUnfoldr1 f = unfoldr (map f) <<< Just

-- | Returns the first element and a new `Unfoldable` generating the remaining elements,
-- | or `Nothing` if there are no elements.
uncons :: forall a u. Unfoldable u => Trivial a -> Maybe (a /\ u a)
uncons = untrivial eUncons
  where eUncons :: forall b. UnfoldrCall a b -> Maybe (a /\ u a)
        eUncons (UnfoldrCall f seed) = f seed <#> map (unfoldr f)

-- | Returns the first element, if present.
head :: forall a. Trivial a -> Maybe a
head = untrivial eHead
  where eHead :: forall b. UnfoldrCall a b -> Maybe a
        eHead (UnfoldrCall f seed) = fst <$> f seed

-- | Removes the first element, if present.
tail :: forall a u. Unfoldable u => Trivial a -> u a
tail = maybe none snd <<< uncons

-- | Converts to any other `Unfoldable`.
-- | Can also be seen as "evaluating" the inner `UnfoldrCall`.
runTrivial :: forall a u. Unfoldable u => Trivial a -> u a
runTrivial = untrivial eRunTrivial
  where eRunTrivial :: forall b. UnfoldrCall a b -> u a
        eRunTrivial (UnfoldrCall f seed) = unfoldr f seed

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
