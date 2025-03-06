module Data.Unfoldable.MaybeEmpty
 ( MaybeEmpty(..)
 ) where

import Prelude

import Data.Newtype (class Newtype, over)
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Data.Unfoldable
 ( class Unfoldable1
 , class Unfoldable
 , unfoldr1
 , unfoldr
 )
import Data.Unfoldable.Trivial (cons)

-- | Lift an `Unfoldable1` into an `Unfoldable` by wrapping it in `Maybe`,
-- | such that an empty sequence gives `Nothing` and a nonempty sequence gives
-- | a nonempty `Unfoldable1` in `Just`.
newtype MaybeEmpty :: forall k. (k -> Type) -> k -> Type
newtype MaybeEmpty f a = MaybeEmpty (Maybe (f a))
derive instance Newtype (MaybeEmpty f a) _

instance maybeEmptyUnfoldable1 :: Unfoldable1 f => Unfoldable1 (MaybeEmpty f) where
  unfoldr1 = (<<<) (MaybeEmpty <<< Just) <<< unfoldr1

instance maybeEmptyUnfoldable :: Unfoldable1 f => Unfoldable (MaybeEmpty f) where
  unfoldr f seed
    | Just (a /\ seed') <- f seed = MaybeEmpty $ Just $ cons a $ unfoldr f seed'
    | otherwise = MaybeEmpty Nothing

instance maybeEmptyFunctor :: Functor f => Functor (MaybeEmpty f) where
  map = over MaybeEmpty <<< map <<< map
