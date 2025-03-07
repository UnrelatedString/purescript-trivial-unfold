module Data.Unfoldable.MaybeEmpty
 ( MaybeEmpty(..)
 ) where

import Prelude

import Data.Newtype (class Newtype, over, unwrap)
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Control.Apply (lift2)
import Data.Functor.Invariant (class Invariant, imapF)
import Data.Generic.Rep (class Generic)
import Data.Traversable (class Traversable, traverse)
import Test.QuickCheck.Arbitrary (class Arbitrary, class Coarbitrary)
import Control.Alternative (class Alt, class Plus, class Alternative)
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
derive newtype instance Show (f a) => Show (MaybeEmpty f a)
derive newtype instance Arbitrary (f a) => Arbitrary (MaybeEmpty f a)
derive newtype instance Coarbitrary (f a) => Coarbitrary (MaybeEmpty f a)
derive newtype instance Semigroup (f a) => Semigroup (MaybeEmpty f a)
derive newtype instance Semigroup (f a) => Monoid (MaybeEmpty f a)
derive newtype instance Bounded (f a) => Bounded (MaybeEmpty f a)
derive newtype instance Semiring (f a) => Semiring (MaybeEmpty f a)
derive newtype instance Eq (f a) => Eq (MaybeEmpty f a)
derive newtype instance Ord (f a) => Ord (MaybeEmpty f a)
derive instance Generic (MaybeEmpty f a) _

instance maybeEmptyUnfoldable1 :: Unfoldable1 f => Unfoldable1 (MaybeEmpty f) where
  unfoldr1 = (<<<) (MaybeEmpty <<< Just) <<< unfoldr1

instance maybeEmptyUnfoldable :: Unfoldable1 f => Unfoldable (MaybeEmpty f) where
  unfoldr f seed
    | Just (a /\ seed') <- f seed = MaybeEmpty $ Just $ cons a $ unfoldr f seed'
    | otherwise = MaybeEmpty Nothing

instance maybeEmptyFunctor :: Functor f => Functor (MaybeEmpty f) where
  map = over MaybeEmpty <<< map <<< map

instance maybeEmptyApply :: Apply f => Apply (MaybeEmpty f) where
  apply (MaybeEmpty f) (MaybeEmpty a) = MaybeEmpty $ lift2 apply f a

instance maybeEmptyApplicative :: Applicative f => Applicative (MaybeEmpty f) where
  pure = MaybeEmpty <<< pure <<< pure

instance maybeEmptyBind :: (Bind f, Traversable f) => Bind (MaybeEmpty f) where
  bind (MaybeEmpty a) f = MaybeEmpty $ bind a $ map join <<< traverse (unwrap <<< f)

instance maybeEmptyMonad :: (Applicative f, Bind f, Traversable f) => Monad (MaybeEmpty f)

instance maybeEmptyInvariant :: Functor f => Invariant (MaybeEmpty f) where
  imap = imapF
