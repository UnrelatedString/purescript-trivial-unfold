module Data.Unfoldable1.Trivial1
 ( Trivial1(..)
 , Unfoldr1Call(..)
 , uncons1
 , runTrivial1
--  , foldEnum
 ) where

import Prelude

import Data.Foldable (class Foldable, foldMap, foldrDefault, foldMapDefaultL)
import Data.Unfoldable1 (class Unfoldable1, unfoldr1)
import Data.Unfoldable (class Unfoldable, unfoldr)
import Data.Tuple.Nested ((/\), type (/\))
import Data.Maybe (Maybe(..))
import Data.Exists (Exists, mkExists, runExists)
import Data.Enum (class BoundedEnum, upFromIncluding)
import Data.Newtype (class Newtype, unwrap)
import Data.Bifunctor (lmap)

-- | A constructor taking the same arguments as `unfoldr1`.
data Unfoldr1Call a b = Unfoldr1Call (b -> (a /\ Maybe b)) b

-- | A newtype wrapping `Unfoldr1Call a b`, existentially quantified over the "seed" type `b`.
newtype Trivial1 a = Trivial1 (Exists (Unfoldr1Call a))
derive instance Newtype (Trivial1 a) _

-- | Internal helper for implementing functions on Trivial1.
untrivial1 :: forall a c. (forall b. Unfoldr1Call a b -> c) -> Trivial1 a -> c
untrivial1 f = runExists f <<< unwrap

-- | Wraps both arguments to `unfoldr1` in an `Unfoldr1Call`.
instance trivial1Unfoldable1 :: Unfoldable1 Trivial1 where
  unfoldr1 f seed = Trivial1 $ mkExists $ Unfoldr1Call f seed

instance trivial1Functor :: Functor Trivial1 where
  map :: forall a c. (a -> c) -> Trivial1 a -> Trivial1 c
  map f = untrivial1 eMap
    where eMap :: forall b. Unfoldr1Call a b -> Trivial1 c
          eMap (Unfoldr1Call g seed) = Trivial1
                                     $ mkExists
                                     $ Unfoldr1Call (
                                       lmap f <<< g
                                     ) seed

-- | Returns the next element, and a new `Trivial1` generating the remaining elements
-- | if there are any elements remaining.
uncons1 :: forall a. Trivial1 a -> a /\ Maybe (Trivial1 a)
uncons1 = untrivial1 eUncons1
  where eUncons1 :: forall b. Unfoldr1Call a b -> a /\ Maybe (Trivial1 a)
        eUncons1 (Unfoldr1Call f seed) = f seed <#> map (unfoldr1 f)

-- | Converts to any other `Unfoldable1`.
-- | Can also be seen as "evaluating" the inner `Unfoldr1Call`.
runTrivial1 :: forall a u. Unfoldable1 u => Trivial1 a -> u a
runTrivial1 = untrivial1 eRunTrivial1
  where eRunTrivial1 :: forall b. Unfoldr1Call a b -> u a
        eRunTrivial1 (Unfoldr1Call f seed) = unfoldr1 f seed
