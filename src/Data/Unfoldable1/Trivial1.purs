module Data.Unfoldable1.Trivial1
 ( Trivial1(..)
 , Unfoldr1Call(..)
 , trivial1
 , uncons1
 , head1
 , tail1
 , runTrivial1
 ) where

import Prelude

import Data.Foldable (class Foldable, foldrDefault, foldMapDefaultL, foldl)
import Data.Semigroup.Foldable (class Foldable1, foldr1Default, foldMap1DefaultL)
import Data.Unfoldable1 (class Unfoldable1, unfoldr1)
import Data.Unfoldable (class Unfoldable, none)
import Data.Tuple (fst, snd)
import Data.Tuple.Nested ((/\), type (/\))
import Data.Maybe (Maybe(..), maybe)
import Data.Exists (Exists, mkExists, runExists)
import Data.Newtype (class Newtype, unwrap)
import Data.Bifunctor (lmap)

import Data.Unfoldable.Trivial (trivial)

-- | A constructor taking the same arguments as `unfoldr1`.
data Unfoldr1Call a b = Unfoldr1Call (b -> (a /\ Maybe b)) b

-- | A newtype wrapping `Unfoldr1Call a b`, existentially quantified over the "seed" type `b`.
newtype Trivial1 a = Trivial1 (Exists (Unfoldr1Call a))
derive instance Newtype (Trivial1 a) _

-- | Coerces its argument to `Trivial1`.
trivial1 :: forall a. Trivial1 a -> Trivial1 a
trivial1 = identity

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

-- | Returns the first element, and an `Unfoldable` of the remaining elements.
uncons1 :: forall a u. Unfoldable u => Trivial1 a -> a /\ u a
uncons1 = untrivial1 eUncons1
  where eUncons1 :: forall b. Unfoldr1Call a b -> a /\ u a
        eUncons1 (Unfoldr1Call f seed) = f seed <#> maybe none (unfoldr1 f)

-- | Returns the first element.
head1 :: forall a. Trivial1 a -> a
head1 = untrivial1 eHead1
  where eHead1 :: forall b. Unfoldr1Call a b -> a
        eHead1 (Unfoldr1Call f seed) = fst $ f seed

-- | Removes the first element.
-- embarrassed how long it took me to realize. I can just.
-- let other Unfoldables use their own Unfoldable1 instances.
-- instead of manually adapting Maybe Trivial1 into Trivial or some shit like that
tail1 :: forall a u. Unfoldable u => Trivial1 a -> u a
tail1 = snd <<< uncons1

-- | Converts to any other `Unfoldable1`.
-- | Can also be seen as "evaluating" the inner `Unfoldr1Call`.
runTrivial1 :: forall a u. Unfoldable1 u => Trivial1 a -> u a
runTrivial1 = untrivial1 eRunTrivial1
  where eRunTrivial1 :: forall b. Unfoldr1Call a b -> u a
        eRunTrivial1 (Unfoldr1Call f seed) = unfoldr1 f seed

instance trivial1Foldable :: Foldable Trivial1 where
  foldl :: forall a c. (c -> a -> c) -> c -> Trivial1 a -> c
  foldl f foldInit = untrivial1 eFoldl
    where eFoldl :: forall b. Unfoldr1Call a b -> c
          eFoldl (Unfoldr1Call g unfoldSeed) = lockstep unfoldSeed foldInit
            where lockstep :: b -> c -> c
                  lockstep seed acc
                    | a /\ Just seed' <- g seed = lockstep seed' $ f acc a
                    | otherwise = f acc $ fst $ g seed

  foldr f = foldrDefault f
  foldMap f = foldMapDefaultL f

instance trivial1Foldable1 :: Foldable1 Trivial1 where
  -- I feel like there might be a cleaner way to do this that's still elegant but eh
  foldl1 :: forall a. (a -> a -> a) -> Trivial1 a -> a
  foldl1 f t = foldl f (head1 t) $ trivial $ tail1 t

  foldr1 f = foldr1Default f
  foldMap1 f = foldMap1DefaultL f
