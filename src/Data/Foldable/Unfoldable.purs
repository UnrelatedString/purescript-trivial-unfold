module Data.Foldable.Unfoldable
 ( TrivialUnfold
 , foldEnum
 ) where

import Prelude

import Data.Foldable (class Foldable, foldMap, foldrDefault, foldMapDefaultL)
import Data.Unfoldable (class Unfoldable, class Unfoldable1, unfoldr)
import Data.Tuple.Nested ((/\), type (/\))
import Data.Maybe (Maybe(..))
import Data.Exists (Exists, mkExists, runExists)
import Data.Enum (class BoundedEnum, upFromIncluding)

data Unfolder a b = Unfolder (b -> Maybe (a /\ b)) b
newtype TrivialUnfold a = TrivialUnfold (Exists (Unfolder a))

instance Unfoldable TrivialUnfold where
  unfoldr f b = TrivialUnfold $ mkExists $ Unfolder f b

instance Unfoldable1 TrivialUnfold where
  unfoldr1 :: forall a b. (b -> a /\ Maybe b) -> b -> TrivialUnfold a
  unfoldr1 f = unfoldr adapter <<< Just
    where adapter :: Maybe b -> Maybe (a /\ Maybe b)
          adapter = map f

-- can't actually define a Foldable Unfolder instance because the types are backwards :(
foldlUnfolder :: forall a b c. (c -> a -> c) -> c -> Unfolder a b -> c
foldlUnfolder f foldInit (Unfolder g unfoldSeed) = lockstep unfoldSeed foldInit
  where lockstep :: b -> c -> c
        lockstep seed acc
          | Just (a /\ seed') <- g seed = lockstep seed' $ f acc a
          | otherwise = acc

instance Foldable TrivialUnfold where
  foldl f b (TrivialUnfold e) = runExists (foldlUnfolder f b) e
  foldr f = foldrDefault f
  foldMap f = foldMapDefaultL f

foldEnum :: forall a b. BoundedEnum a => Monoid b => (a -> b) -> b
foldEnum f = foldMap f everything
  where everything :: TrivialUnfold a
        everything = upFromIncluding bottom
