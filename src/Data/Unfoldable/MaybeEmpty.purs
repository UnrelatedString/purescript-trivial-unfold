module Data.Unfoldable.MaybeEmpty
 ( MaybeEmpty(..)
 ) where

import Data.Newtype (class Newtype)
import Data.Maybe (Maybe)

newtype MaybeEmpty f a = MaybeEmpty (Maybe (f a))
derive instance Newtype (MaybeEmpty f a) _

