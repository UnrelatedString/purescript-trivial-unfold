module Data.Unfoldable.Newtype
 ( NothingIfNone(..)
 ) where

import Data.Newtype (class Newtype)
import Data.Maybe (Maybe)

newtype NothingIfNone f a = NothingIfNone (Maybe (f a))
derive instance Newtype (NothingIfNone f a) _
