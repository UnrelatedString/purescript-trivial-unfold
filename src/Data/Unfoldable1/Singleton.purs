module Data.Unfoldable1.Singleton
 ( Singleton(..)
 ) where

-- | A container of exactly one element.
-- |
-- | Intentionally deficient in its typeclass implementations, because the only respect in
-- | which this is not entirely redundant with `Identity` is that `Identity` does not actually
-- | have an `Unfoldable1` instance. Should it ever gain one, this will be removed in the next
-- | major release.

import Prelude

import Data.Newtype (class Newtype)
