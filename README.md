[![CI](https://github.com/UnrelatedString/purescript-trivial-unfold/actions/workflows/ci.yml/badge.svg?branch=main)](https://github.com/UnrelatedString/purescript-trivial-unfold/actions/workflows/ci.yml)
![Latest Version Tag](https://img.shields.io/github/v/tag/UnrelatedString/purescript-trivial-unfold)
[![Pursuit](https://pursuit.purescript.org/packages/purescript-trivial-unfold/badge)](https://pursuit.purescript.org/packages/purescript-trivial-unfold)

# purescript-trivial-unfold

A simple library providing a number of utilities in `Data.Unfoldable.Trivial` implemented with existentially typed "trivial" wrappers around `unfoldr` and `unfoldr1` calls, intended as adapters from functions in other libraries with polymorphic `forall u. Unfoldable u => a` or `forall u. Unfoldable1 u => u a` result types such as `upFrom` in `purescript-enums`.

```purescript
import Effect.Console (logShow)
import Data.Enum (upFrom)
import Data.Maybe (Maybe(..))
import Data.Array (toUnfoldable)
import Data.Foldable (intercalate)
import Data.Monoid (guard)
import Data.Unfoldable (unfoldr1)
import Data.Multiplicative (Multiplicative(..))

import Data.Unfoldable.Trivial (index, drop, refold1, (::<*>))

main = do

  -- Index into a very large range without evaluating all of it.
  logShow $ index (upFromIncluding 'A') $ 32 + 25
  -- > Just 'z'

  -- Fold over a suffix of an Array without constructing a new Array for the suffix.
  -- The (::<*>) operator is ($) specialized to Trivial,
  -- to conveniently make instances decidable.
  -- (Note that this can also be accomplished with Data.List.Lazy.)
  logShow $ intercalate " " ::<*> drop 2 $ toUnfoldable [
    "I'm", "never", "gonna", "give", "you", "up"
  ]
  -- > "gonna give you up"

  -- Fold directly from a generating function.
  -- Basic folds are also provided specialized, with the "re-" prefix.
  logShow $ refold1 $ flip unfoldr1 1 \n -> Multiplicative n /\ (guard (n < 6) $> n + 1)
  -- > Multiplicative 620
```

# Installation

    spago install trivial-unfold

# Documentation

Module documentation is [published on Pursuit](http://pursuit.purescript.org/packages/purescript-trivial-unfold).
