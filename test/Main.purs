module Test.Main where

import Prelude

import Effect (Effect)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Main (runTest)
import Test.Unit.Assert as Assert
import Test.QuickCheck ((===))
import Test.Unit.QuickCheck (quickCheck)

import Data.Unfoldable.Trivial
  ( Trivial(..)
  , UnfoldrCall(..)
  , defaultUnfoldr1
  , uncons
  , runTrivial
  , foldEnum
  )
import Data.Maybe (Maybe(..))
import Data.Tuple (fst)
import Data.Unfoldable1 (singleton)

main :: Effect Unit
main = runTest do
  smallSuite

smallSuite :: TestSuite
smallSuite = suite "small stuff" do
  test "single uncons" do
    quickCheck \(x :: Int) -> fst <$> (uncons $ singleton x) === Just x
