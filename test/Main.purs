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
import Data.Maybe (Maybe(..), isNothing)
import Data.Tuple (fst)
import Data.Monoid (guard)
import Data.Unfoldable (none, fromMaybe, replicate)
import Data.Unfoldable1 (singleton, replicate1)

main :: Effect Unit
main = runTest do
  smallSuite

smallSuite :: TestSuite
smallSuite = suite "small stuff" do
  test "single uncons" do
    Assert.assert "none should be empty" $ isNothing $ uncons none
    quickCheck \(x :: Int) -> fst <$> uncons (singleton x) === Just x
    quickCheck \(x :: Maybe String) -> fst <$> uncons (fromMaybe x) === x
    quickCheck \x -> fst <$> uncons (replicate x "ehehe") === guard (x > 0) (Just "ehehe")
    quickCheck \x (y :: Int) -> fst <$> uncons (replicate1 x y) === Just y
