module Test.Main where

import Prelude

import Effect (Effect)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Main (runTest)
import Test.Unit.Assert as Assert
import Test.QuickCheck ((===))
import Test.QuickCheck.Arbitrary (class Arbitrary)
import Test.Unit.QuickCheck (quickCheck)

import Data.Unfoldable.Trivial
 ( Trivial(..)
 , UnfoldrCall(..)
 , defaultUnfoldr1
 , uncons
 , head
 , tail
 , runTrivial
 )
import Data.Unfoldable1.Trivial1
 ( Trivial1(..)
 , Unfoldr1Call(..)
 , uncons1
 , head1
 , tail1
 , runTrivial1
 , foldEnum
 )

import Data.Maybe (Maybe(..), isJust, isNothing)
import Data.Enum (class Enum, class BoundedEnum)
import Data.Tuple (fst, snd)
import Data.Tuple.Nested ((/\), type (/\))
import Data.Monoid (guard)
import Data.Semigroup.First (First(..))
import Data.Semigroup.Last (Last(..))
import Data.Unfoldable (none, fromMaybe, replicate)
import Data.Unfoldable1 (singleton, replicate1)
import Data.Newtype (unwrap)
import Type.Proxy (Proxy(..))

main :: Effect Unit
main = runTest do
  smallSuite
  enumSuite

smallSuite :: TestSuite
smallSuite = suite "small stuff" do
  test "single uncons" do
    Assert.assert "none should be empty" $ isNothing $ uncons none
    quickCheck \(x :: Int) -> fst <$> uncons (singleton x) === Just x
    quickCheck \(x :: Maybe String) -> fst <$> uncons (fromMaybe x) === x
    quickCheck \x -> fst <$> uncons (replicate x "ehehe") === guard (x > 0) (Just "ehehe")
    quickCheck \x (y :: Int) -> fst <$> uncons (replicate1 x y) === Just y
  test "double uncons" do
    let double :: forall a. Trivial a -> Maybe (a /\ Maybe a)
        double = map (map $ map fst <<< uncons) <<< uncons
    quickCheck \(x :: Int) -> double (singleton x) === Just (x /\ Nothing)
    quickCheck \x -> isJust (snd =<< double (replicate1 x unit)) === (x > 1)
  test "Maybe round trip" do
    quickCheck \(x :: Maybe Char) -> runTrivial (fromMaybe x) === x

enumSuite :: TestSuite
enumSuite = suite "enums" do
  genericBoundedEnumSuite "Char" (Proxy :: Proxy Char) $ pure unit

genericEnumSuite :: forall a. Enum a => Arbitrary a => Show a =>
  String -> Proxy a -> TestSuite -> TestSuite
genericEnumSuite name _ extras = suite name do
  extras

genericBoundedEnumSuite :: forall a. BoundedEnum a => Arbitrary a => Show a =>
  String -> Proxy a -> TestSuite -> TestSuite
genericBoundedEnumSuite name p extras = genericEnumSuite name p $ (_ <> extras) do
  test "everything" do
    Assert.equal (First bottom) $ foldEnum (First :: a -> First a)
    Assert.equal (Last top) $ foldEnum (Last :: a -> Last a)
