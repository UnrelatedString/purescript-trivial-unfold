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
 ( Trivial
 , trivial
 , uncons
 , runTrivial
)

import Data.Unfoldable.Trivial.Adapter
 ( head
 , tail
 , index
 , foldEnum
)

import Data.Maybe (Maybe(..), isJust, isNothing)
import Data.Enum (class Enum, class BoundedEnum, succ, upFrom, upFromIncluding)
import Data.Tuple (snd)
import Data.Tuple.Nested ((/\), type (/\))
import Data.Monoid (guard)
import Data.Semigroup.First (First(..))
import Data.Semigroup.Last (Last(..))
import Data.Unfoldable (none, fromMaybe, replicate)
import Data.Unfoldable1 (singleton, replicate1)
import Type.Proxy (Proxy(..))

main :: Effect Unit
main = runTest do
  smallSuite
  foldSuite
  enumSuite

smallSuite :: TestSuite
smallSuite = suite "small stuff" do
  test "single uncons" do
    Assert.assert "none should be empty" $ isNothing $ map (map trivial) $ uncons none
    Assert.assert "singleton should be nonempty" $ isJust $ map (map trivial) $ uncons $ singleton unit
  test "head" do
    quickCheck \(x :: Int) -> head (singleton x) === Just x
    quickCheck \(x :: Maybe String) -> head (fromMaybe x) === x
    quickCheck \x -> head (replicate x "ehehe") === guard (x > 0) (Just "ehehe")
    quickCheck \x (y :: Int) -> head (replicate1 x y) === Just y
  test "double uncons" do
    let double :: forall a. Trivial a -> Maybe (a /\ Maybe a)
        double = map (map head) <<< uncons
    quickCheck \(x :: Int) -> double (singleton x) === Just (x /\ Nothing)
    quickCheck \x -> isJust (snd =<< double (replicate1 x unit)) === (x > 1)
  test "head tail gets second element" do
    Assert.assert "tail of none is still none" $ isNothing $ head $ tail none
    quickCheck \(x :: String) -> head (tail $ singleton x) === Nothing
    quickCheck \(x :: Char) -> head (tail $ upFrom x) === (succ =<< succ x)
  test "Maybe round trip" do
    quickCheck \(x :: Maybe Char) -> runTrivial (fromMaybe x) === x

foldSuite :: TestSuite
foldSuite = suite "fold" do
  test "identities" do
    pure unit -- TODO

enumSuite :: TestSuite
enumSuite = suite "enums" do
  test "index matches upFromIncluding" do
    quickCheck \x y -> index (upFromIncluding x) y === if y >= 0 then Just (x + y) else Nothing
  genericBoundedEnumSuite "Char" (Proxy :: Proxy Char) $ pure unit

genericEnumSuite :: forall a. Enum a => Arbitrary a => Show a =>
  String -> Proxy a -> TestSuite -> TestSuite
genericEnumSuite name _ extras = suite name do
  extras

genericBoundedEnumSuite :: forall a. BoundedEnum a => Arbitrary a => Show a =>
  String -> Proxy a -> TestSuite -> TestSuite
genericBoundedEnumSuite name p extras = genericEnumSuite name p $ (_ <> extras) do
  test "ends are in right order" do
    Assert.equal (First bottom) $ foldEnum (First :: a -> First a)
    Assert.equal (Last top) $ foldEnum (Last :: a -> Last a)
