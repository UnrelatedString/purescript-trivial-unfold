module Test.Main where

import Prelude

import Effect (Effect)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Main (runTest)
import Test.Unit.Assert as Assert
import Test.QuickCheck ((===))
import Test.QuickCheck.Arbitrary (class Arbitrary)
import Test.Unit.QuickCheck (quickCheck, quickCheck')

import Data.Unfoldable.Trivial
 ( Trivial
 , trivial
 , uncons
 , runTrivial
 , (::<*>)
)

import Data.Unfoldable1.Trivial1 (Trivial1, runTrivial1, (::<+>))

import Data.Unfoldable.Trivial.Adapter
 ( head1
 , head
 , tail1
 , tail
 , index
 , foldEnum
 , iterate
 , cons
 , snoc
)

import Data.Maybe (Maybe(..), isJust, isNothing)
import Data.Enum (class Enum, class BoundedEnum, succ, pred, upFrom, downFrom, upFromIncluding)
import Data.Tuple (snd)
import Data.Tuple.Nested ((/\), type (/\))
import Data.Monoid (guard)
import Data.Semigroup.First (First(..))
import Data.Semigroup.Last (Last(..))
import Data.Unfoldable (none, fromMaybe, replicate)
import Data.Unfoldable1 (singleton, replicate1)
import Data.Foldable (foldl, foldr, foldMapDefaultL, foldMapDefaultR)
import Data.Semigroup.Foldable (foldl1, foldr1, foldMap1DefaultL, foldMap1DefaultR)
import Type.Proxy (Proxy(..))

main :: Effect Unit
main = runTest do
  smallSuite
  buildSuite
  foldSuite
  enumSuite

smallSuite :: TestSuite
smallSuite = suite "small stuff" do
  test "single uncons" do
    Assert.assert "none should be empty" $ isNothing $ map (map trivial) $ uncons none
    Assert.assert "singleton should be nonempty" $ isJust $ map (map trivial) $ uncons $ singleton unit
  test "head" do
    quickCheck \(x :: Int) -> head (singleton x) === Just x
    quickCheck \(x :: Int) -> head1 (singleton x) === x
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

buildSuite :: TestSuite
buildSuite = suite "build" do
  test "build on none" do
    quickCheck \(x :: Int) -> cons x none === Just x
    quickCheck \(x :: Int) -> snoc none x === Just x
  test "build on singleton" do
    quickCheck \x (y :: Int) -> cons x (singleton y) === Just x
    quickCheck \x (y :: Int) -> tail1 (cons x $ singleton y) === Just y
    quickCheck \x (y :: Int) -> snoc (singleton y) x === Just y
    quickCheck \x (y :: Int) -> tail1 (snoc (singleton y) x) === Just x
  test "build on Unfoldable1" do
    quickCheck \x (y :: Trivial1 Int) -> cons x (runTrivial1 y) === Just x
    quickCheck \x (y :: Trivial1 Int) -> head1 (snoc (runTrivial1 y) x) === head1 y
    quickCheck \x (y :: Trivial1 Int) -> tail (cons x $ runTrivial1 y) === Just (head1 y)
    quickCheck \x (y :: Trivial1 Int) -> tail (snoc (runTrivial1 y) x) === index (runTrivial1 y) 1

foldSuite :: TestSuite
foldSuite = suite "foldl foldr" do
  suite "Foldable Trivial1" do
    test "associative string concatenation agrees" do
      quickCheck \(u :: Trivial1 String) ->
        foldMapDefaultL identity u === foldMapDefaultR identity u
  suite "Foldable Trivial" do
    test "associative string concatenation agrees" do
      quickCheck \(u :: Trivial String) ->
        foldMapDefaultL identity u === foldMapDefaultR identity u
    test "empty folds" do 
      quickCheck \(f :: Int -> String -> Int) x -> (foldl f x ::<*> none) === x
      quickCheck \(f :: String -> Int -> Int) x -> (foldr f x ::<*> none) === x
  suite "Foldable1 Trivial1" do
    test "associative string concatenation agrees" do
      quickCheck \(u :: Trivial1 String) ->
        foldMap1DefaultL identity u === foldMap1DefaultR identity u
    test "singleton folds" do 
      quickCheck \f (x :: Int) -> (foldl1 f ::<+> singleton x) === x
      quickCheck \f (x :: Int) -> (foldr1 f ::<+> singleton x) === x

enumSuite :: TestSuite
enumSuite = suite "enums" do
  genericEnumSuite "Int" (Proxy :: Proxy Int) do
    test "index matches upFromIncluding" do
      quickCheck' 200 \x y -> index (upFromIncluding x) y === if y >= 0 then Just (x + y) else Nothing
    test "index matches iterate" do
      quickCheck' 200 \x -> index (iterate (_+1) 0) x === if x >= 0 then Just x else Nothing
  genericBoundedEnumSuite "Char" (Proxy :: Proxy Char) $ pure unit
  genericBoundedEnumSuite "Ordering" (Proxy :: Proxy Ordering) $ pure unit
  genericBoundedEnumSuite "Boolean" (Proxy :: Proxy Boolean) $ pure unit
  genericBoundedEnumSuite "Unit" (Proxy :: Proxy Unit) $ pure unit -- ...what was I thinking of doing for bounded extras again

genericEnumSuite :: forall a. Enum a => Arbitrary a => Show a =>
  String -> Proxy a -> TestSuite -> TestSuite
genericEnumSuite name _ extras = suite name do
  test "directionality" do
    quickCheck' 100 \(x :: a) -> head (upFrom x) === succ x
    quickCheck' 100 \(x :: a) -> head (downFrom x) === pred x
  extras

genericBoundedEnumSuite :: forall a. BoundedEnum a => Arbitrary a => Show a =>
  String -> Proxy a -> TestSuite -> TestSuite
genericBoundedEnumSuite name p extras = genericEnumSuite name p $ (_ <> extras) do
  test "ends are in right order" do
    Assert.equal (First bottom) $ foldEnum (First :: a -> First a)
    Assert.equal (Last top) $ foldEnum (Last :: a -> Last a)
