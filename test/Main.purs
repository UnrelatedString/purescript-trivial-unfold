module Test.Main where

import Prelude

import Effect (Effect)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Main (runTest)
import Test.Unit.Assert as Assert
import Test.QuickCheck ((===))
import Test.QuickCheck.Arbitrary (class Arbitrary)
import Test.Unit.QuickCheck (quickCheck, quickCheck')
import Effect.Aff (Aff)
import Pipes ((>->), yield, await)
import Pipes.Core (Producer_, Consumer_)
import Pipes.Core as Pipes -- I am NOT importing something called "runEffect" unqualified lol
import Control.Monad.Trans.Class (lift)

import Data.Unfoldable.Trivial.Internal
 ( Trivial
 , trivial
 , runTrivial
 , (::<*>)
)

import Data.Unfoldable1.Trivial1.Internal
 ( Trivial1
 , runTrivial1
 , (::<+>)
)

import Data.Unfoldable.Trivial
 ( head1
 , head
 , tail1
 , tail
 , index
 , foldEnum
 , iterate
 , uncons
 , cons
 , snoc
 , refold1
 , refoldMap
 , refoldMap1
 , take
 , take1
 , index1
 , drop
)

import Data.Maybe (Maybe(..), isJust, isNothing)
import Control.Alternative ((<|>), guard)
import Data.Enum (class Enum, class BoundedEnum, succ, pred, upFrom, downFrom, upFromIncluding)
import Data.Tuple (snd)
import Data.Tuple.Nested ((/\), type (/\))
import Data.Semigroup.First (First(..))
import Data.Semigroup.Last (Last(..))
import Data.Unfoldable (none, fromMaybe, replicate)
import Data.Unfoldable1 (singleton, replicate1, unfoldr1)
import Data.Foldable (foldl, foldr, foldMapDefaultL, foldMapDefaultR, intercalate, length)
import Data.Semigroup.Foldable (foldl1, foldr1, foldMap1DefaultL, foldMap1DefaultR)
import Type.Proxy (Proxy(..))
import Data.Array (toUnfoldable)
import Data.Monoid.Multiplicative (Multiplicative(..))

iff :: forall a. Boolean -> a -> Maybe a
iff = ($>) <<< guard

main :: Effect Unit
main = runTest do
  smallSuite
  buildSuite
  foldSuite
  enumSuite
  exampleInTheReadmeTest

smallSuite :: TestSuite
smallSuite = suite "small stuff" do
  test "single uncons" do
    Assert.assert "none should be empty" $ isNothing $ map (map trivial) $ uncons none
    Assert.assert "singleton should be nonempty" $ isJust $ map (map trivial) $ uncons $ singleton unit
  test "head" do
    quickCheck \(x :: Int) -> head (singleton x) === Just x
    quickCheck \(x :: Int) -> head1 (singleton x) === x
    quickCheck \(x :: Maybe String) -> head (fromMaybe x) === x
    quickCheck \x -> head (replicate x "ehehe") === iff (x > 0) "ehehe"
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
  test "take <> drop" do
    quickCheck \(x :: Trivial Int) n -> refoldMap singleton (take n x) <> refoldMap singleton (drop n x) === (runTrivial x :: Array _)
  test "take agrees with index" do
    quickCheck \(x :: Trivial Char) n -> refoldMap (Just <<< Last) (take n x) === Last <$> index x ((min n $ length x) - 1)
  test "drop agrees with index" do
    quickCheck \(x :: Trivial Char) n -> drop n x === index x (max n 0)
  test "take1 agrees with index1" do
    quickCheck \(x :: Trivial1 Char) n -> refoldMap1 Last (take1 n x) === (Last $ index1 x (clamp 0 (length x - 1) (n - 1)))

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
    quickCheck \x (y :: Trivial1 Int) -> tail (snoc (runTrivial1 y) x) === index (runTrivial1 y) 1 <|> Just x

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
      quickCheck' 20 \x y -> index (upFromIncluding x) y === iff (y >= 0) (x + y)
    test "index matches iterate" do
      quickCheck' 20 \x -> index (iterate (_+1) 0) x === iff (x >= 0) x
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

-- because it would be ESPECIALLY embarrassing if this didn't work :P
exampleInTheReadmeTest :: TestSuite
exampleInTheReadmeTest = test "Example in the README" $
  Pipes.runEffect $ exampleInTheReadme >-> do
    equals $ Just 'z'
    equals   "gonna give you up"
    equals $ Multiplicative 720
  where equals :: forall a. Show a => a -> Consumer_ String Aff Unit
        equals value = await >>= lift <<< Assert.equal (show value)

-- it took me embarrassingly long to fully realize Identity doesn't just, like,
-- automatically coerce to literally any other monad :p
exampleInTheReadme :: forall m. Monad m => Producer_ String m Unit
exampleInTheReadme = do
  -- ehehehehe
  let logShow :: forall a. Show a => a -> _
      logShow = yield <<< show
  
  -- Imports to show in actual example
  {-
  import Effect.Console (logShow)
  import Data.Enum (upFrom)
  import Data.Maybe (Maybe(..))
  import Data.Array (toUnfoldable)
  import Data.Foldable (intercalate)
  import Data.Monoid (guard)
  import Data.Unfoldable (unfoldr1)
  import Data.Multiplicative (Multiplicative(..))

  import Data.Unfoldable.Trivial ((::<*>), index, drop, refold1)
  -}

  -- main = do
  
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
