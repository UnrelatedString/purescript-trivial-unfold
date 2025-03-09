module Test.Main where

import Prelude

import Effect (Effect)
import Test.Spec(Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy, AnyShow(..))
import Test.QuickCheck ((===))
import Test.QuickCheck.Arbitrary (class Arbitrary)
import Test.Spec.QuickCheck (quickCheck, quickCheck')
import Test.Spec.Runner.Node (runSpecAndExitProcess)
import Test.Spec.Reporter.June.Pretty (prettyReporter)
import Effect.Aff (Aff)
import Effect.Exception as Effect.Exception
import Pipes ((>->), yield, await)
import Pipes.Core (Producer_, Consumer_)
import Pipes.Core as Pipes -- I am NOT importing something called "runEffect" unqualified lol
import Control.Monad.Trans.Class (lift)
import Control.Monad.Error.Class (class MonadThrow)

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
 , tail
 , last1
 , last
 , init
 , index
 , foldEnum
 , iterate
 , uncons
 , cons
 , snoc
 , refoldMap
 , refoldMap1
 , take
 , take1
 , index1
 , drop
)

import Data.Unfoldable.MaybeEmpty
 ( MaybeEmpty(..)
 , distributeMaybesA
 , toAlternative
 )

import Data.Maybe (Maybe(..), isJust, isNothing)
import Control.Alternative ((<|>), guard)
import Data.Enum (class Enum, class BoundedEnum, succ, pred, upFrom, downFrom, upFromIncluding, enumFromTo)
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
import Data.Newtype (un, ala)
import Control.Extend (duplicate)
import Data.Identity (Identity)
import Data.Distributive (distribute)
import Data.List.NonEmpty as NEL

iff :: forall a. Boolean -> a -> Maybe a
iff = ($>) <<< guard

oughta :: forall f m t. Functor f => Show (f (AnyShow t)) => MonadThrow Effect.Exception.Error m => f t -> (f (AnyShow t) -> Boolean) -> m Unit
oughta = map AnyShow >>> shouldSatisfy

main :: Effect Unit
main = runSpecAndExitProcess [prettyReporter] do
  smallSuite
  buildSuite
  foldSuite
  enumSuite
  newtypesSuite
  exampleInTheReadmeTest

smallSuite :: Spec Unit
smallSuite = describe "small stuff" do
  it "single uncons" do
    map (map trivial) (uncons none) `oughta` isNothing
    map (map trivial) (uncons $ singleton unit) `oughta` isJust
  it "head is sane" do
    quickCheck \(x :: Int) -> head (singleton x) === Just x
    quickCheck \(x :: Int) -> head1 (singleton x) === x
    quickCheck \(x :: Maybe String) -> head (fromMaybe x) === x
    quickCheck \x -> head (replicate x "ehehe") === iff (x > 0) "ehehe"
    quickCheck \x (y :: Int) -> head (replicate1 x y) === Just y
  it "last is sane" do
    quickCheck \(x :: Int) -> last (singleton x) === Just x
    quickCheck \(x :: Int) -> last1 (singleton x) === x
    quickCheck \(x :: Maybe String) -> last (fromMaybe x) === x
    quickCheck' 5 \x -> last (replicate x "ehehe") === iff (x > 0) "ehehe"
    quickCheck' 5 \x (y :: Int) -> last (replicate1 x y) === Just y
  it "double uncons" do
    let double :: forall a. Trivial a -> Maybe (a /\ Maybe a)
        double = map (map head) <<< uncons
    quickCheck \(x :: Int) -> double (singleton x) === Just (x /\ Nothing)
    quickCheck \x -> isJust (snd =<< double (replicate1 x unit)) === (x > 1)
  it "head tail gets second element" do
    head (tail none) `oughta` isNothing
    quickCheck \(x :: String) -> head (tail $ singleton x) === Nothing
    quickCheck \(x :: Char) -> head (tail $ upFrom x) === (succ =<< succ x)
    quickCheck \(x :: Trivial Int) -> head (tail x) === index x 1
  it "last tail gets last" do
    quickCheck \(x :: String) -> last (tail $ singleton x) === Nothing
    quickCheck \(x :: Trivial Int) -> last (tail x) === tail x *> last x
  it "last init gets second to last" do
    quickCheck \(x :: String) -> last (init $ singleton x) === Nothing
    quickCheck' 50 \(x :: Char) y -> last (init $ enumFromTo x y) === case compare x y of
      LT -> pred y
      GT -> succ y
      EQ -> Nothing
  it "Maybe round trip" do
    quickCheck \(x :: Maybe Char) -> runTrivial (fromMaybe x) === x
  it "take <> drop" do
    quickCheck \(x :: Trivial Int) n -> refoldMap singleton (take n x) <> refoldMap singleton (drop n x) === (runTrivial x :: Array _)
  it "take agrees with index" do
    quickCheck \(x :: Trivial Char) n -> refoldMap (Just <<< Last) (take n x) === Last <$> index x ((min n $ length x) - 1)
  it "drop agrees with index" do
    quickCheck \(x :: Trivial Char) n -> drop n x === index x (max n 0)
  it "take1 agrees with index1" do
    quickCheck \(x :: Trivial1 Char) n -> refoldMap1 Last (take1 n x) === (Last $ index1 x (clamp 0 (length x - 1) (n - 1)))

buildSuite :: Spec Unit
buildSuite = describe "build" do
  it "build on none" do
    quickCheck \(x :: Int) -> cons x none === Just x
    quickCheck \(x :: Int) -> snoc none x === Just x
  it "build on singleton" do
    quickCheck \x (y :: Int) -> cons x (singleton y) === Just x
    quickCheck \x (y :: Int) -> tail (cons x $ singleton y) === Just y
    quickCheck \x (y :: Int) -> snoc (singleton y) x === Just y
    quickCheck \x (y :: Int) -> tail (snoc (singleton y) x) === Just x
  it "build on Unfoldable1" do
    quickCheck \x (y :: Trivial1 Int) -> cons x (runTrivial1 y) === Just x
    quickCheck \x (y :: Trivial1 Int) -> head1 (snoc (runTrivial1 y) x) === head1 y
    quickCheck \x (y :: Trivial1 Int) -> tail (cons x $ runTrivial1 y) === Just (head1 y)
    quickCheck \x (y :: Trivial1 Int) -> tail (snoc (runTrivial1 y) x) === index (runTrivial1 y) 1 <|> Just x

foldSuite :: Spec Unit
foldSuite = describe "foldl foldr" do
  describe "Foldable Trivial1" do
    it "associative string concatenation agrees" do
      quickCheck \(u :: Trivial1 String) ->
        foldMapDefaultL identity u === foldMapDefaultR identity u
  describe "Foldable Trivial" do
    it "associative string concatenation agrees" do
      quickCheck \(u :: Trivial String) ->
        foldMapDefaultL identity u === foldMapDefaultR identity u
    it "empty folds" do 
      quickCheck \(f :: Int -> String -> Int) x -> (foldl f x ::<*> none) === x
      quickCheck \(f :: String -> Int -> Int) x -> (foldr f x ::<*> none) === x
  describe "Foldable1 Trivial1" do
    it "associative string concatenation agrees" do
      quickCheck \(u :: Trivial1 String) ->
        foldMap1DefaultL identity u === foldMap1DefaultR identity u
    it "singleton folds" do 
      quickCheck \f (x :: Int) -> (foldl1 f ::<+> singleton x) === x
      quickCheck \f (x :: Int) -> (foldr1 f ::<+> singleton x) === x

enumSuite :: Spec Unit
enumSuite = describe "enums" do
  genericEnumSuite "Int" (Proxy :: Proxy Int) do
    it "index matches upFromIncluding" do
      quickCheck' 20 \x y -> index (upFromIncluding x) y === iff (y >= 0) (x + y)
    it "index matches iterate" do
      quickCheck' 5 \x -> index (iterate (_+1) 0) x === iff (x >= 0) x
  genericBoundedEnumSuite "Char" (Proxy :: Proxy Char) $ pure unit
  genericBoundedEnumSuite "Ordering" (Proxy :: Proxy Ordering) $ pure unit
  genericBoundedEnumSuite "Boolean" (Proxy :: Proxy Boolean) $ pure unit
  genericBoundedEnumSuite "Unit" (Proxy :: Proxy Unit) $ pure unit -- ...what was I thinking of doing for bounded extras again

genericEnumSuite :: forall a. Enum a => Arbitrary a => Show a =>
  String -> Proxy a -> Spec Unit
 -> Spec Unit

genericEnumSuite name _ extras = describe name do
  it "directionality" do
    quickCheck' 100 \(x :: a) -> head (upFrom x) === succ x
    quickCheck' 100 \(x :: a) -> head (downFrom x) === pred x
  extras

genericBoundedEnumSuite :: forall a. BoundedEnum a => Arbitrary a => Show a =>
  String -> Proxy a -> Spec Unit
 -> Spec Unit

genericBoundedEnumSuite name p extras = genericEnumSuite name p $ (_ *> extras) do
  it "ends are in right order" do
    First bottom `shouldEqual` foldEnum (First :: a -> First a)
    Last top `shouldEqual` foldEnum (Last :: a -> Last a)

newtypesSuite :: Spec Unit
newtypesSuite = describe "Newtypes" do
  it "empty MaybeEmpty is Nothing" do
    (Nothing :: Maybe (NEL.NonEmptyList Unit)) `shouldEqual` un MaybeEmpty none
  it "MaybeEmpty Maybe Int agrees with Extend Maybe" do
    quickCheck \(x :: Trivial Int) -> un MaybeEmpty (runTrivial x) === duplicate (runTrivial x)
  it "NonEmptyList always roundtrips intact" do
    quickCheck \(x :: NEL.NonEmptyList String) -> un MaybeEmpty (NEL.toUnfoldable x) === Just x
  it ("distributeMaybes would agree with Distributive Identity..." <>
        "if Identity had an Unfoldable1 instance, which it just doesn't for some reason") do
    quickCheck \(x :: Maybe (Identity Char)) -> distributeMaybesA (MaybeEmpty x) === distribute x
  it "toAlternative agrees with Monad Array" do
    quickCheck \(x :: Array Number) -> join (toAlternative $ toUnfoldable x) === x

-- because it would be ESPECIALLY embarrassing if this didn't work :P
exampleInTheReadmeTest :: Spec Unit
exampleInTheReadmeTest = it "Example in the README" $
  Pipes.runEffect $ exampleInTheReadme >-> do
    equals $ Just 'z'
    equals   "gonna give you up"
    equals   720
  where equals :: forall a. Show a => a -> Consumer_ String Aff Unit
        equals value = await >>= lift <<< shouldEqual (show value)

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
  import Data.Newtype (ala)
  import Data.Multiplicative (Multiplicative(..))

  import Data.Unfoldable.Trivial (index, drop, refoldMap1, (::<*>))
  -}

  -- main = do

  -- Index into a very large range without evaluating all of it.
  logShow $ index (upFromIncluding 'A') $ 32 + 25
  -- > Just 'z'

  -- Fold over a suffix of an Array without constructing a new Array for
  -- the suffix.
  -- The (::<*>) operator is ($) specialized to Trivial,
  -- and (::<+>) likewise for Trivial1,
  -- to conveniently make instances decidable.
  -- (Note that this can also be accomplished with Data.List.Lazy.)
  logShow $ intercalate " " ::<*> drop 2 $ toUnfoldable [
    "I'm", "never", "gonna", "give", "you", "up"
  ]
  -- > "gonna give you up"

  -- Fold directly from a generating function.
  -- Basic folds are also provided specialized, with the "re-" prefix;
  -- i.e. `refoldMap1 Multiplicative $ unfoldr1 fact 1` is equivalent to
  --      `foldMap1 Multiplicative ::<+> unfoldr1 fact 1`.
  let fact n = n /\ (guard (n < 6) $> n + 1)
  logShow $ ala Multiplicative refoldMap1 $ unfoldr1 fact 1
  -- > 620
