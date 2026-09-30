module RatioInf where

import Math.ExpPairs.RatioInf (RatioInf (..), RationalInf)

import Test.Tasty
import Test.Tasty.SmallCheck as SC

testPlus :: Rational -> Rational -> Bool
testPlus a b = Finite (a+b) == Finite a + Finite b

testMinus :: Rational -> Rational -> Bool
testMinus a b = Finite (a-b) == Finite a - Finite b

testMultiply :: Rational -> Rational -> Bool
testMultiply a b = Finite (a*b) == Finite a * Finite b

testDivide :: Rational -> Rational -> Bool
testDivide a b = b==0 || (Finite (a/b) == Finite a / Finite b)

testInfPlus :: RationalInf -> Rational -> Bool
testInfPlus a b =  a + Finite b == a

testInfMinus :: RationalInf -> Rational -> Bool
testInfMinus a b = a - Finite b == a

testInfMultiply :: RationalInf -> Rational -> Bool
testInfMultiply a b = b==0 || a * Finite b * Finite b == a

testInfDivide :: RationalInf -> Rational -> Bool
testInfDivide a b =  b==0 || a / Finite b / Finite b == a

testConversion :: Rational -> Bool
testConversion a = toRational (Finite a) == a

testSuite :: TestTree
testSuite = testGroup "RatioInf"
  [ adjustOption (\(SC.SmallCheckDepth n) -> SC.SmallCheckDepth (n `div` 2)) $
      SC.testProperty "plus"                testPlus
  , adjustOption (\(SC.SmallCheckDepth n) -> SC.SmallCheckDepth (n `div` 2)) $
      SC.testProperty "minus"               testMinus
  , adjustOption (\(SC.SmallCheckDepth n) -> SC.SmallCheckDepth (n `div` 2)) $
      SC.testProperty "multiply"            testMultiply
  , adjustOption (\(SC.SmallCheckDepth n) -> SC.SmallCheckDepth (n `div` 2)) $
      SC.testProperty "divide"              testDivide
  , SC.testProperty "infinity plus"     $ testInfPlus Infinity
  , SC.testProperty "infinity minus"    $ testInfMinus Infinity
  , SC.testProperty "infinity multiply" $ testInfMultiply Infinity
  , SC.testProperty "infinity divide"   $ testInfDivide Infinity
  , SC.testProperty "conversion"          testConversion
  ]

