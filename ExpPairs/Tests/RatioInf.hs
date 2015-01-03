module ExpPairs.Tests.RatioInf where

import ExpPairs.RatioInf

import Test.SmallCheck
import Test.QuickCheck

testPlus :: Rational -> Rational -> Bool
testPlus a b = Finite (a+b) == Finite a + Finite b

testMinus :: Rational -> Rational -> Bool
testMinus a b = Finite (a-b) == Finite a - Finite b

testMultiply :: Rational -> Rational -> Bool
testMultiply a b = Finite (a*b) == Finite a * Finite b

testDivide :: Rational -> Rational -> Bool
testDivide a b = b==0 || (Finite (a/b) == Finite a / Finite b)

testInfPlus :: RationalInf -> Rational -> Bool
testInfPlus a b =	a + Finite b == a

testInfMinus :: RationalInf -> Rational -> Bool
testInfMinus a b = a - Finite b == a

testInfMultiply :: RationalInf -> Rational -> Bool
testInfMultiply a b = b==0 || a * Finite b * Finite b == a

testInfDivide :: RationalInf -> Rational -> Bool
testInfDivide a b =	b==0 || a / Finite b / Finite b == a

testConversion :: Rational -> Bool
testConversion a = toRational (Finite a) == a

testSmth depth (name, test) = do
	putStrLn name
	mapM_ (\_ -> quickCheck test) [1::Integer .. 1]
	smallCheck depth test

testSuite :: IO ()
testSuite = do
	mapM_ (testSmth 5) [
		("plus", testPlus),
		("minus", testMinus),
		("multiply", testMultiply),
		("divide", testDivide)
		]
	mapM_ (testSmth 25) [
		("infplus plus", testInfPlus InfPlus),
		("infplus minus", testInfPlus InfMinus),
		("infminus plus", testInfMinus InfPlus),
		("infminus minus", testInfMinus InfMinus),
		("infmultiply plus", testInfMultiply InfPlus),
		("infmultiply minus", testInfMultiply InfMinus),
		("infdivide plus", testInfDivide InfPlus),
		("infdivide minus", testInfDivide InfMinus),
		("conversion", testConversion)
		]
