module LinearForm where

import Math.ExpPairs.LinearForm

import Test.Tasty
import Test.Tasty.SmallCheck as SC
import Test.Tasty.QuickCheck as QC

extractCoeffs :: Num t => LinearForm t -> (t, t, t)
extractCoeffs lf =
	( evalLF (1, 0, 0) lf
	, evalLF (0, 1, 0) lf
	, evalLF (0, 0, 1) lf
	)

testPlus :: Rational -> Rational -> Rational -> Rational -> Rational -> Rational -> Bool
testPlus a b c d e f = a+d==ad && b+e==be && c+f==cf where
	l1 = LinearForm a b c
	l2 = LinearForm d e f
	(ad, be, cf) = extractCoeffs (l1 + l2)

testMinus :: Rational -> Rational -> Rational -> Rational -> Rational -> Rational -> Bool
testMinus a b c d e f = a-d==ad && b-e==be && c-f==cf where
	l1 = LinearForm a b c
	l2 = LinearForm d e f
	(ad, be, cf) = extractCoeffs (l1 - l2)

testFromInteger :: Integer -> Bool
testFromInteger a = evalLF (0, 0, 1) (fromInteger a) == a

testSuite :: TestTree
testSuite = testGroup "LinearForm"
	[ QC.testProperty "plus" testPlus
	, QC.testProperty "minus" testMinus
	, SC.testProperty "from integer" testFromInteger
	, QC.testProperty "from integer" testFromInteger
	]
