module LinearForm where

import Math.ExpPairs.LinearForm

import Test.Tasty
import Test.Tasty.SmallCheck as SC
import Test.Tasty.QuickCheck as QC

testPlus :: Rational -> Rational -> Rational -> Rational -> Rational -> Rational -> Bool
testPlus a b c d e f = a+d==ad && b+e==be && c+f==cf where
	l1 = LinearForm a b c
	l2 = LinearForm d e f
	l3 = l1 + l2
	ad = evalLF (1, 0, 0) l3
	be = evalLF (0, 1, 0) l3
	cf = evalLF (0, 0, 1) l3

testMinus :: Rational -> Rational -> Rational -> Rational -> Rational -> Rational -> Bool
testMinus a b c d e f = a-d==ad && b-e==be && c-f==cf where
	l1 = LinearForm a b c
	l2 = LinearForm d e f
	l3 = l1 - l2
	ad = evalLF (1, 0, 0) l3
	be = evalLF (0, 1, 0) l3
	cf = evalLF (0, 0, 1) l3

testFromInteger :: Integer -> Bool
testFromInteger a = evalLF (0, 0, 1) (fromInteger a) == a

testSuite :: TestTree
testSuite = testGroup "LinearForm"
	[ QC.testProperty "plus" testPlus
	, QC.testProperty "minus" testMinus
	, SC.testProperty "from integer" testFromInteger
	, QC.testProperty "from integer" testFromInteger
	]
