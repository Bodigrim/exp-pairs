module Kratzel where

import Data.Ratio
import Data.List
import Math.ExpPairs
import Math.ExpPairs.Kratzel

import Test.Tasty
import Test.Tasty.SmallCheck as SC
import Test.Tasty.QuickCheck as QC hiding (Positive)
import Test.Tasty.HUnit

import Instances (Positive (..))
import Etalon (testEtalon)

testAbMonotonic :: Positive Integer -> Positive Integer -> Positive Integer -> Positive Integer -> Bool
testAbMonotonic (Positive a') (Positive b') (Positive c') (Positive d') =  (a==c && b==d) || zab > zcd where
	[a, c, b, d] = sort [a', b', c', d']
	zab = optimalValue $ snd $ tauab a b
	zcd = optimalValue $ snd $ tauab c d

testAbCompareLow :: Positive Integer -> Positive Integer -> Bool
testAbCompareLow (Positive a') (Positive b') = optimalValue (snd $ tauab a b) >= Finite (1%(2*a+2*b)) where
	[a, b] = sort [a', b']

testAbCompareHigh :: Positive Integer -> Positive Integer -> Bool
testAbCompareHigh (Positive a') (Positive b') = optimalValue (snd $ tauab a b) < Finite (1%(a+b)) where
	[a, b] = sort [a', b']


testAbcMonotonic :: Positive Integer -> Positive Integer -> Positive Integer -> Positive Integer -> Positive Integer -> Positive Integer -> Bool
testAbcMonotonic (Positive a') (Positive b') (Positive c') (Positive d') (Positive e') (Positive f') = (a==d && b==e && c==f) || theoremAbc==Kr64 || zabc >= zdef where
	[a, d, b, e, c, f] = sort [a', b', c', d', e', f']
	(theoremAbc, resultAbc) = tauabc a b c
	zabc = optimalValue resultAbc
	zdef = optimalValue $ snd $ tauabc d e f

testAbcCompareLow :: Positive Integer -> Positive Integer -> Positive Integer -> Bool
testAbcCompareLow (Positive a') (Positive b') (Positive c') = c>=a+b || optimalValue (snd $ tauabc a b c) >= Finite (1%(a+b+c)) where
	[a, b, c] = sort [a', b', c']

testAbcCompareHigh :: Positive Integer -> Positive Integer -> Positive Integer -> Bool
testAbcCompareHigh (Positive a') (Positive b') (Positive c') = c>=a+b || optimalValue (snd $ tauabc a b c) < Finite (2%(a+b+c)) where
	[a, b, c] = sort [a', b', c']

etalonTauab :: Integer -> Integer -> Integer -> Integer -> Bool
etalonTauab a b c d = Finite (c%d) >= (optimalValue . snd) (tauab a b)

etalonTauabc :: Integer -> Integer -> Integer -> Integer -> Integer -> Bool
etalonTauabc a b c d e = Finite (d%e) >= (optimalValue . snd) (tauabc a b c)

testSuite :: TestTree
testSuite = testGroup "Kratzel"
	[ testCase "etalon tauab"
		(testEtalon 100 (\[x1, x2, x3, x4] -> etalonTauab x1 x2 x3 x4) "tests/etalon-tauab.txt")
	, testCase "etalon tauabc"
		(testEtalon 100 (\[x1, x2, x3, x4, x5] -> etalonTauabc x1 x2 x3 x4 x5) "tests/etalon-tauabc.txt")
	, SC.testProperty "tauabc compare with 1/(a+b+c)" testAbcCompareLow
	, QC.testProperty "tauabc compare with 1/(a+b+c)" testAbcCompareLow
	, SC.testProperty "tauabc compare with 2/(a+b+c)" testAbcCompareHigh
	, QC.testProperty "tauabc compare with 2/(a+b+c)" testAbcCompareHigh
	, QC.testProperty "tauabc monotonic" testAbcMonotonic
	, SC.testProperty "tauab compare with 1/2(a+b)" testAbCompareLow
	, QC.testProperty "tauab compare with 1/2(a+b)" testAbCompareLow
	, SC.testProperty "tauab compare with 1/(a+b)" testAbCompareHigh
	, QC.testProperty "tauab compare with 1/(a+b)" testAbCompareHigh
	, SC.testProperty "tauab monotonic" testAbMonotonic
	, QC.testProperty "tauab monotonic" testAbMonotonic
	]
