module MenzerNowak where

import Data.Ratio
import Data.List
import Math.ExpPairs
import Math.ExpPairs.MenzerNowak
import Math.ExpPairs.Kratzel

import Test.Tasty
import Test.Tasty.SmallCheck as SC
import Test.Tasty.QuickCheck as QC hiding (Positive)

import Instances (Positive (..))

testMonotonic :: Positive Integer -> Positive Integer -> Positive Integer -> Positive Integer -> Bool
testMonotonic (Positive a') (Positive b') (Positive c') (Positive d') =  (a==c && b==d) || zab > zcd where
	[a, c, b, d] = sort [a', b', c', d']
	zab = optimalValue $ menzerNowak a b
	zcd = optimalValue $ menzerNowak c d

testCompareLow :: Positive Integer -> Positive Integer -> Bool
testCompareLow (Positive a') (Positive b') = optimalValue (snd $ tauab a b) <= optimalValue (menzerNowak a b) + Finite eps  where
	[a, b] = sort [a', b']
	eps = 1 % (10 ^ (30::Integer))

testCompareHigh :: Positive Integer -> Positive Integer -> Bool
testCompareHigh (Positive a') (Positive b') = optimalValue (menzerNowak a b) < 1 where
	[a, b] = sort [a', b']

testSuite :: TestTree
testSuite = testGroup "MenzerNowak"
	[ SC.testProperty "compare with tauab" testCompareLow
	, QC.testProperty "compare with tauab" testCompareLow
	, SC.testProperty "compare with 1" testCompareHigh
	, QC.testProperty "compare with 1" testCompareHigh
	, SC.testProperty "monotonic" testMonotonic
	, QC.testProperty "monotonic" testMonotonic
	]
