module ExpPairs.Tests.MenzerNowak where

import Data.Ratio
import Data.List
import ExpPairs.MenzerNowak
import ExpPairs.Kratzel

import Test.SmallCheck
import Test.SmallCheck.Series
import Test.QuickCheck hiding (Positive)

instance (Num a, Ord a, Arbitrary a) => Arbitrary (Positive a) where
  arbitrary =
    (Positive . abs) `fmap` (arbitrary `suchThat` (> 0))


snd4 (_, a, _, _) = a

testMonotonic :: (Positive Integer) -> (Positive Integer) -> (Positive Integer) -> (Positive Integer) -> Bool
testMonotonic (Positive a') (Positive b') (Positive c') (Positive d') =  (a==c && b==d) || zab > zcd where
	[a, c, b, d] = sort [a', b', c', d']
	zab = snd4 $ menzerNowak a b
	zcd = snd4 $ menzerNowak c d

testCompareLow :: (Positive Integer) -> (Positive Integer) -> Bool
testCompareLow (Positive a') (Positive b') = snd4 (snd $ tauab a b) <= snd4 (menzerNowak a b) + 1%(10^30)  where
	[a, b] = sort [a', b']

testCompareHigh :: (Positive Integer) -> (Positive Integer) -> Bool
testCompareHigh (Positive a') (Positive b') = snd4 (menzerNowak a b) < 1 where
	[a, b] = sort [a', b']

testSmth depth (name, test) = do
	putStrLn name
	mapM_ (\_ -> quickCheck test) [1..1]
	smallCheck depth test

testSuite = do
	mapM_ (testSmth 10) [
		("menzerNowak compare with tauab", testCompareLow),
		("menzerNowak compare with 1", testCompareHigh)
		]
	mapM_ (testSmth 3) [
		("menzerNowak monotonic", testMonotonic)
		]
