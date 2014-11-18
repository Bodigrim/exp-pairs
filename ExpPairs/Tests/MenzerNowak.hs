module ExpPairs.Tests.MenzerNowak where

import Data.Ratio
import Data.List
import ExpPairs.MenzerNowak
import ExpPairs.Kratzel

import Test.SmallCheck
import Test.SmallCheck.Series
import Test.QuickCheck

snd4 (_, a, _, _) = a

testMonotonic a' b' c' d' =  (a==c && b==d) || zab > zcd where
	[a'', b'', c'', d''] = map (\n -> abs n + 1) [a', b', c', d']
	[a, c] = sort [a'', c'']
	[b, d] = sort [b'', d'']
	zab = snd4 $ menzerNowak a b
	zcd = snd4 $ menzerNowak c d

testCompareLow a' b' = snd4 (snd $ tauab a b) <= snd4 (menzerNowak a b) + 1%(10^30) where
	[a, b] = map (\n -> abs n + 1) [a', b']

testCompareHigh a' b' = snd4 (menzerNowak a b) < 1 where
	[a, b] = map (\n -> abs n + 1) [a', b']

testSmth depth (name, test) = do
	putStrLn name
	mapM_ (\_ -> quickCheck test) [1..1]
	smallCheck depth test

testSuite = do
	mapM_ (testSmth 5) [
		("menzerNowak compare with tauab", testCompareLow),
		("menzerNowak compare with 1", testCompareHigh)
		]
	mapM_ (testSmth 1) [
		("menzerNowak monotonic", testMonotonic)
		]
