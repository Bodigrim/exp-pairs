module ExpPairs.Tests.Pair where

import Data.Ratio
import ExpPairs.Pair

testBounds :: InitPair -> Bool
testBounds ip = k>=0 && k<=1%2 && l>=1%2 && l<=1 where
	(k, l) = initPairToValue ip

testSuite = do
	putStrLn $ if all testBounds [minBound..maxBound] then "bounds passed" else "bounds failed"
