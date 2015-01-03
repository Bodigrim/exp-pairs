module ExpPairs.Tests.Kratzel where

import Data.Ratio
import Data.List
import ExpPairs.Optimize
import ExpPairs.Kratzel

import Test.SmallCheck
import Test.SmallCheck.Series
import Test.QuickCheck hiding (Positive)

instance (Num a, Ord a, Arbitrary a) => Arbitrary (Positive a) where
  arbitrary =
    (Positive . abs) `fmap` (arbitrary `suchThat` (> 0))


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
testAbcMonotonic (Positive a') (Positive b') (Positive c') (Positive d') (Positive e') (Positive f') =  (a==d && b==e && c==f) || zabc >= zdef where
	[a, d, b, e, c, f] = sort [a', b', c', d', e', f']
	zabc = optimalValue $ snd $ tauabc a b c
	zdef = optimalValue $ snd $ tauabc d e f

testAbcCompareLow :: Positive Integer -> Positive Integer -> Positive Integer -> Bool
testAbcCompareLow (Positive a') (Positive b') (Positive c') = c>=a+b || optimalValue (snd $ tauabc a b c) >= Finite (1%(a+b+c)) where
	[a, b, c] = sort [a', b', c']

testAbcCompareHigh :: Positive Integer -> Positive Integer -> Positive Integer -> Bool
testAbcCompareHigh (Positive a') (Positive b') (Positive c') = c>=a+b || optimalValue (snd $ tauabc a b c) < Finite (2%(a+b+c)) where
	[a, b, c] = sort [a', b', c']

etalonTauab [a,b,c,d] = Finite (c%d) >= (optimalValue . snd) (tauab a b)

etalonTauabc [a,b,c,d,e] = Finite (d%e) >= (optimalValue . snd) (tauabc a b c)

testEtalon f filename = do
	etalon <- readFile filename
	let tests = map (map read . words) (lines etalon) in
		let results = map f tests in
			putStrLn $ filename ++ (if and results then " success" else " fail at " ++ show (fst $ head $ dropWhile snd $ zip tests results))
	return etalon

testSmth depth (name, test) = do
	putStrLn name
	mapM_ (\_ -> quickCheck test) [1..1]
	smallCheck depth test

testSuite = do
	testEtalon etalonTauab  "ExpPairs/Tests/etalon-tauab.txt"
	testEtalon etalonTauabc "ExpPairs/Tests/etalon-tauabc.txt"
	mapM_ (testSmth 7) [
		("tauabc compare with 1/(a+b+c)", testAbcCompareLow),
		("tauabc compare with 2/(a+b+c)", testAbcCompareHigh)
		]
	mapM_ (testSmth 2) [
		("tauabc monotonic", testAbcMonotonic)
		]
	mapM_ (testSmth 10) [
		("tauab compare with 1/2(a+b)", testAbCompareLow),
		("tauab compare with 1/(a+b)", testAbCompareHigh)
		]
	mapM_ (testSmth 3) [
		("tauab monotonic", testAbMonotonic)
		]
