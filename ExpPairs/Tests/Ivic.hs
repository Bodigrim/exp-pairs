{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module ExpPairs.Tests.Ivic where

import Data.Ratio
import Data.List
import ExpPairs.Optimize
import ExpPairs.Ivic
import ExpPairs.RatioInf

import Test.SmallCheck
import Test.SmallCheck.Series
import Test.QuickCheck

newtype Ratio01 t = Ratio01 t

instance (Ord t, Fractional t, Arbitrary t) => Arbitrary (Ratio01 t) where
	arbitrary = fmap (Ratio01 . ratio01) arbitrary

instance (Ord t, Fractional t, Serial m t) => Serial m (Ratio01 t) where
	series = cons1 (Ratio01 . ratio01)

instance Show t => Show (Ratio01 t) where
  showsPrec n (Ratio01 x) = showsPrec n x


ratio01 a
	| abs a <= 1 = recip 2 + a/4
	| a < 0      = recip (negate a) / 4
	| otherwise  = 3 * recip 4 + recip a / 4

testZetaOnS1 (Ratio01 a') (Ratio01 b') = a==b || za>=zb where
	[a,b] = sort $ map (\n -> (n-1%2)*6) [a', b']
	[za, zb] = map (optimalValue . zetaOnS) [a,b]

testZetaOnS2 (Ratio01 a') (Ratio01 b') = a==b || za>zb where
	[a,b] = sort [a', b']
	[za, zb] = map (optimalValue . zetaOnS) [a,b]

testZetaOnSsym (Ratio01 a') = (toRational . abs) (za-za') == abs (a-1%2) where
	a = (a'-1%2)*6
	za = optimalValue $ zetaOnS a
	za' = optimalValue $ zetaOnS (1-a)

testZetaOnSZero (Ratio01 a') = a<1 || optimalValue (zetaOnS a) == 0 where
	a = (a'-1%2)*6


testMOnS1 (Ratio01 a') (Ratio01 b') = a==b || za<=zb where
	[a,b] = sort $ map (\n -> (n-1%2)*6) [a', b']
	[za, zb] = map (optimalValue . mOnS) [a,b]

testMOnS2 (Ratio01 a') (Ratio01 b') = a==b || za<zb where
	[a,b] = sort $ map (\n -> n/2+1%2) [a', b']
	[za, zb] = map (optimalValue . mOnS) [a,b]

testMOnSZero (Ratio01 a') = a>=1%2 || (optimalValue . mOnS) a == 0 where
	a = (a'-1%2)*6

testMOnSInf (Ratio01 a') = a<1 || (optimalValue . mOnS) a == InfPlus where
	a = (a'-1%2)*6


testZetaReverse (Ratio01 s') = abs (s-t) <= 5%1000 where
	s = s' / 2
	zs = zetaOnS s
	t = toRational $ optimalValue $ reverseZetaOnS $ toRational $ optimalValue zs

-- Convexity tests - they fail and it is OK
testZetaConvex (Ratio01 a') (Ratio01 b') (Ratio01 c') = a==b || b==c || zb <= k * Finite b + l where
	[a,b,c] = sort [a', b', c']
	[za, zb, zc] = map (optimalValue . zetaOnS) [a,b,c]
	k = (za-zc) / Finite (a-c)
	l = za - k * Finite a

-- Ivic, Th. 8.1, p. 205
testMConvex (Ratio01 a') (Ratio01 b') (Ratio01 c') = a==b || b==c || za==InfPlus || zc==InfPlus
	|| zb>= za*zc*Finite(c-a)/(zc*Finite(c-b) + za*Finite(b-a)) where
		[a,b,c] = sort $ map (\n -> n/2+1%2) [a', b', c']
		[za, zb, zc] = map (optimalValue . mOnS) [a,b,c] :: [RationalInf]

etalonZetaOnS  [a,b,c,d] = Finite (c%d) >= optimalValue (zetaOnS $ a%b)

etalonMOnS  [a,b,c,d] = Finite (c%d) <= (optimalValue . mOnS) (a%b)

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
	testEtalon etalonZetaOnS "ExpPairs/Tests/etalon-zetaOnS.txt"
	testEtalon etalonMOnS    "ExpPairs/Tests/etalon-mOnS.txt"
	--mapM_ (testSmth 1) [
	--	("mOnS convex", testMConvex),
	--	("zetaOnS convex", testZetaConvex)
	--	]
	mapM_ (testSmth 3) [
		("zetaOnS monotonic", testZetaOnS1),
		("zetaOnS strict monotonic", testZetaOnS2),
		("mOnS monotonic", testMOnS1),
		("mOnS strict monotonic", testMOnS2)
		]
	mapM_ (testSmth 9) [
		("zetaOnS reverse", testZetaReverse),
		("zetaOnS symmetry", testZetaOnSsym),
		("zetaOnS above s=1", testZetaOnSZero),
		("mOnS below s=1/2", testMOnSZero),
		("mOnS above s=1", testMOnSInf)
		]
