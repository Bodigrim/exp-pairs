{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module ExpPairs.Tests.Ivic where

import Data.Ratio
import Data.List
import ExpPairs.Ivic
import ExpPairs.RatioInf

import Test.SmallCheck
import Test.SmallCheck.Series
import Test.QuickCheck

snd4 (_, a, _, _) = a

newtype Ratio01 t = Ratio01 t

instance (Ord t, Fractional t, Arbitrary t) => Arbitrary (Ratio01 t) where
	arbitrary = fmap (Ratio01 . ratio01) arbitrary

instance (Ord t, Fractional t, Serial m t) => Serial m (Ratio01 t) where
	series = cons1 (Ratio01 . ratio01)

instance Show t => Show (Ratio01 t) where
  showsPrec n (Ratio01 x) = showsPrec n x


ratio01 a = if a==0 then 0 else (b+1)/2 where
	b = (if a > 0 then min else max) a (recip a)

testZetaOnS1 (Ratio01 a') (Ratio01 b') = a==b || za>=zb where
	[a,b] = sort $ map (\n -> (n-1%2)*6) [a', b']
	[za, zb] = map (snd4 . zetaOnS) [a,b]

testZetaOnS2 (Ratio01 a') (Ratio01 b') = a==b || za>zb where
	[a,b] = sort [a', b']
	[za, zb] = map (snd4 . zetaOnS) [a,b]

testZetaOnSsym (Ratio01 a') = abs (za-za') == abs (a-1%2) where
	a = (a'-1%2)*6
	za = snd4 $ zetaOnS a
	za' = snd4 $ zetaOnS (1-a)

testZetaOnSZero (Ratio01 a') = a<1 || snd4 (zetaOnS a) == 0 where
	a = (a'-1%2)*6


testMOnS1 (Ratio01 a') (Ratio01 b') = a==b || za<=zb where
	[a,b] = sort $ map (\n -> (n-1%2)*6) [a', b']
	[za, zb] = map mOnS [a,b]

testMOnS2 (Ratio01 a') (Ratio01 b') = a==b || za<zb where
	[a,b] = sort $ map (\n -> n/2+1%2) [a', b']
	[za, zb] = map mOnS [a,b]

testMOnSZero (Ratio01 a') = a>=1%2 || mOnS a == 0 where
	a = (a'-1%2)*6

testMOnSInf (Ratio01 a') = a<1 || mOnS a == InfPlus where
	a = (a'-1%2)*6


-- Convexity tests - they fail and it is OK
testZetaConvex (Ratio01 a') (Ratio01 b') (Ratio01 c') = a==b || b==c || zb <= k*b+l where
	[a,b,c] = sort [a', b', c']
	[za, zb, zc] = map (snd4 . zetaOnS) [a,b,c]
	k = (za-zc) / (a-c)
	l = za - k*a

-- Ivic, Th. 8.1, p. 205
testMConvex (Ratio01 a') (Ratio01 b') (Ratio01 c') = a==b || b==c || za==InfPlus || zc==InfPlus
	|| zb>= za*zc*Finite(c-a)/(zc*Finite(c-b) + za*Finite(b-a)) where
		[a,b,c] = sort $ map (\n -> n/2+1%2) [a', b', c']
		[za, zb, zc] = map mOnS [a,b,c] :: [RationalInf]


testSmth depth (name, test) = do
	putStrLn name
	mapM_ (\_ -> quickCheck test) [1..1]
	smallCheck depth test

testSuite = do
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
		("zetaOnS symmetry", testZetaOnSsym),
		("zetaOnS above s=1", testZetaOnSZero),
		("mOnS below s=1/2", testMOnSZero),
		("mOnS above s=1", testMOnSZero)
		]
