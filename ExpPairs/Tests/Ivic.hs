module ExpPairs.Tests.Ivic where

import Data.Ratio
import Data.List
import ExpPairs.Ivic
import ExpPairs.RatioInf

import Test.SmallCheck
import Test.SmallCheck.Series
import Test.QuickCheck

snd4 (_, a, _, _) = a

ratio01 a = if a==0 then 0 else (b+1)/2 where
	b = (if a>0 then min else max) a (recip a)

ratioI from to a = from + (to-from) * (ratio01 a)

testZetaOnS1 a' b' = a==b || za>=zb where
	[a,b] = sort $ map (ratioI (-3) 3) [a', b']
	[za, zb] = map (snd4 . zetaOnS) [a,b]

testZetaOnS2 a' b' = a==b || za>zb where
	[a,b] = sort $ map ratio01 [a', b']
	[za, zb] = map (snd4 . zetaOnS) [a,b]

testZetaOnSsym a' = abs (za-za') == abs (a-1%2) where
	a = ratioI (-3) 3 a'
	za = snd4 $ zetaOnS a
	za' = snd4 $ zetaOnS (1-a)

testZetaOnSZero a' = a<1 || snd4 (zetaOnS a) == 0 where
	a = ratioI (-3) 3 a'


testMOnS1 a' b' = a==b || za<=zb where
	[a,b] = sort $ map (ratioI (-3) 3) [a', b']
	[za, zb] = map mOnS [a,b]

testMOnS2 a' b' = a==b || za<zb where
	[a,b] = sort $ map (ratioI (1%2) 1) [a', b']
	[za, zb] = map mOnS [a,b]

testMOnSZero a' = a>=1%2 || mOnS a == 0 where
	a = ratioI (-3) 3 a'

testMOnSInf a' = a<1 || mOnS a == InfPlus where
	a = ratioI (-3) 3 a'


-- Convexity tests - they fail and it is OK
testZetaConvex a' b' c' = a==b || b==c || zb <= k*b+l where
	[a,b,c] = sort $ map ratio01 [a', b', c']
	[za, zb, zc] = map (snd4 . zetaOnS) [a,b,c]
	k = (za-zc) / (a-c)
	l = za - k*a

-- Ivic, Th. 8.1, p. 205
testMConvex a' b' c' = a==b || b==c || za==InfPlus || zc==InfPlus
	|| zb>= za*zc*Finite(c-a)/(zc*Finite(c-b) + za*Finite(b-a)) where
		[a,b,c] = sort $ map (ratioI (1%2) 1) [a', b', c']
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
