module ExpPairs.Tests.Ivic where

import Data.Ratio
import ExpPairs.Ivic
import ExpPairs.RatioInf

import Test.SmallCheck
import Test.SmallCheck.Series
import Test.QuickCheck

snd4 (_, a, _, _) = a

testZetaOnS1 a b = a==b || a<b && za>=zb || a>b && za<=zb where
	za = snd4 $ zetaOnS a
	zb = snd4 $ zetaOnS b

testZetaOnS2 a' b' = a==b || a<b && za>zb || a>b && za<zb where
	(_,a) = properFraction $ abs a'
	(_,b) = properFraction $ abs b'
	za = snd4 $ zetaOnS a
	zb = snd4 $ zetaOnS b

testZetaOnSsym a = abs (za-za') == abs (a-1%2) where
	za = snd4 $ zetaOnS a
	za' = snd4 $ zetaOnS (1-a)

testZetaOnSZero a = a<1 || snd4 (zetaOnS a) == 0


testMOnS1 a b = a==b || a<b && za<=zb || a>b && za>=zb where
	za = mOnS a
	zb = mOnS b

testMOnS2 a' b' = a==b || a<b && za<zb || a>b && za>zb where
	a = 1%2 + (snd $ properFraction $ abs a') / 2
	b = 1%2 + (snd $ properFraction $ abs b') / 2
	za = mOnS a
	zb = mOnS b

testMOnSZero a = a>=1%2 || mOnS a == 0

testMOnSInf a = a<1 || mOnS a == InfPlus

-- test convexity properties

testSmth depth (name, test) = do
	putStrLn name
	mapM_ (\_ -> quickCheck test) [1..1]
	smallCheck depth test

testSuite = do
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
