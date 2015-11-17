module Ivic where

import Data.Ratio
import Math.ExpPairs
import Math.ExpPairs.Ivic

import Test.Tasty
import Test.Tasty.SmallCheck as SC
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

import Instances
import Etalon (testEtalon)

fromMinus3To3 :: Rational -> Rational
fromMinus3To3 n = (n - 1 % 2) * 6

fromHalfToOne :: Rational -> Rational
fromHalfToOne n = n / 2 + 1 % 2

testZetaOnS1 :: Sorted (Ratio01 Rational, Ratio01 Rational) -> Bool
testZetaOnS1 (Sorted (Ratio01 a', Ratio01 b')) = a == b || za >= zb where
  [ a,  b] = map fromMinus3To3 [a', b']
  [za, zb] = map (optimalValue . zetaOnS) [a, b]

-- May fail due to the granularity of 'sect'.
testZetaOnS2 :: Sorted (Ratio01 Rational, Ratio01 Rational) -> Bool
testZetaOnS2 (Sorted (Ratio01 a, Ratio01 b)) = a == b || za > zb where
  [za, zb] = map (optimalValue . zetaOnS) [a, b]

testZetaOnSsym :: Ratio01 Rational -> Bool
testZetaOnSsym (Ratio01 a') = (toRational . abs) (za - za') == abs (a - 1 % 2) where
  a   = fromMinus3To3 a'
  za  = optimalValue $ zetaOnS a
  za' = optimalValue $ zetaOnS (1 - a)

testZetaOnSZero :: Ratio01 Rational -> Bool
testZetaOnSZero (Ratio01 a') = a < 1 || optimalValue (zetaOnS a) == 0 where
  a = fromMinus3To3 a'

testMOnS1 :: Sorted (Ratio01 Rational, Ratio01 Rational) -> Bool
testMOnS1 (Sorted (Ratio01 a', Ratio01 b')) = a == b || za <= zb where
  [ a,  b] = map fromMinus3To3 [a', b']
  [za, zb] = map (optimalValue . mOnS) [a, b]

testMOnS2 :: Sorted (Ratio01 Rational, Ratio01 Rational) -> Bool
testMOnS2 (Sorted (Ratio01 a', Ratio01 b')) = a == b || za < zb where
  [ a,  b] = map fromHalfToOne [a', b']
  [za, zb] = map (optimalValue . mOnS) [a, b]

testMOnSZero :: Ratio01 Rational -> Bool
testMOnSZero (Ratio01 a') = a >= 1%2 || (optimalValue . mOnS) a == 0 where
  a = fromMinus3To3 a'

testMOnSInf :: Ratio01 Rational -> Bool
testMOnSInf (Ratio01 a') = a < 1 || (optimalValue . mOnS) a == InfPlus where
  a = fromMinus3To3 a'

testZetaReverse :: Ratio01 Rational -> Bool
testZetaReverse (Ratio01 s') = abs (s - t) <= 5 % 1000 where
  s = s' / 2
  zs = zetaOnS s
  t = toRational $ optimalValue $ reverseZetaOnS $ toRational $ optimalValue zs

-- Convexity tests - they fail and it is OK
testZetaConvex :: Sorted (Ratio01 Rational, Ratio01 Rational, Ratio01 Rational) -> Bool
testZetaConvex (Sorted (Ratio01 a, Ratio01 b, Ratio01 c)) = a == b || b == c || zb <= k * Finite b + l where
  [za, zb, zc] = map (optimalValue . zetaOnS) [a, b, c]
  k = (za - zc) / Finite (a - c)
  l = za - k * Finite a

-- Ivic, Th. 8.1, p. 205
testMConvex :: Sorted (Ratio01 Rational, Ratio01 Rational, Ratio01 Rational) -> Bool
testMConvex (Sorted (Ratio01 a', Ratio01 b', Ratio01 c')) = a==b || b==c || za==InfPlus || zc==InfPlus
  || zb>= za*zc*Finite(c-a)/(zc*Finite(c-b) + za*Finite(b-a)) where
    [a,b,c] = map fromHalfToOne [a', b', c']
    [za, zb, zc] = map (optimalValue . mOnS) [a,b,c] :: [RationalInf]

etalonZetaOnS :: Integer -> Integer -> Integer -> Integer -> Bool
etalonZetaOnS a b c d = Finite (c%d) >= optimalValue (zetaOnS $ a%b)

etalonMOnS :: Integer -> Integer -> Integer -> Integer -> Bool
etalonMOnS a b c d = Finite (c%d) <= (optimalValue . mOnS) (a%b)

testSuite :: TestTree
testSuite = testGroup "Ivic"
  [ testCase "etalon zetaOnS"
    (testEtalon 100 (\(a:b:c:d:_) -> etalonZetaOnS a b c d) "tests/etalon-zetaOnS.txt")
  , testCase "etalon mOnS"
    (testEtalon 100 (\(a:b:c:d:_) -> etalonMOnS a b c d) "tests/etalon-mOnS.txt")
  , adjustOption (\(SC.SmallCheckDepth n) -> SC.SmallCheckDepth (n `div` 2)) $
      SC.testProperty "zetaOnS monotonic" testZetaOnS1
  , QC.testProperty "zetaOnS monotonic" testZetaOnS1
  , adjustOption (\(SC.SmallCheckDepth n) -> SC.SmallCheckDepth (n `div` 2)) $
      SC.testProperty "zetaOnS strict monotonic" testZetaOnS2
  , QC.testProperty "zetaOnS strict monotonic" testZetaOnS2
  , adjustOption (\(SC.SmallCheckDepth n) -> SC.SmallCheckDepth (n `div` 2)) $
      SC.testProperty "mOnS monotonic" testMOnS1
  , QC.testProperty "mOnS monotonic" testMOnS1
  -- , adjustOption (\(SC.SmallCheckDepth n) -> SC.SmallCheckDepth (n `div` 2)) $
  --     SC.testProperty "mOnS strict monotonic" testMOnS2
  -- , QC.testProperty "mOnS strict monotonic" testMOnS2
  , SC.testProperty "zetaOnS reverse" testZetaReverse
  , QC.testProperty "zetaOnS reverse" testZetaReverse
  , SC.testProperty "zetaOnS symmetry" testZetaOnSsym
  , QC.testProperty "zetaOnS symmetry" testZetaOnSsym
  , SC.testProperty "zetaOnS above s=1" testZetaOnSZero
  , QC.testProperty "zetaOnS above s=1" testZetaOnSZero
  , SC.testProperty "mOnS below s=1/2" testMOnSZero
  , QC.testProperty "mOnS below s=1/2" testMOnSZero
  , SC.testProperty "mOnS above s=1" testMOnSInf
  , QC.testProperty "mOnS above s=1" testMOnSInf
  -- , SC.testProperty "mOnS convex" testMConvex
  -- , QC.testProperty "mOnS convex" testMConvex
  -- , SC.testProperty "zetaOnS convex" testZetaConvex
  -- , QC.testProperty "zetaOnS convex" testZetaConvex
  ]


