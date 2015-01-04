module Math.ExpPairs.Tests.Matrix3 where

import qualified Data.Matrix as M
import qualified Math.ExpPairs.Matrix3 as M3

import Test.QuickCheck

instance (Arbitrary a) => Arbitrary (M3.Matrix3 a) where
  arbitrary = fmap M3.fromList $ vectorOf 9 arbitrary

instance (Arbitrary a) => Arbitrary (M3.Vector3 a) where
  arbitrary = fmap (\[a,b,c] -> M3.Vector3 a b c) $ vectorOf 3 arbitrary

toM = M.fromList 3 3 . M3.toList
toM3 = M3.fromList . M.toList

testOp :: (M3.Matrix3 Integer -> M3.Matrix3 Integer -> M3.Matrix3 Integer) -> (M.Matrix Integer -> M.Matrix Integer -> M.Matrix Integer) -> M3.Matrix3 Integer -> M3.Matrix3 Integer -> Bool
testOp op1 op2 m1 m2 = m'==m'' where
	m'  = toM $ m1 `op1` m2
	m'' = toM m1 `op2` toM m2

testDet1 :: M3.Matrix3 Integer -> Bool
testDet1 m = M3.det m == M.detLaplace (toM m)

testDet2 :: M3.Matrix3 Rational -> Bool
testDet2 m = M3.det m == M.detLU (toM m)

testRecip :: M3.Matrix3 Rational -> Bool
testRecip m = M3.det m==0 || m/=m' && m==m'' && M3.det m * M3.det m' == 1 where
	m' = recip m
	m'' = recip m'

testConv :: M3.Matrix3 Integer -> Bool
testConv m = (toM3 . toM) m == m

testNormalize :: Integer -> M3.Matrix3 Integer -> Bool
testNormalize a m = (M3.normalize m' == m') && (a==0 || a>0 && m'==m'' || a<0 && m'==negate m'') where
	m' = M3.normalize m
	m'' = M3.normalize (m * fromInteger a)

testMultCol :: M3.Matrix3 Integer -> M3.Vector3 Integer -> Bool
testMultCol m v@(M3.Vector3 v1 v2 v3) = a==a' && b==b' && c==c' where
	(M3.Vector3 a b c) = M3.multCol m v
	[a', b', c'] = M.toList $ toM m * M.fromList 3 1 [v1, v2, v3]


testSmth _ (name, test) = do
	putStrLn name
	mapM_ (\_ -> quickCheck test) [1::Integer .. 1]

testSuite :: IO ()
testSuite = do
	mapM_ (testSmth 1) [
		("matrix plus", testOp (+) (+)),
		("matrix minus", testOp (-) (-)),
		("matrix mult", testOp (*) (*))
		]
	mapM_ (testSmth 1) [
		("matrix det1", testDet1),
		("matrix conversion", testConv)
		]
	mapM_ (testSmth 1) [
		("matrix det2", testDet2),
		("matrix recip", testRecip)
		]
	mapM_ (testSmth 1) [
		("matrix normalize", testNormalize)
		]
	mapM_ (testSmth 1) [
		("matrix mult column", testMultCol)
		]
