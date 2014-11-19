{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module ExpPairs.Tests.Pair where

import Data.Ratio
import ExpPairs.Pair

import Test.SmallCheck
import Test.SmallCheck.Series
import Test.QuickCheck
import Control.Monad

newtype Ratio01 t = Ratio01 t

instance (Ord t, Fractional t, Arbitrary t) => Arbitrary (Ratio01 t) where
	arbitrary = fmap (Ratio01 . ratio01) arbitrary

ratio01 a = if a==0 then 0 else (b+1)/2 where
	b = (if a > 0 then min else max) a (recip a)


instance (Ord t, Fractional t, Arbitrary t) => Arbitrary (InitPair' t) where
	arbitrary = fmap f (liftM2 (,) arbitrary arbitrary) where
		f :: (Num t, Ord t, Fractional t) => (Ratio01 t, Ratio01 t) -> InitPair' t
		f (Ratio01 x, Ratio01 y)
			| 100*x<5 = Corput01
			| 100*x<10 = Corput12
			| otherwise = Mix x' y' where
				x' = x*10/9
				y' = y*(1-x)

instance Serial m t => Serial m (InitPair' t) where
	series = cons0 Corput01 \/ cons0 Corput12 \/ cons2 Mix


testBounds :: InitPair -> Bool
testBounds ip = k>=0 && k<=1%2 && l>=1%2 && l<=1 where
	(k, l) = initPairToValue ip

testSmth depth (name, test) = do
	putStrLn name
	mapM_ (\_ -> quickCheck test) [1..1]
	smallCheck depth test

testSuite = do
	testSmth 5 ("bounds", testBounds)
