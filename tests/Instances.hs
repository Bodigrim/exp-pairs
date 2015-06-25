{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module Instances (Ratio01 (..), Positive (..)) where

import Test.QuickCheck hiding (Positive)
import Test.SmallCheck.Series
import Control.Monad
import Control.Applicative

import Math.ExpPairs.ProcessMatrix
import Math.ExpPairs.Pair (InitPair' (..))
import Math.ExpPairs.Matrix3 as M3 (Matrix3, fromList, Vector3 (..))

instance Arbitrary Process where
	arbitrary = fmap f arbitrary where
		f x = if x then A else BA

instance Monad m => Serial m Process where
	series = cons0 A \/ cons0 BA

newtype Ratio01 t = Ratio01 t

instance (Ord t, Fractional t, Arbitrary t) => Arbitrary (Ratio01 t) where
	arbitrary = fmap (Ratio01 . ratio01) arbitrary

instance (Ord t, Fractional t, Serial m t) => Serial m (Ratio01 t) where
	series = cons1 (Ratio01 . ratio01)

instance Show t => Show (Ratio01 t) where
  showsPrec n (Ratio01 x) = showsPrec n x

ratio01 :: (Fractional a, Ord a) => a -> a
ratio01 a
	| abs a <= 1 = recip 2 + a/4
	| a < 0      = recip (negate a) / 4
	| otherwise  = 3 * recip 4 + recip a / 4

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

instance (Num a, Ord a, Arbitrary a) => Arbitrary (Positive a) where
  arbitrary =
    (Positive . abs) `fmap` (arbitrary `suchThat` (> 0))

instance (Arbitrary a) => Arbitrary (M3.Matrix3 a) where
  arbitrary = M3.fromList <$> vectorOf 9 arbitrary

instance (Arbitrary a) => Arbitrary (M3.Vector3 a) where
  arbitrary = (\[a,b,c] -> M3.Vector3 a b c) <$> vectorOf 3 arbitrary
