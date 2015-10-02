{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, DeriveGeneric, CPP #-}
module Instances (Ratio01 (..), Positive (..)) where

import Test.QuickCheck hiding (Positive)
import Test.SmallCheck.Series
#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
#endif
import Control.Monad
import GHC.Generics          (Generic (..))

import Math.ExpPairs.LinearForm
import Math.ExpPairs.ProcessMatrix
import Math.ExpPairs.Pair (InitPair' (..))
import Math.ExpPairs.Matrix3 as M3 (Matrix3, fromList)

instance Arbitrary a => Arbitrary (LinearForm a) where
  arbitrary = LinearForm <$> arbitrary <*> arbitrary <*> arbitrary
  shrink = genericShrink

instance (Monad m, Serial m a) => Serial m (LinearForm a) where
  series = cons3 LinearForm

instance Arbitrary a => Arbitrary (RationalForm a) where
  arbitrary = RationalForm <$> arbitrary <*> arbitrary
  shrink = genericShrink

instance (Monad m, Serial m a) => Serial m (RationalForm a) where
  series = cons2 RationalForm

instance Arbitrary a => Arbitrary (Constraint a) where
  arbitrary = Constraint <$> arbitrary <*> arbitrary
  shrink = genericShrink

instance (Monad m, Serial m a) => Serial m (Constraint a) where
  series = cons2 Constraint

instance Arbitrary IneqType where
  arbitrary = f <$> arbitrary where
    f x = if x then Strict else NonStrict
  shrink = genericShrink

instance Monad m => Serial m IneqType where
  series = cons0 Strict \/ cons0 NonStrict

instance Arbitrary Process where
  arbitrary = f <$> arbitrary where
    f x = if x then A else BA
  shrink = genericShrink

instance Monad m => Serial m Process where
  series = cons0 A \/ cons0 BA

newtype Ratio01 t = Ratio01 t
  deriving (Generic)

instance (Ord t, Fractional t, Arbitrary t) => Arbitrary (Ratio01 t) where
  arbitrary = (Ratio01 . ratio01) <$> arbitrary
  shrink = genericShrink

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
  arbitrary = f <$> liftM2 (,) arbitrary arbitrary where
    f :: (Num t, Ord t, Fractional t) => (Ratio01 t, Ratio01 t) -> InitPair' t
    f (Ratio01 x, Ratio01 y)
      | 100*x<5   = Corput01
      | 100*x<10  = Corput12
      | otherwise = Mix x' y' where
        x' = x*10/9
        y' = y*(1-x)
  shrink = genericShrink

instance Serial m t => Serial m (InitPair' t) where
  series = cons0 Corput01 \/ cons0 Corput12 \/ cons2 Mix

instance (Num a, Ord a, Arbitrary a) => Arbitrary (Positive a) where
  arbitrary = Positive <$> (arbitrary `suchThat` (> 0))
  shrink (Positive x) = Positive <$> filter (> 0) (shrink x)

instance (Arbitrary a) => Arbitrary (M3.Matrix3 a) where
  arbitrary = M3.fromList <$> vectorOf 9 arbitrary
  shrink = genericShrink
