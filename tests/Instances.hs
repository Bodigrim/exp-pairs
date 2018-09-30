{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, DeriveGeneric, CPP #-}
module Instances (Ratio01 (..), Positive (..), Sorted(..)) where

import Test.QuickCheck (Arbitrary(..), Gen, genericShrink, suchThat, vectorOf)
import Test.SmallCheck.Series
import Control.Applicative
import Control.Monad
#if __GLASGOW_HASKELL__ < 710
import Data.Foldable
#endif
import GHC.Generics          (Generic (..))

import Math.ExpPairs.LinearForm
import Math.ExpPairs.Process
import Math.ExpPairs.ProcessMatrix
import Math.ExpPairs.Pair (InitPair' (..))
import Math.ExpPairs.Matrix3 as M3 (Matrix3, fromList)

instance Arbitrary a => Arbitrary (LinearForm a) where
  arbitrary = LinearForm <$> arbitrary <*> arbitrary <*> arbitrary
  shrink = genericShrink

instance (Monad m, Serial m a) => Serial m (LinearForm a) where
  series = cons3 LinearForm

instance Arbitrary a => Arbitrary (RationalForm a) where
  arbitrary = (:/:) <$> arbitrary <*> arbitrary
  shrink = genericShrink

instance (Monad m, Serial m a) => Serial m (RationalForm a) where
  series = cons2 (:/:)

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
  deriving (Eq, Ord, Generic)

instance (Ord t, Fractional t, Arbitrary t) => Arbitrary (Ratio01 t) where
  arbitrary = Ratio01 <$> (arbitrary `suchThat` (\x -> 0 <= x && x <= 1))
  shrink (Ratio01 y) = Ratio01 <$> filter (\x -> 0 <= x && x <= 1) (shrink y)

instance (Ord t, Fractional t, Serial m t) => Serial m (Ratio01 t) where
  series = Ratio01 <$> (series `suchThatSerial` (\x -> 0 <= x && x <= 1))

instance Show t => Show (Ratio01 t) where
  showsPrec n (Ratio01 x) = showsPrec n x

instance (Ord t, Fractional t, Arbitrary t) => Arbitrary (InitPair' t) where
  arbitrary = f <$> liftM2 (,) arbitrary arbitrary where
    f :: (Ord t, Fractional t) => (Ratio01 t, Ratio01 t) -> InitPair' t
    f (Ratio01 x, Ratio01 y)
      | 100*x<5   = Corput01
      | 100*x<10  = Corput12
      | otherwise = Mix x' y' where
        x' = x*10/9
        y' = y*(1-x)
  shrink = genericShrink

instance (Ord t, Fractional t, Serial m t) => Serial m (InitPair' t) where
  series = cons0 Corput01 \/ cons0 Corput12 \/ mseries
    where
      mseries = do
        (Ratio01 x) <- series
        (Ratio01 y) <- series
        return $ Mix x (y * (1-x))

instance (Num a, Ord a, Arbitrary a) => Arbitrary (Positive a) where
  arbitrary = Positive <$> (arbitrary `suchThat` (> 0))
  shrink (Positive x) = Positive <$> filter (> 0) (shrink x)

instance (Arbitrary a) => Arbitrary (M3.Matrix3 a) where
  arbitrary = M3.fromList <$> vectorOf 9 arbitrary
  shrink = genericShrink

suchThatSerial :: Series m a -> (a -> Bool) -> Series m a
suchThatSerial s p = s >>= \x -> if p x then pure x else empty

cons5 :: (Serial m a, Serial m b, Serial m c, Serial m d, Serial m e) =>
         (a->b->c->d->e->f) -> Series m f
cons5 f = decDepth $
  f <$> series
    <~> series
    <~> series
    <~> series
    <~> series

instance (Serial m a, Serial m b, Serial m c, Serial m d, Serial m e) => Serial m (a,b,c,d,e) where
  series = cons5 (,,,,)

cons6 :: (Serial m a, Serial m b, Serial m c, Serial m d, Serial m e, Serial m f) =>
         (a->b->c->d->e->f->g) -> Series m g
cons6 f = decDepth $
  f <$> series
    <~> series
    <~> series
    <~> series
    <~> series
    <~> series

instance (Serial m a, Serial m b, Serial m c, Serial m d, Serial m e, Serial m f) => Serial m (a,b,c,d,e,f) where
  series = cons6 (,,,,,)

newtype Sorted t = Sorted t
  deriving (Show, Generic)

instance (Ord t, Arbitrary t) => Arbitrary (Sorted (t, t)) where
  arbitrary = Sorted <$> (arbitrary `suchThat` uncurry (<=))

instance (Ord t, Serial m t) => Serial m (Sorted (t, t)) where
  series = Sorted <$> (series `suchThatSerial` uncurry (<=))

instance (Ord t, Arbitrary t) => Arbitrary (Sorted (t, t, t)) where
  arbitrary = Sorted <$> (arbitrary `suchThat` (\(a, b, c) -> a <= b && b <= c))

instance (Ord t, Serial m t) => Serial m (Sorted (t, t, t)) where
  series = Sorted <$> (series `suchThatSerial` (\(a, b, c) -> a <= b && b <= c))

instance (Ord t, Arbitrary t) => Arbitrary (Sorted (t, t, t, t)) where
  arbitrary = Sorted <$> (arbitrary `suchThat` (\(a, b, c, d) -> a <= b && b <= c && c <= d))

instance (Ord t, Serial m t) => Serial m (Sorted (t, t, t, t)) where
  series = Sorted <$> (series `suchThatSerial` (\(a, b, c, d) -> a <= b && b <= c && c <= d))

instance (Ord t, Arbitrary t) => Arbitrary (Sorted (t, t, t, t, t)) where
  arbitrary = Sorted <$> (arbitrary `suchThat` (\(a, b, c, d, e) -> a <= b && b <= c && c <= d && d <= e))

instance (Ord t, Serial m t) => Serial m (Sorted (t, t, t, t, t)) where
  series = Sorted <$> (series `suchThatSerial` (\(a, b, c, d, e) -> a <= b && b <= c && c <= d && d <= e))

instance (Ord t, Arbitrary t) => Arbitrary (Sorted (t, t, t, t, t, t)) where
  arbitrary = Sorted <$> (arbitrary `suchThat` (\(a, b, c, d, e, f) -> a <= b && b <= c && c <= d && d <= e && e <= f))

instance (Ord t, Serial m t) => Serial m (Sorted (t, t, t, t, t, t)) where
  series = Sorted <$> (series `suchThatSerial` (\(a, b, c, d, e, f) -> a <= b && b <= c && c <= d && d <= e && e <= f))


instance Arbitrary Path where
  arbitrary = foldMap (\x -> if x then aPath else baPath) <$> (arbitrary :: Gen [Bool])

instance Monad m => Serial m Path where
  series = foldMap (\x -> if x then aPath else baPath) <$> (series :: Monad m => Series m [Bool])
