{-|
Module      : Math.ExpPairs.RatioInf
Copyright   : (c) Andrew Lelechenko, 2014-2020
License     : GPL-3
Maintainer  : andrew.lelechenko@gmail.com

Rational numbers extended with infinity.
-}

{-# LANGUAGE Safe #-}

module Math.ExpPairs.RatioInf
  ( RatioInf (..)
  , RationalInf
  ) where

import Data.Ratio (Ratio, numerator, denominator)
import Prettyprinter

-- | Extend 'Ratio' @t@ with \( \infty \) infinity.
data RatioInf t
  = Finite !(Ratio t) -- ^ Finite value
  | Infinity          -- ^ Infinity
  deriving (Eq, Ord, Show)

-- |Arbitrary-precision rational numbers with infinity.
type RationalInf = RatioInf Integer

instance (Integral t, Pretty t) => Pretty (RatioInf t) where
  pretty (Finite x)
    | denominator x == 1 = pretty (numerator x)
    | otherwise          = pretty (numerator x) <+> pretty "/" <+> pretty (denominator x)
  pretty Infinity    = pretty "Inf"

instance Integral t => Num (RatioInf t) where
  Infinity + _ = Infinity
  _ + Infinity = Infinity
  (Finite a) + (Finite b) = Finite (a+b)
  {-# SPECIALIZE (+) :: RationalInf -> RationalInf -> RationalInf #-}

  fromInteger = Finite . fromInteger
  {-# SPECIALIZE fromInteger :: Integer -> RationalInf #-}

  signum Infinity   = Finite 1
  signum (Finite r) = Finite (signum r)
  {-# SPECIALIZE signum :: RationalInf -> RationalInf #-}

  abs Infinity   = Infinity
  abs (Finite r) = Finite (abs r)
  {-# SPECIALIZE abs :: RationalInf -> RationalInf #-}

  negate Infinity   = Infinity
  negate (Finite r) = Finite (negate r)
  {-# SPECIALIZE negate :: RationalInf -> RationalInf #-}

  Infinity * Infinity = Infinity
  Infinity * Finite a = case signum a of
    1  -> Infinity
    -1 -> Infinity
    _  -> error "Cannot multiply infinity by zero"
  Finite a * Infinity = case signum a of
    1  -> Infinity
    -1 -> Infinity
    _  -> error "Cannot multiply infinity by zero"
  Finite a * Finite b = Finite (a * b)
  {-# SPECIALIZE (*) :: RationalInf -> RationalInf -> RationalInf #-}

instance Integral t => Fractional (RatioInf t) where
  fromRational = Finite . fromRational
  {-# SPECIALIZE fromRational :: Rational -> RationalInf #-}

  Infinity / Infinity = error "Cannot divide infinity by infinity"
  Infinity / Finite a = case signum a of
    1  -> Infinity
    -1 -> Infinity
    _  -> error "Cannot divide infinity by zero"
  Finite _ / Infinity = Finite 0
  Finite 0 / Finite 0 = error "Cannot divide zero by zero"
  Finite _ / Finite 0 = Infinity
  Finite a / Finite b = Finite (a / b)
  {-# SPECIALIZE (/) :: RationalInf -> RationalInf -> RationalInf #-}

instance Integral t => Real (RatioInf t) where
  toRational (Finite r) = toRational r
  toRational Infinity   = error "Cannot convert infinity to Rational"
  {-# SPECIALIZE toRational :: RationalInf -> Rational #-}
