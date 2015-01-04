{-|
Module      : Math.ExpPairs.RatioInf
Description : Rational numbers with infinities
Copyright   : (c) Andrew Lelechenko, 2014-2015
License     : GPL-3
Maintainer  : andrew.lelechenko@gmail.com
Stability   : experimental
Portability : POSIX

Provides types and necessary instances for rational numbers, extended with infinite values. Just use 'RationalInf' instead of 'Rational' from "Data.Ratio".
-}
module Math.ExpPairs.RatioInf (RatioInf (..), RationalInf) where

import Data.Ratio

-- |Extends a rational type with positive and negative
-- infinities.
data RatioInf t = InfMinus | Finite (Ratio t) | InfPlus
	deriving (Ord, Eq)

-- |Arbitrary-precision rational numbers with positive and negative
-- infinities.
type RationalInf = RatioInf Integer

instance (Integral t, Show t) => Show (RatioInf t) where
	show InfMinus   = "-Inf"
	show (Finite x) = show x
	show InfPlus    = "+Inf"

instance (Integral t) => Num (RatioInf t) where
	InfMinus + InfPlus = error "Cannot add up negative and positive infinities"
	InfPlus + InfMinus = error "Cannot add up negative and positive infinities"
	InfMinus + _ = InfMinus
	InfPlus + _  = InfPlus
	_ + InfMinus = InfMinus
	_ + InfPlus  = InfPlus
	(Finite a) + (Finite b) = Finite (a+b)

	fromInteger n = Finite (fromInteger n)

	signum InfMinus   = Finite (-1)
	signum InfPlus    = Finite 1
	signum (Finite r) = Finite (signum r)

	abs InfMinus   = InfPlus
	abs InfPlus    = InfPlus
	abs (Finite r) = Finite (abs r)

	negate InfMinus   = InfPlus
	negate InfPlus    = InfMinus
	negate (Finite r) = Finite (negate r)

	InfMinus * a
		| signum a == Finite 0    = error "Cannot multiply infinity by zero"
		| signum a == Finite 1    = InfMinus
		| signum a == Finite (-1) = InfPlus
	InfPlus  * a
		| signum a == Finite 0    = error "Cannot multiply infinity by zero"
		| signum a == Finite 1    = InfPlus
		| signum a == Finite (-1) = InfMinus
	a * InfMinus
		| signum a == Finite 0    = error "Cannot multiply infinity by zero"
		| signum a == Finite 1    = InfMinus
		| signum a == Finite (-1) = InfPlus
	a * InfPlus
		| signum a == Finite 0    = error "Cannot multiply infinity by zero"
		| signum a == Finite 1    = InfPlus
		| signum a == Finite (-1) = InfMinus
	(Finite a) * (Finite b)     = Finite (a*b)

instance (Integral t) => Fractional (RatioInf t) where
	fromRational = Finite . fromRational

	InfMinus / InfMinus = error "Cannot divide infinity by infinity"
	InfMinus / InfPlus  = error "Cannot divide infinity by infinity"
	InfMinus / (Finite a)
		| signum a ==  0 = error "Cannot divide infinity by zero"
		| signum a ==  1 = InfMinus
		| signum a == -1 = InfPlus

	InfPlus  / InfMinus = error "Cannot divide infinity by infinity"
	InfPlus  / InfPlus  = error "Cannot divide infinity by infinity"
	InfPlus / (Finite a)
		| signum a ==  0 = error "Cannot divide infinity by zero"
		| signum a ==  1 = InfPlus
		| signum a == -1 = InfMinus

	(Finite _) / InfPlus  = Finite 0
	(Finite _) / InfMinus = Finite 0

	(Finite _) / (Finite 0) = error "Cannot divide finite value by zero"
	(Finite a) / (Finite b) = Finite (a/b)

instance (Integral t) => Real (RatioInf t) where
	toRational (Finite r) = toRational r
	toRational InfPlus    = error "Cannot map infinity into Rational"
	toRational InfMinus   = error "Cannot map infinity into Rational"
