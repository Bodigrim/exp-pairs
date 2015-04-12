{-|
Module      : Math.ExpPairs.LinearForm
Description : Linear forms, rational forms and constraints
Copyright   : (c) Andrew Lelechenko, 2014-2015
License     : GPL-3
Maintainer  : andrew.lelechenko@gmail.com
Stability   : experimental
Portability : POSIX

Provides types for rational forms (to hold objective functions in "Math.ExpPairs") and linear contraints (to hold constraints of optimization). Both of them are built atop of projective linear forms.
-}
module Math.ExpPairs.LinearForm
	( LinearForm (..)
	, evalLF
	, substituteLF
	, RationalForm (..)
	, evalRF
	, IneqType (..)
	, Constraint (..)
	, checkConstraint
	) where

import Data.List   (intercalate)
import Data.Ratio  (numerator, denominator)
import Data.Monoid (Monoid, mempty, mappend)

import Math.ExpPairs.RatioInf

-- |Define an affine linear form of two variables: a*k + b*l + c*m.
-- First argument of 'LinearForm' stands for a, second for b
-- and third for c. Linear forms form a monoid by addition.
data LinearForm t = LinearForm t t t
	deriving (Eq)

instance (Num t, Eq t, Show t) => Show (LinearForm t) where
	show (LinearForm a b c) = if (a==0) && (b==0) && (c==0)
		then "0"
		else "(" ++ intercalate " + " (filter (/=[]) $
			[if a/= 0 then show a ++ "k" else []] ++
			[if b/= 0 then show b ++ "l" else []] ++
			[if c/= 0 then show c ++ "m" else []] ) ++ ")" -- where
			-- show' :: Rational -> String
			-- show' z = if denominator z==1 then show (numerator z) else show z

instance Num t => Num (LinearForm t) where
	(LinearForm a b c) + (LinearForm d e f) = LinearForm (a+d) (b+e) (c+f)
	(*) = undefined
	negate (LinearForm a b c) = LinearForm (negate a) (negate b) (negate c)
	abs = undefined
	signum = undefined
	fromInteger n = LinearForm 0 0 (fromInteger n)

instance Num t => Monoid (LinearForm t) where
	mempty = 0
	mappend = (+)

scaleLF :: (Num t, Eq t) => t -> LinearForm t -> LinearForm t
scaleLF 0 (LinearForm {}) = LinearForm 0 0 0
scaleLF s (LinearForm a b c) = LinearForm (a*s) (b*s) (c*s)

-- |Evaluate a linear form a*k + b*l + c*m for given k, l and m.
evalLF :: Num t => (t, t, t) -> LinearForm t -> t
evalLF (k, l, m) (LinearForm a b c) = a*k+l*b+m*c

-- |Substitute linear forms k, l and m into a given linear form
-- a*k + b*l + c*m to obtain a new linear form.
substituteLF :: (Eq t, Num t) => (LinearForm t, LinearForm t, LinearForm t) -> LinearForm t -> LinearForm t
substituteLF (k, l, m) (LinearForm a b c) = scaleLF a k + scaleLF b l + scaleLF c m

-- | Define a rational form of two variables, equal to the ratio of two 'LinearForm'.
data RationalForm t = RationalForm (LinearForm t) (LinearForm t)
	deriving (Show)

instance Num t => Num (RationalForm t) where
	(+) = undefined
	(*) = undefined
	negate (RationalForm a b) = RationalForm (negate a) b
	abs = undefined
	signum = undefined
	fromInteger n = RationalForm (fromInteger n) 1

instance Num t => Fractional (RationalForm t) where
	fromRational r = RationalForm (fromInteger $ numerator r) (fromInteger $ denominator r)
	recip (RationalForm a b) = RationalForm b a

-- |Evaluate a rational form (a*k + b*l + c*m) \/ (a'*k + b'*l + c'*m)
-- for given k, l and m.
evalRF :: (Real t, Num t) => (Integer, Integer, Integer) -> RationalForm t -> RationalInf
evalRF (k', l', m') (RationalForm num den) = if denom==0 then InfPlus else Finite (numer / denom) where
	k = fromInteger k'
	l = fromInteger l'
	m = fromInteger m'
	numer = toRational $ evalLF (k, l, m) num
	denom = toRational $ evalLF (k, l, m) den

-- |Constants to specify the strictness of 'Constraint'.
data IneqType
	-- | Strict inequality (>0).
	= Strict
	-- | Non-strict inequality (≥0).
	| NonStrict
	deriving (Eq, Show)

-- |A linear constraint of two variables.
data Constraint t = Constraint (LinearForm t) IneqType
	deriving (Show)

-- |Evaluate a rational form of constraint and compare
-- its value with 0. Strictness depends on the given 'IneqType'.
checkConstraint :: (Num t, Eq t) => (Integer, Integer, Integer) -> Constraint t -> Bool
checkConstraint (k', l', m') (Constraint lf ineq)
	= if ineq==NonStrict
		then signum numer /= -1
		else signum numer == 1 where
			k = fromInteger k'
			l = fromInteger l'
			m = fromInteger m'
			numer = evalLF (k, l, m) lf
