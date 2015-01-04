{-|
Module      : Math.ExpPairs.RatioInf
Description : Initial exponent pairs
Copyright   : (c) Andrew Lelechenko, 2014-2015
License     : GPL-3
Maintainer  : andrew.lelechenko@gmail.com
Stability   : experimental
Portability : POSIX

Provides a set of initial exponent pairs, consisting
of two points (0, 1), (1\/2, 1\/2) and a triangle with vertices in (1\/6, 2\/3), (2\/13, 35\/52) and (32\/205, 269\/410). The triangle is represented as a list of nodes of a net, covering the triangle.

Below /A/ and /B/ stands for van der Corput's processes.
-}
module Math.ExpPairs.Pair (Triangle (..), InitPair' (..), InitPair, initPairs, initPairToValue) where

import Data.Ratio

-- |Vertices of the triangle of initial exponent pairs.
data Triangle
	-- |Usual van der Corput exponent pair
	-- (1\/6, 2\/3) = /AB/(0, 1).
	= Corput16
	-- |An exponent pair (2\/13, 35\/52) from /Huxley M. N./
	-- `Exponential sums and the Riemann zeta function'
	-- \/\/ Proceedings of the International Number
	-- Theory Conference held at Universite Laval in 1987, Walter de Gruyter, 1989, P. 417-423.
	| HuxW87b1
	-- | An exponent pair (32\/205, 269\/410) from /Huxley M. N./
	-- `Exponential sums and the Riemann zeta function V' \/\/
  -- Proc. Lond. Math. Soc., 2005, Vol. 90, no. 1., P. 1--41.
	| Hux05
	deriving (Show, Bounded, Enum, Eq, Ord)

-- |Type to hold an initial exponent pair.
data InitPair' t
	-- |Usual van der Corput exponent pair
	-- (0, 1).
	= Corput01
	-- |Usual van der Corput exponent pair
	-- (1\/2, 1\/2) = /B/(0, 1).
	| Corput12
	-- |Point from the interior of 'Triangle'.
	-- Exactly
	-- 'Mix' a b = a * 'Corput16' + b * 'HuxW87b1' + (1-a-b) * 'Hux05'
	| Mix t t
	deriving (Eq)

-- |Exponent pair built from rational fractions of
-- 'Corput16', 'HuxW87b1' and 'Hux05'
type InitPair = InitPair' Rational

instance (Show t, Num t, Eq t) => Show (InitPair' t) where
	show Corput01 = "(0, 1)"
	show Corput12 = "(1/2, 1/2)"
	show (Mix r1 r2) =
		s1 ++ (if s1/="" && (s2/=""||s3/="") then " + " else "")
		++ s2 ++ (if s2/="" && s3/="" then " + " else "") ++ s3
		where
			r3 = 1 - r1 - r2
			f r t = if r==0 then "" else (if r==1 then "" else show r ++ " * ") ++ show t
			s1 = f r1 Corput16
			s2 = f r2 HuxW87b1
			s3 = f r3 Hux05

sect :: Integer
sect = 30

-- |The set of initial exponent pairs. It consists of
-- 'Corput01', 'Corput12' and 496 = sum [1..31] 'Mix'-points,
-- which forms a uniform net over 'Triangle'.
initPairs :: [InitPair]
initPairs = Corput01 : Corput12 : [Mix (r1%sect) (r2%sect) | r1<-[0..sect], r2<-[0..sect-r1]]

-- |Convert initial exponent pair from its symbolic representation
-- as 'InitPair' to pair of rationals.
initPairToValue :: InitPair -> (Rational, Rational)
initPairToValue Corput01 = (0, 1)
initPairToValue Corput12 = (1%2, 1%2)
initPairToValue (Mix r1 r2) = (x, y) where
	r3 = 1 - r1 - r2
	(x1, y1) = (1%6, 2%3)
	(x2, y2) = ( 2 %  13,  35 %  52)
	(x3, y3) = (32 % 205, 269 % 410)
	x = x1*r1 + x2*r2 + x3*r3
	y = y1*r1 + y2*r2 + y3*r3

