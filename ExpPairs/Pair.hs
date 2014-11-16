module ExpPairs.Pair where

import Data.Ratio
import Data.List
import qualified ExpPairs.Matrix3 as Mx

import ExpPairs.Process

data InitPair = Usual | HuxW87b1 | Hux05
	deriving (Show, Bounded, Enum)

markToPair :: InitPair -> (Rational, Rational)
markToPair Usual    = (0, 1)
markToPair HuxW87b1 = ( 2 %  13,  35 %  52)
markToPair Hux05    = (32 % 205, 269 % 410)


fracs2proj :: (Rational, Rational) -> (Integer, Integer, Integer)
fracs2proj (q, r) = (k, l, m) where
	dq = denominator q
	dr = denominator r
	m = lcm dq dr
	k = numerator q * (m `div` dq)
	l = numerator r * (m `div` dr)

proj2fracs :: (Integer, Integer, Integer) -> (Rational, Rational)
proj2fracs (k, l, m) = (k%m, l%m)

evalPath :: Path -> InitPair -> (Rational, Rational)
evalPath (Path m _) mark = (k, l) where
	m' = fmap fromInteger m
	(a, b, c) = fracs2proj $ markToPair mark
	(Mx.Vector3 a' b' c') = Mx.multCol m' (Mx.Vector3 a b c)
	(k, l) = proj2fracs (a', b', c')
