module ExpPairs.Ivic where

import Data.Ratio

import ExpPairs.Optimize

zetaOnS :: Rational -> (Double, Rational, InitPair, Path)
zetaOnS s = optimizeWithConstraints
	[RationalForm (LinearForm 1 1 (-s)) 2]
	[Constraint (RationalForm (LinearForm (-1) 1 (-s)) 1) NonStrict]

lemma82_f :: Rational -> Rational
lemma82_f s
	| s < 1%2   = undefined
	| s<= 2%3   =  2/(3-4*s)
	| s<=11%14  = 10/(7-8*s)
	| s<=13%15  = 34/(15-16*s)
	| s<=57%62  = 98/(31-32*s)
	| otherwise =	 5/(1-s)

-- Ivic, (8.97)
-- R << T V^{-2f(sigma)} + T^alpha1 V^beta1 + T^alpha2 V^beta2
--
-- If a<1 then T^a V^b << T V^{b+(a-1)/muS}
--
-- (8.97) implies that alpha1 <= 1 for S >= 1/2
-- and that alpha2 <= 1 for S >= 2/3 or S >= 5/8 and
--          (4S-2)k + (8S-6)l + 2S-1 >=0

mOnS :: Rational -> Rational
mOnS s
	| s < 1%2 = undefined
	| s < 5%8 = 4/(3-4*s)
	| s>= 1   = 1000000
	| otherwise = x1 `min` x2 `min` (2*fS) where
		(_, muS, _, _) = zetaOnS s
		fS = lemma82_f s
		alpha1 = (4-4*s)/(1+2*s)
		beta1 = -12/(1+2*s)
		x1 = (1-alpha1)/muS - beta1

		--alpha2 = 4*(1-s)*(k+l)/((2*m+4*l)*s-m+2*k-2*l)
		--beta2  = -4*(m+2*k+2*l)/((2*m+4*l)*s-m+2*k-2*l)
		--ratio = (1-alpha2)/muS - beta2
		--numer = numerator ratio
		--denom = denominator ratio
		numer = LinearForm
			(-4*s + (-8*muS + 2))
			(-8*s + (-8*muS + 6))
			(-2*s + (-4*muS + 1))
		denom = LinearForm
			(2*muS)
			(4*muS*s - 2*muS)
			(2*muS*s - muS)

		cons = if s >= 2%3 then [] else [Constraint
			(RationalForm (LinearForm (4*s-2) (8*s-6) (2*s-1)) 1)  NonStrict
			]

		(_, x2', _, _) = optimizeWithConstraints [RationalForm numer denom] cons
		x2 = -x2'

