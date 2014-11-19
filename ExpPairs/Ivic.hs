module ExpPairs.Ivic where

import Data.Ratio
import Data.List
import Data.Ord

import ExpPairs.Optimize

zetaOnS :: Rational -> OptimizeResult
zetaOnS s
	| s >= 1  = simulateOptimize 0
	| s >= 1%2 = optimize
		[RationalForm (LinearForm 1 1 (-s)) 2]
		[Constraint (LinearForm (-1) 1 (-s)) NonStrict]
	| otherwise = optRes {optimalValue = r} where
		optRes = zetaOnS (1-s)
		r = Finite (1%2 - s) + optimalValue optRes

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

mOnS :: Rational -> OptimizeResult
mOnS s
	| s < 1%2 = simulateOptimize 0
	| s < 5%8 = simulateOptimize $ 4/(3-4*s)
	| s>= 1   = simulateOptimize' InfPlus
	| otherwise = minimumBy (comparing optimalValue) [x1, x2, simulateOptimize (lemma82_f s * 2)] where

		optRes = zetaOnS s
		muS    = toRational $ optimalValue optRes
		alpha1 = (4-4*s)/(1+2*s)
		beta1  = -12/(1+2*s)
		x1 = optRes {optimalValue = Finite $ (1-alpha1)/muS - beta1}

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
			(LinearForm (4*s-2) (8*s-6) (2*s-1)) NonStrict
			]

		x2' = optimize [RationalForm numer denom] cons
		x2 = x2' {optimalValue = negate $ optimalValue x2'}

checkAbscissa :: [(Rational, Rational)] -> Rational -> Bool
checkAbscissa xs s = sum rs < Finite 1 where
	qs = map (\(n,m) -> optimalValue (mOnS (n*s)) / Finite m) xs
	rs = map (\q -> 1/q) qs

searchMinAbscissa :: [(Rational, Rational)] -> Rational
searchMinAbscissa xs = searchMinAbscissa' from to where
	from = 1 % 2 / minimum (map fst xs)
	to   = 1 % 1
	searchMinAbscissa' a b
		| b-a < 1%1000000 = a
		| checkAbscissa xs ((a+b)/2) = searchMinAbscissa' a ((a+b)/2)
		| otherwise = searchMinAbscissa' ((a+b)/2) b
