module Math.ExpPairs.Ivic where

import Data.Ratio
import Data.List
import Data.Ord

import Math.ExpPairs

zetaOnS :: Rational -> OptimizeResult
zetaOnS s
	| s >= 1  = simulateOptimize 0
	| s >= 1%2 = optimize
		[RationalForm (LinearForm 1 1 (-s)) 2]
		[Constraint (LinearForm (-1) 1 (-s)) NonStrict]
	| otherwise = optRes {optimalValue = r} where
		optRes = zetaOnS (1-s)
		r = Finite (1%2 - s) + optimalValue optRes

zetaOnHalf :: Rational
zetaOnHalf = 32%205

reverseZetaOnS :: Rational -> OptimizeResult
reverseZetaOnS mu
	| mu >= 1%2   = simulateOptimize 0
	| mu > zetaOnHalf = optimize [RationalForm (LinearForm 1 (-1) 1) 1] [Constraint (LinearForm 0 (-2) (1+2*mu)) NonStrict]
	| otherwise = optRes {optimalValue = negate $ optimalValue optRes} where
	optRes = optimize [RationalForm (LinearForm 1 (-1) 0) 1] [Constraint (LinearForm 1 0 (-mu)) NonStrict, Constraint (LinearForm (-1) 1 (-1%2)) NonStrict]

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

reverseMOnS m = reverseMOnS' from to where
	from = 1 % 2
	to   = 1 % 1
	reverseMOnS' a b
		| b-a < 1%1000000 = a
		| optimalValue (mOnS ((a+b)/2)) > m = reverseMOnS' a ((a+b)/2)
		| otherwise = reverseMOnS' ((a+b)/2) b

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

-- % \begin{lemma}\label{l:pointwise-moments-on-1/2}
-- % For $A\ge12$ let
-- % $$
-- % f(A) = 1+\inf \{ l/k \mid (4-A)k+4l+2 \ge 0 \}.
-- % $$
-- % Then
-- % $$
-- % R \ll T V^{-6} \log^8 T + T^{f(A)+\eps} V^{-A}
-- %   \ll T^{\max \{ 1+32(A-6)/205, f(A) \}} V^{-A}
-- % $$
-- % and thus
-- % $$
-- % M(A) \le \max \{ 1+32(A-6)/205, f(A) \}.
-- % $$
-- % \end{lemma}

-- Constant
-- is produced by
-- optimize [RationalForm (LinearForm 4 4 2) (LinearForm 1 0 0)] [Constraint (LinearForm (-64) (-77) 64) Strict]

mBigOnHalf a
	| a < 4     = simulateOptimize 1
	| a < 12    = simulateOptimize $ 1+(a-4)/8
	| a > 41614060315296730740083860226662 % 2636743270445733804969041895717 = simulateOptimize $ 1 + 32*(a-6)/205
	| otherwise = if Finite x >= optimalValue optRes
		then simulateOptimize x
		else optRes where
			optRes = optimize [RationalForm (LinearForm 1 1 0) (LinearForm 1 0 0)]
				[Constraint (LinearForm (4-a) 4 2) NonStrict]
			x = 1 + 32*(a-6)/205

reverseMBigOnHalf m
	| m <= 2 = simulateOptimize $ (m-1)*8 + 4
	| otherwise = if Finite a <= optimalValue optRes
		then simulateOptimize a
		else optRes where
		a = (m-1)*205/32 + 6
		optRes = optimize [RationalForm (LinearForm 4 4 2) (LinearForm 1 0 0)] [Constraint (LinearForm (1-m) 1 0) NonStrict]


f l lambda = (l-lambda)*32/205
	+ ((toRational . optimalValue . mBigOnHalf) (4*l/(4-lambda)) - 1) * (4-lambda) /4

--bestLambda l = minimum $ map (f l) lambdas `zip` lambdas where
--	lambdas = [0,1%100..4-1%100]

heckeZetaByHalf a = 1 - xt where
	d = toRational 12.571624917200547
	ia 2 = 0
	ia 3 = 1%4
	ia a
		| a<=12 = 32%205 * ((1 + 4 / d) * a - 4) + (toRational (optimalValue (mBigOnHalf d)) - 1) * a / d
		| a<=15 = 32%205 * a + toRational (optimalValue (mBigOnHalf a)) - 1
		| otherwise = 32%205 * (2 * a - 6)
	xt = 1%2 / (1 + ia a)


bestLambda l = (\x -> (x, fromRational $ f l x)) (bestLambda' 0 (3999%1000)) where
	bestLambda' a b
		| b-a < 1%1000000000 = a
		| otherwise = if mx1 > mx2 then bestLambda' x1 b else bestLambda' a x2 where
			x1 = (2*a+b)/3
			x2 = (a+2*b)/3
			ma = f l a
			mb = f l b
			mx1 = f l x1
			mx2 = f l x2

difur a = a * m' - m + (77%205) where
	h = 1%(10^100)
	m = toRational $ optimalValue $ mBigOnHalf a
	mh = toRational $ optimalValue $ mBigOnHalf (a+h)
	m' = (mh-m) / h

solveD a b
	| b-a < 1%(10^6) = a
	| otherwise = if difur c > 0 then solveD a c else solveD c b where
		c = (a+b)/2

{-
D = 12.571624917200547
-}

