{-|
Module      : Math.ExpPairs.Kratzel
Description : Asymmetric divisor problem
Copyright   : (c) Andrew Lelechenko, 2014-2015
License     : GPL-3
Maintainer  : andrew.lelechenko@gmail.com
Stability   : experimental
Portability : POSIX

Let τ_{a, b}(n) denote the number of integer
(v, w) with v^a w^b = n.

Let τ_{a, b, c}(n) denote the number of integer
(v, w, z) with v^a w^b z^c = n.

Krätzel
	(/Krätzel E./
	`Lattice points'.
	Dordrecht: Kluwer, 1988)
proved asymptotic formulas for
Σ_{n ≤ x} τ_{a, b}(n) with an error term of order x^(Θ(a, b) + ε)
and for
Σ_{n ≤ x} τ_{a, b, c}(n) with an error term of order x^(Θ(a, b, c) + ε).
He also provided a set of theorems to estimate Θ(a, b) and Θ(a, b, c).

-}
module Math.ExpPairs.Kratzel
	( TauabTheorem (..)
	, tauab
	, TauabcTheorem (..)
	, tauabc
	) where

import Control.Arrow
import Data.Ratio ((%))
import Data.Ord   (comparing)
import Data.List  (minimumBy)

import Math.ExpPairs

-- |Special type to specify the theorem of Krätzel1988,
-- which provided the best estimate of Θ(a, b)
data TauabTheorem
	-- | Theorem 5.11, case a)
	= Kr511a
	-- | Theorem 5.11, case b)
	| Kr511b
	-- | Theorem 5.12, case a)
	| Kr512a
	-- | Theorem 5.12, case b)
	| Kr512b
	deriving (Eq, Ord, Enum, Bounded, Show)

divideResult :: Real a => a -> (b, OptimizeResult) -> (b, OptimizeResult)
divideResult d = second (\o -> o {optimalValue = optimalValue o / Finite (toRational d)})

-- |Compute Θ(a, b) for given a and b.
tauab :: Integer -> Integer -> (TauabTheorem, OptimizeResult)
tauab a' b'
	| d /= 1 = divideResult d $ tauab (a'`div` d) (b' `div` d) where
			d = gcd a' b'
tauab a' b' = minimumBy (comparing (optimalValue . snd)) [kr511a, kr511b, kr512a, kr512b] where
	a = toRational a'
	b = toRational b'
	kr511a = (Kr511a, optimize
		[RationalForm (LinearForm 2 2 (-1)) (LinearForm 0 0 (a+b))]
		[Constraint (LinearForm (-2*b) (2*a) (-a)) NonStrict])
	kr511b = (Kr511b, optimize
		[RationalForm (LinearForm 1 0 0) (LinearForm b (-a) a)]
		[Constraint (LinearForm (2*b) (-2*a) a) Strict])
	kr512a = (Kr512a, simulateOptimize r) where
		r = if 11*a >= 8*b then 19/29/(a+b) else 1%1
	kr512b = if 11*a >= 8*b then kr512a else (Kr512b, optimize
		[
			RationalForm (LinearForm (-11) 8 (-4)) (LinearForm (-29*b) (29*a) (4*b-20*a))
		]
		[
			Constraint (LinearForm (-2*b) (2*a) (-a)) NonStrict,
			Constraint (LinearForm (-29) 0 4) Strict,
			Constraint (LinearForm 29 29 (-24)) Strict
		])

-- |Special type to specify the theorem of Krätzel1988,
-- which provided the best estimate of Θ(a, b, c)
data TauabcTheorem
	-- | Kolesnik
	-- (/Kolesnik G./ `On the estimation of multiple exponential sums'
	-- \/\/ Recent progress in analytic number theory,
	-- London: Academic Press, 1981, Vol. 1, P. 231–246)
	-- proved that  Θ(1, 1, 1) = 43 \/96.
	= Kolesnik
	-- | Theorem 6.1
	| Kr61
	-- | Theorem 6.2
	| Kr62
	-- | Theorem 6.3
	| Kr63
	-- | Theorem 6.4
	| Kr64
	-- | Theorem 6.5
	| Kr65
	-- | Theorem 6.6
	| Kr66
	-- | In certain cases Θ(a, b, c) = Θ(a, b).
	| Tauab TauabTheorem
	deriving (Eq, Ord, Show)

-- |Compute Θ(a, b, c) for given a, b and c.
tauabc :: Integer -> Integer -> Integer -> (TauabcTheorem, OptimizeResult)
tauabc a' b' c'
	| d /= 1 = divideResult d $ tauabc (a'`div` d) (b' `div` d) (c' `div` d) where
			d = gcd (gcd a' b') c'
tauabc 1 1 1 = (Kolesnik, simulateOptimize $ 43%96)
tauabc a' b' c' = minimumBy (comparing (optimalValue . snd)) [kr61, kr62, kr63, kr64, kr65, kr66] where
	a = toRational a'
	b = toRational b'
	c = toRational c'
	kr61
		| c<a+b = (Kr61, simulateOptimize $ 2/(a+b+c))
		| optimalValue optRes < Finite (recip c) = (Kr61, simulateOptimize $ 1/c)
		| otherwise = (Tauab th, optRes)
		where
			(th, optRes) = tauab a' b'
	kr62 = (Kr62, optimize
		[RationalForm (LinearForm 2 2 0) (LinearForm 0 0 (a+b+c))]
		[
			Constraint (LinearForm (-b-c) a 0) NonStrict,
			Constraint (LinearForm (-2*c) (-2*c) (a+b+c)) NonStrict
		])
	kr63 = (Kr63, optimize
		[RationalForm (LinearForm 4 2 3) (LinearForm (2*(a+b+c)) 0 (3*(a+b+c)))]
		[Constraint (LinearForm (2*(a-b-c)) (2*a) (2*a-b-c)) NonStrict])
	kr64 = (Kr64, simulateOptimize r) where
		r = recip (a+b+c) * minimum ((a+b+c):[2-4*(k-1)%(3*2^k-4) | k<-[1..maxk], (3*2^k-2*k-4)%1 * a >= 2 * (b+c), (3*2^k-8)%1 * (a+b) >= (3*2^k-4*k+4)%1 * c])
		maxk = 4 `max` floor (logBase 2 (fromRational $ b+c) :: Double)
	kr65 = (Kr65, simulateOptimize r) where
		r = if 7*a>=2*(b+c) && 4*(a+b)>=5*c then 3%2/(a+b+c) else 1%1
	kr66 = (Kr66, simulateOptimize r) where
		r = if 18*a>=7*(b+c) && 2*(a+b)>=3*c then 25%17/(a+b+c) else 1%1
