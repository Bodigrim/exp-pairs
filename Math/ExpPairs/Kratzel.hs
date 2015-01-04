module Math.ExpPairs.Kratzel where

import Data.Ratio
import Data.Ord
import Data.List

import Math.ExpPairs

data TauabTheorem = Kr511a | Kr511b | Kr512a | Kr512b
	deriving (Show)

tauab :: Integer -> Integer -> (TauabTheorem, OptimizeResult)
tauab a' b' = minimumBy (comparing (optimalValue . snd)) [kr511a, kr511b, kr512a, kr512b] where
	a = a'%1
	b = b'%1
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

data TauabcTheorem = Kolesnik | Kr61 | Kr62 | Kr63 | Kr64 | Kr65 | Kr66 | Tauab TauabTheorem
	deriving (Show)

tauabc :: Integer -> Integer -> Integer -> (TauabcTheorem, OptimizeResult)
tauabc 1 1 1 = (Kolesnik, simulateOptimize $ 43%96)
tauabc a' b' c' = minimumBy (comparing (optimalValue . snd)) [kr61, kr62, kr63, kr64, kr65, kr66] where
	a = a'%1
	b = b'%1
	c = c'%1
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
		maxk = 4 `max` floor (logBase 2 (fromRational $ b+c))
	kr65 = (Kr65, simulateOptimize r) where
		r = if 7*a>=2*(b+c) && 4*(a+b)>=5*c then 3%2/(a+b+c) else 1%1
	kr66 = (Kr66, simulateOptimize r) where
		r = if 18*a>=7*(b+c) && 2*(a+b)>=3*c then 25%17/(a+b+c) else 1%1
