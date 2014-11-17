module ExpPairs.Kratzel where

import Data.Ratio
import Data.Ord
import Data.List

import ExpPairs.Optimize

data TauabTheorems = Kr511a | Kr511b | Kr512a | Kr512b
	deriving (Show)

tauab :: Integer -> Integer -> (TauabTheorems, (Double, Rational, InitPair, Path))
tauab a' b' = minimumBy (comparing (\(_, (_, r, _, _)) -> r)) [kr511a, kr511b, kr512a, kr512b] where
	a = a'%1
	b = b'%1
	kr511a = (Kr511a, optimizeWithConstraints
		[RationalForm (LinearForm 2 2 (-1)) (LinearForm 0 0 (a+b))]
		[Constraint (LinearForm (-2*b) (2*a) (-a)) NonStrict])
	kr511b = (Kr511b, optimizeWithConstraints
		[RationalForm (LinearForm 1 0 0) (LinearForm b (-a) a)]
		[Constraint (LinearForm (2*b) (-2*a) a) Strict])
	kr512a = (Kr512a, simulateOptimizeWithConstraints r) where
		r = if 11*a >= 8*b then 19/29/(a+b) else 1%1
	kr512b = if 11*a >= 8*b then kr512a else (Kr512b, optimizeWithConstraints
		[
			RationalForm (LinearForm (-11) 8 (-4)) (LinearForm (-29*b) (29*a) (4*b-20*a))
		]
		[
			Constraint (LinearForm (-2*b) (2*a) (-a)) NonStrict,
			Constraint (LinearForm (-29) 0 4) Strict,
			Constraint (LinearForm 29 29 (-24)) Strict
		])

data TauabcTheorems = Kr62 | Kr63
	deriving (Show)

tauabc :: Integer -> Integer -> Integer -> (TauabcTheorems, (Double, Rational, InitPair, Path))
tauabc a' b' c' = minimumBy (comparing (\(_, (_, r, _, _)) -> r)) [kr62, kr63] where
	a = a'%1
	b = b'%1
	c = c'%1
	kr62 = (Kr62, optimizeWithConstraints
		[RationalForm (LinearForm 2 2 0) (LinearForm 0 0 (a+b+c))]
		[
			Constraint (LinearForm (-b-c) a 0) NonStrict,
			Constraint (LinearForm (-2*c) (-2*c) (a+b+c)) NonStrict
		])
	kr63 = (Kr63, optimizeWithConstraints
		[RationalForm (LinearForm 4 2 3) (LinearForm (2*(a+b+c)) 0 (3*(a+b+c)))]
		[Constraint (LinearForm (2*(a-b-c)) (2*a) (2*a-b-c)) NonStrict])
