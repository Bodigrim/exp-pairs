module ExpPairs.Optimize (optimizeWithConstraints, LinearForm (..), RationalForm (..), IneqType (..), Constraint (..), InitPair(..), Path) where

import Data.Ratio
import Data.Ord
import Data.List
import Data.Monoid

import ExpPairs.LinearForm
import ExpPairs.Process
import ExpPairs.Pair
import ExpPairs.RatioInf

fracs2proj :: (Rational, Rational) -> (Integer, Integer, Integer)
fracs2proj (q, r) = (k, l, m) where
	dq = denominator q
	dr = denominator r
	m = lcm dq dr
	k = numerator q * (m `div` dq)
	l = numerator r * (m `div` dr)

proj2fracs :: (Integer, Integer, Integer) -> (Rational, Rational)
proj2fracs (k, l, m) = (k%m, l%m)


evalFunctional :: [RationalForm Rational] -> [Constraint Rational] -> Path -> (RationalInf, InitPair)
evalFunctional rfs cons path = if rs==[] then (InfPlus, undefined) else minimumBy (comparing fst) rs where
	initPairs = [(minBound::InitPair)..maxBound]
	ps = map (evalPath path . fracs2proj . initPairToValue) initPairs `zip` initPairs
	qs = filter (\(p,_) -> all (checkConstraint p) cons) ps
	rs = map (\(p, ip) -> (maximum $ map (evalRF p) rfs, ip)) qs

checkMConstraints :: Path -> [Constraint Rational] -> Bool
checkMConstraints path cons = all (\con -> any (\p -> checkConstraint (evalPath path p) con ) triangleT) cons where
	triangleT = map fracs2proj [ (0%1,1%1), (0%1,1%2), (1%2,1%2)]

simulateOptimizeWithConstraints :: Rational -> (Double, Rational, InitPair, Path)
simulateOptimizeWithConstraints r = (d, r, Usual, mempty) where
	d = fromRational r

optimizeWithConstraints :: [RationalForm Rational] -> [Constraint Rational] -> (Double, Rational, InitPair, Path)
optimizeWithConstraints rfs cons = (d, r, ip, path) where
	(r', ip, path) = optimizeWithConstraints' rfs cons (InfPlus, undefined, mempty)
	r = toRational r'
	d = fromRational r

optimizeWithConstraints' :: [RationalForm Rational] -> [Constraint Rational] -> (RationalInf, InitPair, Path) -> (RationalInf, InitPair, Path)
optimizeWithConstraints' rfs cons (r, ip, path)
	| lengthPath path > 100 = (r, ip, path)
	| otherwise = (r2, ip2, path2) where
		(r0, ip0) = if r0' < r then (r0', ip0') else (r, ip) where
			(r0', ip0') = evalFunctional rfs cons path

		cons0 = if r0==InfPlus
			then cons
			else cons ++ map (\(RationalForm num den) -> Constraint (consBuilder num den) Strict) rfs
		consBuilder num den = substituteLF (num, den, 1) (LinearForm (-1) (toRational r0) 0)

		(r1, ip1, path1) = if checkMConstraints patha cons0 && r1' < r0 then (r1', ip1', path1') else (r0, ip0, path) where
			patha  = path `mappend` aPath
			(r1', ip1', path1') = optimizeWithConstraints' rfs cons (r0, ip0, patha)

		(r2, ip2, path2) = if checkMConstraints pathba cons0 && r2' < r1 then (r2', ip2', path2') else (r1, ip1, path1) where
			pathba  = path `mappend` baPath
			(r2', ip2', path2') = optimizeWithConstraints' rfs cons (r1, ip1, pathba)
