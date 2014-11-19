module ExpPairs.Optimize (optimize, LinearForm (..), RationalForm (..), IneqType (..), Constraint (..), InitPair(..), Path, simulateOptimize, RatioInf (..), RationalInf) where

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


-- TODO: do not check all initPairs if all corners are inside constraints
-- TODO: do not recheck usual pairs three times
evalFunctional :: [RationalForm Rational] -> [Constraint Rational] -> Path -> (RationalInf, InitPair)
evalFunctional rfs cons path = if null rs then (InfPlus, undefined) else minimumBy (comparing fst) rs where
	ps = map (evalPath path . fracs2proj . initPairToValue) initPairs `zip` initPairs
	qs = filter (\(p,_) -> all (checkConstraint p) cons) ps
	rs = map (\(p, ip) -> (maximum $ map (evalRF p) rfs, ip)) qs

checkMConstraints :: Path -> [Constraint Rational] -> Bool
checkMConstraints path cons = all (\con -> any (\p -> checkConstraint (evalPath path p) con ) triangleT) cons where
	triangleT = map fracs2proj [ (0%1,1%1), (0%1,1%2), (1%2,1%2)]

simulateOptimize :: Rational -> (Double, Rational, InitPair, Path)
simulateOptimize r = (d, r, Corput01, mempty) where
	d = fromRational r

optimize :: [RationalForm Rational] -> [Constraint Rational] -> (Double, Rational, InitPair, Path)
optimize rfs cons = (d, r, ip, path) where
	(r', ip, path) = optimize' rfs cons (InfPlus, undefined, mempty)
	r = toRational r'
	d = fromRational r

optimize' :: [RationalForm Rational] -> [Constraint Rational] -> (RationalInf, InitPair, Path) -> (RationalInf, InitPair, Path)
optimize' rfs cons ret@(r, ip, path)
	| lengthPath path > 100 = ret
	| otherwise = retBA where
		ret0@(r0, ip0, _) = if r0' < r then (r0', ip0', path) else ret where
			(r0', ip0') = evalFunctional rfs cons path

		cons0 = if r0==InfPlus then cons else cons ++ map (consBuilder r0) rfs

		retA@(r1, ip1, _) = if checkMConstraints patha cons0 && r1' < r0 then branchA else ret0 where
			patha  = path `mappend` aPath
			branchA@(r1', _, _) = optimize' rfs cons (r0, ip0, patha)

		cons1 = if r1==r0	then cons0 else cons ++ map (consBuilder r1) rfs

		retBA = if checkMConstraints pathba cons1 && r2' < r1 then branchB else retA where
			pathba  = path `mappend` baPath
			branchB@(r2', _, _) = optimize' rfs cons (r1, ip1, pathba)

		consBuilder rr (RationalForm num den) = Constraint (substituteLF (num, den, 1) (LinearForm (-1) (toRational rr) 0)) Strict

