module ExpPairs.Optimize (optimize, LinearForm (..), RationalForm (..), IneqType (..), Constraint (..), InitPair(..), Path, simulateOptimize,simulateOptimize', RatioInf (..), RationalInf, OptimizeResult, optimalValue, optimalPair, optimalPath) where

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


evalFunctional :: [InitPair] -> [InitPair] -> [RationalForm Rational] -> [Constraint Rational] -> Path -> (RationalInf, InitPair)
evalFunctional corners interiors rfs cons path = if null rs then (InfPlus, undefined) else minimumBy (comparing fst) rs where
	applyPath ips = map (evalPath path . fracs2proj . initPairToValue) ips `zip` ips
	corners'   = applyPath corners
	interiors' = applyPath interiors

	predicate (p, _) = all (checkConstraint p) cons
	qs = if all predicate corners' then corners' else filter predicate interiors'

	rs = map (\(p, ip) -> (maximum $ map (evalRF p) rfs, ip)) qs

checkMConstraints :: Path -> [Constraint Rational] -> Bool
checkMConstraints path = all (\con -> any (\p -> checkConstraint (evalPath path p) con ) triangleT) where
	triangleT = map fracs2proj [ (0%1,1%1), (0%1,1%2), (1%2,1%2)]

data OptimizeResult = OptimizeResult {
	optimalValue :: RationalInf,
	optimalPair  :: InitPair,
	optimalPath  :: Path
	}

instance Show OptimizeResult where
	show (OptimizeResult r ip p) = show' r ++ "\n" ++ show ip ++ "\t" ++ show p where
		show' (Finite r) = show (fromRational r ) ++ " = " ++ show r
		show' r = show r

simulateOptimize :: Rational -> OptimizeResult
simulateOptimize r = OptimizeResult (Finite r) Corput01 mempty

simulateOptimize' :: RationalInf -> OptimizeResult
simulateOptimize' r = OptimizeResult r Corput01 mempty

optimize :: [RationalForm Rational] -> [Constraint Rational] -> OptimizeResult
optimize rfs cons = optimize' rfs cons (OptimizeResult r0 ip0 mempty) where
	(r0, ip0) = evalFunctional [Corput01, Corput12] [Corput01, Corput12] rfs cons mempty

optimize' :: [RationalForm Rational] -> [Constraint Rational] -> OptimizeResult -> OptimizeResult
optimize' rfs cons ret@(OptimizeResult r ip path)
	| lengthPath path > 100 = ret
	| otherwise = retBA where
		ret0@(OptimizeResult r0 ip0 _) = if r0' < r then (OptimizeResult r0' ip0' path) else ret where
			(r0', ip0') = evalFunctional corners interiors rfs cons path
			corners = [Mix 1 0, Mix 0 1, Mix 0 0]
			interiors = initPairs

		cons0 = if r0==InfPlus then cons else cons ++ map (consBuilder r0) rfs

		retA@(OptimizeResult r1 ip1 _) = if checkMConstraints patha cons0 && r1' < r0 then branchA else ret0 where
			patha  = path `mappend` aPath
			branchA@(OptimizeResult r1' _ _) = optimize' rfs cons (OptimizeResult r0 ip0 patha)

		cons1 = if r1==r0	then cons0 else cons ++ map (consBuilder r1) rfs

		retBA = if checkMConstraints pathba cons1 && r2' < r1 then branchB else retA where
			pathba  = path `mappend` baPath
			branchB@(OptimizeResult r2' _ _) = optimize' rfs cons (OptimizeResult r1 ip1 pathba)

		consBuilder rr (RationalForm num den) = Constraint (substituteLF (num, den, 1) (LinearForm (-1) (toRational rr) 0)) Strict

