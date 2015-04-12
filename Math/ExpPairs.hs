{-|
Module      : Math.ExpPairs
Description : Linear programming over exponent pairs
Copyright   : (c) Andrew Lelechenko, 2014-2015
License     : GPL-3
Maintainer  : andrew.lelechenko@gmail.com
Stability   : experimental
Portability : POSIX

Package implements an algorithm to minimize the maximum of a list of rational objective functions over the set of exponent pairs. See full description in
A. V. Lelechenko, Linear programming over exponent pairs. Acta Univ. Sapientiae, Inform. 5, No. 2, 271-287 (2013).
<http://www.acta.sapientia.ro/acta-info/C5-2/info52-7.pdf>

A set of useful applications can be found in
"Math.ExpPairs.Ivic", "Math.ExpPairs.Kratzel" and "Math.ExpPairs.MenzerNowak".
-}
module Math.ExpPairs
	( optimize
	, OptimizeResult
	, optimalValue
	, optimalPair
	, optimalPath
	, simulateOptimize
	, simulateOptimize'
	, LinearForm (..)
	, RationalForm (..)
	, IneqType (..)
	, Constraint (..)
	, InitPair
	, Path
	, RatioInf (..)
	, RationalInf
	) where

import Data.Ratio  ((%), numerator, denominator)
import Data.Ord    (comparing)
import Data.List   (minimumBy)
import Data.Monoid (mempty, mappend)

import Math.ExpPairs.LinearForm
import Math.ExpPairs.Process
import Math.ExpPairs.Pair
import Math.ExpPairs.RatioInf

fracs2proj :: (Rational, Rational) -> (Integer, Integer, Integer)
fracs2proj (q, r) = (k, l, m) where
	dq = denominator q
	dr = denominator r
	m = lcm dq dr
	k = numerator q * (m `div` dq)
	l = numerator r * (m `div` dr)

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

-- |Container for the result of optimization.
data OptimizeResult = OptimizeResult {
	-- | The minimal value of objective function.
	optimalValue :: RationalInf,
	-- | The initial exponent pair, on which minimal value was achieved.
	optimalPair  :: InitPair,
	-- | The sequence of processes, after which minimal value was
	-- achieved.
	optimalPath  :: Path
	}

instance Show OptimizeResult where
	show (OptimizeResult r' ip p) = show' r' ++ "\n" ++ show ip ++ "\t" ++ show p where
		show' (Finite r) = show (fromRational r :: Double) ++ " = " ++ show r
		show' r = show r

instance Eq OptimizeResult where
	a==b = optimalValue a == optimalValue b

instance Ord OptimizeResult where
	compare a b = compare (optimalValue a) (optimalValue b)

-- |Wrap 'Rational' into 'OptimizeResult'.
simulateOptimize :: Rational -> OptimizeResult
simulateOptimize r = OptimizeResult (Finite r) Corput01 mempty

-- |Wrap 'RationalInf' into 'OptimizeResult'.
simulateOptimize' :: RationalInf -> OptimizeResult
simulateOptimize' r = OptimizeResult r Corput01 mempty

-- |This function takes a list of rational forms and a list
-- of constraints and returns an exponent pair, which satisfies
-- all constraints and minimizes the maximum of all rational forms.
optimize :: [RationalForm Rational] -> [Constraint Rational] -> OptimizeResult
optimize rfs cons = optimize' rfs cons (OptimizeResult r0 ip0 mempty) where
	(r0, ip0) = evalFunctional [Corput01, Corput12] [Corput01, Corput12] rfs cons mempty

optimize' :: [RationalForm Rational] -> [Constraint Rational] -> OptimizeResult -> OptimizeResult
optimize' rfs cons ret@(OptimizeResult r _ path)
	| lengthPath path > 100 = ret
	| otherwise = retBA where
		ret0@(OptimizeResult r0 ip0 _) = if r0' < r then OptimizeResult r0' ip0' path else ret where
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

