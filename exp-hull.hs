import Data.Ratio
import Data.Ord
import Data.List
import Data.Monoid

import ExpPairs.LinearForm
import ExpPairs.Process
import ExpPairs.Pair
import ExpPairs.Graham
import qualified ExpPairs.Matrix3 as Mx

-- t == Integer
-- t == Rational
evalFunctional :: (Real t1, Real t2) => [RationalForm t1] -> [Constraint t2] -> Path -> (Rational, InitPair)
evalFunctional rfs cons path = minimumBy (comparing fst) rs where
	initPairs = [(minBound::InitPair)..maxBound]
	ps = map (fracs2proj . evalPath path) initPairs `zip` initPairs
	qs = filter (\(p,_) -> all (checkConstraint p) cons) ps
	rs = map (\(p, ip) -> (maximum $ map (evalRF p) rfs, ip)) qs

checkMConstraints :: (Eq t, Num t) => Path -> [Constraint t] -> Bool
checkMConstraints (Path m _) cons = all (\con -> any (\p -> checkConstraint (f p) con ) triangleT) cons where
	triangleT = map fracs2proj [ (0%1,1%1), (0%1,1%2), (1%2,1%2)]
	f (a,b,c) = (a',b',c') where
		m' = fmap fromInteger m
		(Mx.Vector3 a' b' c') = Mx.multCol m' (Mx.Vector3 a b c)

optimizeWithConstraints :: [RationalForm Rational] -> [Constraint Rational] -> (Rational, InitPair, Path)
optimizeWithConstraints rfs cons = optimizeWithConstraints' rfs cons (10^10, Usual, mempty)

optimizeWithConstraints' :: [RationalForm Rational] -> [Constraint Rational] -> (Rational, InitPair, Path) -> (Rational, InitPair, Path)
optimizeWithConstraints' rfs cons (r, ip, path@(Path m _))
	| Mx.maximum m > 10^100 = (r, ip, path)
	| otherwise = undefined where
		(r0', ip0') = evalFunctional rfs cons path
		(r0, ip0) = if r0' < r then  (r0', ip0') else (r, ip)
		patha  = path `mappend` aPath
		pathba = path `mappend` baPath
		cons1 = cons ++ map (\(RationalForm num den) -> Constraint (consBuilder num den) Strict) rfs
		consBuilder num den = RationalForm (substituteLF (num, den, undefined) (LinearForm (-1) r0 0)) 1



--optimizeWithConstrains(Thetas, cons=[], r=1000000, M=matid(3), path="") = {
--	limit = limitOWC;
--	bestpi = 0;
--	bestpath = path;

--	if(vecmax(M)>limit,
--		return([r, path, bestpi]));

--	temp = evalFunctional(M, Thetas, cons);
--	if(temp[1] < r,
--		r = temp[1];
--		bestpi=temp[2]
--		);

--	Ma  = normalize(M * a);
--	Mba = normalize(M * ba);

--	cons1 = cons;
--	for(k=1, #Thetas,
--		cons1 = vec_append(cons1, [r*Thetas[k][2] - Thetas[k][1], STRICT] )
--		);

--	if(checkMConstrains(Ma, cons1),
--		patha = concat(path, "a*");
--		[temp, patha, temp2] = optimizeWithConstrains(Thetas, cons, r, Ma, patha);
--		if(temp < r, r = temp; bestpath = patha; bestpi=temp2);
--		,
--		);

--	if(checkMConstrains(Mba, cons1),
--		pathba = concat(path, "ba*");
--		[temp, pathba, temp2] = optimizeWithConstrains(Thetas, cons, r, Mba, pathba);
--		if(temp < r, r = temp; estpath = pathba; bestpi=temp2);
--		,
--		);

--	[r, bestpath, bestpi];

--	};
