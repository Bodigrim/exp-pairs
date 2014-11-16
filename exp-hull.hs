import Data.Ratio
import Data.Ord
import Data.List
import Data.Monoid

import ExpPairs.Optimize
import ExpPairs.Kratzel


zetaOnS s = optimizeWithConstraints
	[RationalForm (LinearForm 1 1 (-s)) 2]
	[Constraint (RationalForm (LinearForm (-1) 1 (-s)) 1) NonStrict]

menzerNowak a' b' = optimizeWithConstraints
	[
		RationalForm (LinearForm 1 1 0) (LinearForm (a+b) 0 (a+b)),
		RationalForm (LinearForm 1 0 0) (LinearForm (a+b) (-a) a)
	]
	[] where
		a = a'%1
		b = b'%1

-- MenzerNowak(a,b) = [ [k+l, (k+m)*(a+b)] , [k, k*b+a*(m+k-l)] ]
