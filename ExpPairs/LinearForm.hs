module ExpPairs.LinearForm where

import Data.List
import Data.Ratio
import Data.Monoid

data LinearForm t = LinearForm t t t
	deriving (Eq)

instance (Num t, Eq t, Show t) => Show (LinearForm t) where
	show (LinearForm a b c) = if (a==0) && (b==0) && (c==0)
		then "0"
		else "(" ++ (intercalate " + " $ filter (/=[]) $
			[if a/= 0 then show a ++ "k" else []] ++
			[if b/= 0 then show b ++ "l" else []] ++
			[if c/= 0 then show c        else []] ) ++ ")" -- where
			-- show' :: Rational -> String
			-- show' z = if denominator z==1 then show (numerator z) else show z

instance Num t => Num (LinearForm t) where
	(LinearForm a b c) + (LinearForm d e f) = LinearForm (a+d) (b+e) (c+f)
	(*) = undefined
	negate (LinearForm a b c) = LinearForm (negate a) (negate b) (negate c)
	abs = undefined
	signum = undefined
	fromInteger n = LinearForm 0 0 (fromInteger n)

instance Num t => Monoid (LinearForm t) where
	mempty = 0
	mappend = (+)

scaleLF :: (Num t, Eq t) => t -> LinearForm t -> LinearForm t
scaleLF 0 (LinearForm a b c) = LinearForm 0 0 0
scaleLF s (LinearForm a b c) = LinearForm (a*s) (b*s) (c*s)

evalLF (k, l, m) (LinearForm a b c) = a*k+l*b+m*c

substituteLF (k, l, m) (LinearForm a b c) = (scaleLF a k) + (scaleLF b l) + (scaleLF c m)

data RationalForm t = RationalForm (LinearForm t) (LinearForm t)
	deriving (Show)

evalRF :: (Real t, Num t) => (Integer, Integer, Integer) -> RationalForm t -> Rational
evalRF (k', l', m') (RationalForm num den) =
	(toRational $ evalLF (k, l, m) num)
		/ (toRational $ evalLF (k, l, m) den) where
			k = fromInteger k'
			l = fromInteger l'
			m = fromInteger m'

substituteRF (k, l, m) (RationalForm num den) = RationalForm (substituteLF (k, l, m) num) (substituteLF (k, l, m) den)

data IneqType = Strict | NonStrict
	deriving (Eq, Show)

data Constraint t = Constraint (RationalForm t) IneqType
	deriving (Show)

checkConstraint :: (Num t, Eq t) => (Integer, Integer, Integer) -> Constraint t -> Bool
checkConstraint (k', l', m') (Constraint (RationalForm num den) ineq)
	= if ineq==Strict
		then numer/=0 && signum numer == signum denom
		else signum numer==0 || signum numer == signum denom where
			k = fromInteger k'
			l = fromInteger l'
			m = fromInteger m'
			numer = evalLF (k, l, m) num
			denom = evalLF (k, l, m) den
