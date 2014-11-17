module ExpPairs.RatioInf (RatioInf (..), RationalInf) where

import Data.Ratio

data RatioInf t = InfMinus | Finite (Ratio t) | InfPlus
	deriving (Ord, Eq)

type RationalInf = RatioInf Integer

instance (Integral t, Show t) => Show (RatioInf t) where
	show InfMinus = "-Inf"
	show (Finite x) = show x
	show InfPlus = "+Inf"

instance (Integral t) => Num (RatioInf t) where
	InfMinus + InfPlus = undefined
	InfPlus + InfMinus = undefined
	InfMinus + _ = InfMinus
	InfPlus + _ = InfPlus
	_ + InfMinus = InfMinus
	_ + InfPlus = InfPlus
	(Finite a) + (Finite b) = Finite (a+b)

	fromInteger n = Finite (fromInteger n)

	signum InfMinus = Finite (-1)
	signum InfPlus  = Finite 1
	signum (Finite r) = Finite (signum r)

	abs InfMinus = InfPlus
	abs InfPlus = InfPlus
	abs (Finite r) = Finite (abs r)

	negate InfMinus = InfPlus
	negate InfPlus = InfMinus
	negate (Finite r) = Finite (negate r)

	InfMinus * a
		| signum a == Finite 0 = undefined
		| signum a == Finite 1 = InfMinus
		| signum a == Finite (-1) = InfPlus
	InfPlus  * a
		| signum a == Finite 0 = undefined
		| signum a == Finite 1 = InfPlus
		| signum a == Finite (-1) = InfMinus
	a * InfMinus
		| signum a == Finite 0 = undefined
		| signum a == Finite 1 = InfMinus
		| signum a == Finite (-1) = InfPlus
	a * InfPlus
		| signum a == Finite 0 = undefined
		| signum a == Finite 1 = InfPlus
		| signum a == Finite (-1) = InfMinus
	(Finite a) * (Finite b) = Finite (a*b)

instance (Integral t) => Fractional (RatioInf t) where
	fromRational = Finite . fromRational

	InfMinus / InfMinus = undefined
	InfMinus / InfPlus = undefined
	InfPlus / InfMinus = undefined
	InfPlus / InfPlus = undefined

	InfMinus / (Finite a)
		| signum a == 0 = undefined
		| signum a == 1 = InfMinus
		| signum a == (-1) = InfPlus
	InfPlus / (Finite a)
		| signum a == 0 = undefined
		| signum a == 1 = InfPlus
		| signum a == (-1) = InfMinus

	(Finite a) / InfPlus = Finite 0
	(Finite a) / InfMinus = Finite 0

	(Finite a) / (Finite 0) = undefined
	(Finite a) / (Finite b) = Finite (a/b)

instance (Integral t) => Real (RatioInf t) where
	toRational (Finite r) = toRational r
	toRational InfPlus = undefined
	toRational InfMinus = undefined
