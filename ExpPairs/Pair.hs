module ExpPairs.Pair (InitPair' (..), InitPair, initPairs, initPairToValue) where

import Data.Ratio

data Triangle = Corput16 | HuxW87b1 | Hux05
	deriving (Show, Bounded, Enum, Eq, Ord)

data InitPair' t = Corput01 | Corput12 | Mix t t
	deriving (Eq)
type InitPair = InitPair' Rational

instance (Show t, Num t, Eq t) => Show (InitPair' t) where
	show Corput01 = "(0, 1)"
	show Corput12 = "(1/2, 1/2)"
	show (Mix r1 r2) =
		s1 ++ (if s1/="" && (s2/=""||s3/="") then " + " else "")
		++ s2 ++ (if s2/="" && s3/="" then " + " else "") ++ s3
		where
			r3 = 1 - r1 - r2
			f r t = if r==0 then "" else (if r==1 then "" else show r ++ " * ") ++ show t
			s1 = f r1 Corput16
			s2 = f r2 HuxW87b1
			s3 = f r3 Hux05


sect = 30

initPairs = Corput01 : Corput12 : [Mix (r1%sect) (r2%sect) | r1<-[0..sect], r2<-[0..sect-r1]]

initPairToValue :: InitPair -> (Rational, Rational)
initPairToValue Corput01 = (0, 1)
initPairToValue Corput12 = (1%2, 1%2)
initPairToValue (Mix r1 r2) = (x, y) where
	r3 = 1 - r1 - r2
	(x1, y1) = (1%6, 2%3)
	(x2, y2) = ( 2 %  13,  35 %  52)
	(x3, y3) = (32 % 205, 269 % 410)
	x = x1*r1 + x2*r2 + x3*r3
	y = y1*r1 + y2*r2 + y3*r3

