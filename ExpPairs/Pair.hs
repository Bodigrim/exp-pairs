module ExpPairs.Pair (InitPair (..), initPairToValue) where

import Data.Ratio

data InitPair = Usual | HuxW87b1 | Hux05
	deriving (Show, Bounded, Enum, Eq, Ord)

initPairToValue :: InitPair -> (Rational, Rational)
initPairToValue Usual    = (0, 1)
initPairToValue HuxW87b1 = ( 2 %  13,  35 %  52)
initPairToValue Hux05    = (32 % 205, 269 % 410)

