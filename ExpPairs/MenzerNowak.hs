module ExpPairs.MenzerNowak where

import Data.Ratio

import ExpPairs.Optimize

menzerNowak :: Integer -> Integer -> (Double, Rational, InitPair, Path)
menzerNowak a' b' = optimizeWithConstraints
	[
		RationalForm (LinearForm 1 1 0) (LinearForm (a+b) 0 (a+b)),
		RationalForm (LinearForm 1 0 0) (LinearForm (a+b) (-a) a)
	]
	[] where
		a = a'%1
		b = b'%1
