module Math.ExpPairs.MenzerNowak where

import Data.Ratio

import Math.ExpPairs

menzerNowak :: Integer -> Integer -> OptimizeResult
menzerNowak a' b' = optimize
	[
		RationalForm (LinearForm 1 1 0) (LinearForm (a+b) 0 (a+b)),
		RationalForm (LinearForm 1 0 0) (LinearForm (a+b) (-a) a)
	]
	[] where
		a = a'%1
		b = b'%1
