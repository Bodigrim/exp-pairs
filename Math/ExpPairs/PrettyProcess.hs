{-# LANGUAGE TemplateHaskell #-}
module Math.ExpPairs.PrettyProcess
	( prettifyM,
		PrettyProcess) where

import Data.List                (minimumBy, unfoldr)
import Data.Ord                 (comparing)
import Data.Function.Memoize    (memoize, deriveMemoizable)

import Math.ExpPairs.ProcessMatrix

data PrettyProcess = Simply [Process] | Repeat PrettyProcess Int | Sequence PrettyProcess PrettyProcess

deriveMemoizable ''PrettyProcess

data PrettyProcessWithLength = PPWL Int PrettyProcess

instance Show PrettyProcess where
	show (Simply xs) = concatMap show xs
	show (Repeat _  0) = ""
	show (Repeat xs 1) = show xs
	show (Repeat xs n) = case show xs of
		[ch] -> ch : '^' : show n
		chs  -> '(' : chs ++ (')' : '^' : show n)
	show (Sequence a b) = show a ++ show b

bracketWidth :: Int
bracketWidth = 4
subscriptWidth :: Int
subscriptWidth = 4
printedLength' :: Process -> Int
printedLength' A = 10
printedLength' BA = 20

printedLength :: PrettyProcess -> Int
printedLength (Simply xs) = sum (map printedLength' xs)
printedLength (Repeat _ 0) = 0
printedLength (Repeat xs 1) = printedLengthM xs
printedLength (Repeat (Simply [A]) n) = printedLength' A + subscriptWidth
printedLength (Repeat xs n) = printedLength xs + bracketWidth * 2 + subscriptWidth
printedLength (Sequence a b) = printedLengthM a + printedLengthM b

printedLengthM = memoize printedLength

-- | Returns non-trivial divisors of an argument.
divisors :: Int -> [Int]
divisors n = ds1 ++ reverse ds2 where
	(ds1, ds2) = unzip [ (a, n `div` a) | a <- [1 .. sqrtint n], n `mod` a == 0 ]
	sqrtint = round . sqrt . fromIntegral

asRepeat :: [Process] -> ([Process], Int)
asRepeat [] = ([], 0)
asRepeat xs = pair where
	l = length xs
	candidates = [ (take d xs, l `div` d) | d <- divisors l ]
	pair = head $ filter (\(ys, n) -> concat (replicate n ys) == xs) candidates

prettify :: [Process] -> PrettyProcess
prettify []   = Simply []
prettify [A]  = Simply [A]
prettify [BA] = Simply [BA]
prettify xs = minimumBy (comparing printedLengthM) yss where
	xs'' = case asRepeat xs of
		(_, 1)   -> Simply xs
		(xs', n) -> Repeat (prettifyM xs') n
	yss = xs'' : map f bcs

	bcs :: [([Process], [Process])]
	bcs = takeWhile (not . null . snd) $ iterate bcf ([head xs], tail xs)
	bcf (xs, y:ys) = (xs++[y], ys)

	f (bs, cs) = Sequence (prettifyM bs) (prettifyM cs)

prettifyM = memoize prettify
