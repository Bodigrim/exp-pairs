{-|
Module      : Math.ExpPairs.Process
Description : Processes of van der Corput
Copyright   : (c) Andrew Lelechenko, 2014-2015
License     : GPL-3
Maintainer  : andrew.lelechenko@gmail.com
Stability   : experimental
Portability : TemplateHaskell

Provides types for sequences of /A/- and /B/-processes of van der Corput. A good account on this topic can be found in /Graham S. W.,  Kolesnik G. A./ Van Der Corput's Method of Exponential Sums, Cambridge University Press, 1991, especially Ch. 5.
-}
{-# LANGUAGE TemplateHaskell  #-}
module Math.ExpPairs.Process (Process (..), Path (), aPath, baPath, evalPath, lengthPath) where

import Data.Monoid
import Data.List
import Data.Ord
import Data.Function.Memoize

import qualified Math.ExpPairs.Matrix3 as Mx

-- | Since B^2 = id, B 'Corput16' = 'Corput16', B 'Hux05' = 'Hux05' and B 'HuxW87b1' = ???, the sequence of /A/- and /B/-processes, applied to 'initPairs' can be rewritten as a sequence of 'A' and 'BA'.
data Process
	-- | /A/-process
	= A
	-- | /BA/-process
	| BA
	deriving (Eq, Show, Read, Ord, Enum)

deriveMemoizable ''Process

type ProcessMatrix = Mx.Matrix3 Integer

process2matrix :: Process -> ProcessMatrix
process2matrix  A = Mx.Matrix3 1 0 0 1 1 1  2 0 2
process2matrix BA = Mx.Matrix3 0 1 0 2 0 1  2 0 2

-- | Holds a list of 'Process' and a matrix of projective
-- transformation, which they define. It also provides a fancy 'Show'
-- instance. E. g.,
--
-- > show (mconcat $ replicate 10 aPath) == "A^10"
--
data Path = Path ProcessMatrix [Process]

-- | Path consisting of a single process 'A'.
aPath :: Path
aPath  = Path (process2matrix  A) [ A]

-- | Path consisting of a single process 'BA'.
baPath :: Path
baPath = Path (process2matrix BA) [BA]

instance Monoid Path where
	mempty = Path 1 []
	mappend (Path m1 l1) (Path m2 l2) = Path (Mx.normalize $ m1*m2) (l1++l2)

instance Show Path where
	show (Path m l) = prettyProcesses l -- ++ "\n" ++ Mx.prettyMatrix m

instance Read Path where
	readsPrec _ zs = [reads' zs] where
		reads' ('A':xs) = (aPath `mappend` path, ys) where
			(path, ys) = reads' xs
		reads' ('B':'A':xs) = (baPath `mappend` path, ys) where
			(path, ys) = reads' xs
		reads' ('B':xs) = (baPath, xs)
		reads' xs = (mempty, xs)

instance Eq Path where
	(Path m1 _) == (Path m2 _) = Mx.normalize m1 == Mx.normalize m2

instance Ord Path where
	(Path _ q1) <= (Path _ q2) = cmp q1 q2 where
		cmp (A:p1) (A:p2) = cmp p1 p2
		cmp (BA:p1) (BA:p2) = cmp p2 p1
		cmp (A:_) (BA:_) = True
		cmp (BA:_) (A:_) = False
		cmp [] _ = True
		cmp _ [] = False

-- |Apply a projective transformation, defined by 'Path',
-- to a given point in two-dimensional projective space.
evalPath :: (Num t) => Path -> (t, t, t) -> (t, t, t)
evalPath (Path m _) (a,b,c) = (a',b',c') where
	m' = fmap fromInteger m
	(Mx.Vector3 a' b' c') = Mx.multCol m' (Mx.Vector3 a b c)

-- | Count processes in the 'Path'. Note that 'BA' counts
-- for one process, not two.
lengthPath :: Path -> Int
lengthPath (Path _ xs) = length xs

symbolWidth :: Int
symbolWidth = 10
bracketWidth :: Int
bracketWidth = 4
subscriptWidth :: Int
subscriptWidth = 4

-- Пусть строка из n символов является повторением строки из l символов
-- Какова длина такой записи?
len0 :: [Process] -> Int -> (Int, String)
len0 xs 1 = (lxs, pxs) where
	lxs = length pxs * symbolWidth
	pxs = concatMap show xs
len0 [A] n = (symbolWidth + subscriptWidth, show A ++ "^" ++ show n)
len0 [BA] n = (symbolWidth + bracketWidth*2 + subscriptWidth, "(" ++ show BA ++ ")^" ++ show n)
len0 xs n = (lxs + bracketWidth*2 + subscriptWidth, "(" ++ pxs ++ ")^" ++ show n) where
	(lxs, pxs) = len2M xs

len0M :: [Process] -> Int -> (Int, String)
len0M = memoize len0

-- Простейшая оптимизация: строка as, целиком состоящая из n повторений подстроки bs, может быть записана как bs^n
len1 :: [Process] -> (Int, String)
len1 as = if null inner
	then len0M as 1
	else len0M as 1 `min` minimumBy (comparing fst) inner where
		l = length as
		bs n = take n as
		cs m xs = concat (replicate m xs)
		inner = [len0M (bs n) (l`div`n) | n<-[1..l-1], l`mod`n==0, cs (l`div`n) (bs n) == as]

len1M :: [Process] -> (Int, String)
len1M = memoize len1

-- Перебираем все способы разбить строку на две части и применить к каждой из них len1
len2 :: [Process] -> (Int, String)
len2 as = if null inner
	then len1M as
	else len1M as `min` minimumBy (comparing fst) inner where
		l = length as
		bs n = take n as
		cs n = drop n as
		add (x, xs) (y, ys) = (x+y, xs++ys)
		inner = [ len2M (bs n) `add` len2M (cs n)  | n<-[1..l-1] ]

len2M :: [Process] -> (Int, String)
len2M = memoize len2

prettyProcesses :: [Process] -> String
prettyProcesses = snd . len2M



