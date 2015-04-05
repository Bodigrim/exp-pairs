{-# LANGUAGE BangPatterns, RecordWildCards #-}
{-|
Module      : Math.ExpPairs.Matrix3
Description : Implements matrices of order 3
Copyright   : (c) Andrew Lelechenko, 2014-2015
License     : GPL-3
Maintainer  : andrew.lelechenko@gmail.com
Stability   : experimental
Portability : POSIX

Provides types and functions for matrices and vectors of order 3.
Can be used instead of "Data.Matrix" to reduce overhead and simplify code.
-}
module Math.ExpPairs.Matrix3 (Matrix3 (..), Vector3 (..), fromList, toList, det, multCol, normalize, prettyMatrix) where

import qualified Data.List as List
import Data.Monoid

-- |Three-component vector.
data Vector3 t = Vector3 {
	a1 :: !t,
	a2 :: !t,
	a3 :: !t
	}
	deriving (Eq, Show)

-- |Matrix of order 3. Instances of 'Num', 'Fractional' and 'Monoid'
-- are defined in terms of the multiplicative group of matrices,
-- not the additive one. E. g.,
--
-- > toList 1 == [1,0,0,0,1,0,0,0,1]
-- > toList 1 /= [1,1,1,1,1,1,1,1,1]
--
data Matrix3 t = Matrix3 {
	a11 :: !t,
	a12 :: !t,
	a13 :: !t,
	a21 :: !t,
	a22 :: !t,
	a23 :: !t,
	a31 :: !t,
	a32 :: !t,
	a33 :: !t
	}
	deriving (Eq, Show)

instance Num t => Num (Matrix3 t) where
	a + b = Matrix3 {
		a11 = a11 a + a11 b,
		a12 = a12 a + a12 b,
		a13 = a13 a + a13 b,
		a21 = a21 a + a21 b,
		a22 = a22 a + a22 b,
		a23 = a23 a + a23 b,
		a31 = a31 a + a31 b,
		a32 = a32 a + a32 b,
		a33 = a33 a + a33 b
		}

	-- intercalate ",\n" [ "a"++(show i)++(show j)++" = "++( intercalate " + " ["a"++(show i)++(show k)++" a * "++"a"++(show k)++(show j)++" b" | k<-[1..3]] )  | i<-[1..3], j<-[1..3]]
	a * b = Matrix3 {
		a11 = a11 a * a11 b + a12 a * a21 b + a13 a * a31 b,
		a12 = a11 a * a12 b + a12 a * a22 b + a13 a * a32 b,
		a13 = a11 a * a13 b + a12 a * a23 b + a13 a * a33 b,
		a21 = a21 a * a11 b + a22 a * a21 b + a23 a * a31 b,
		a22 = a21 a * a12 b + a22 a * a22 b + a23 a * a32 b,
		a23 = a21 a * a13 b + a22 a * a23 b + a23 a * a33 b,
		a31 = a31 a * a11 b + a32 a * a21 b + a33 a * a31 b,
		a32 = a31 a * a12 b + a32 a * a22 b + a33 a * a32 b,
		a33 = a31 a * a13 b + a32 a * a23 b + a33 a * a33 b
		}

	negate Matrix3 {..} = Matrix3 {
		a11 = - a11,
		a12 = - a12,
		a13 = - a13,
		a21 = - a21,
		a22 = - a22,
		a23 = - a23,
		a31 = - a31,
		a32 = - a32,
		a33 = - a33
		}

	abs = undefined

	signum = undefined

	-- Multiplicative, not additive behaviour
	fromInteger n = Matrix3 {
		a11 = fromInteger n,
		a12 = 0,
		a13 = 0,
		a21 = 0,
		a22 = fromInteger n,
		a23 = 0,
		a31 = 0,
		a32 = 0,
		a33 = fromInteger n
		}

-- |Computes the determinant of a matrix.
det :: (Num t) => Matrix3 t -> t
det Matrix3 {..} =
	a11 * (a22 * a33 - a32 * a23)
	- a12 * (a21 * a33 - a23 * a31)
	+ a13 * (a21 * a32 - a22 * a31)

instance Fractional t => Fractional (Matrix3 t) where
	-- Multiplicative, not additive behaviour
	fromRational n = Matrix3 {
		a11 = fromRational n,
		a12 = 0,
		a13 = 0,
		a21 = 0,
		a22 = fromRational n,
		a23 = 0,
		a31 = 0,
		a32 = 0,
		a33 = fromRational n
		}

	recip a@(Matrix3 {..}) = Matrix3 {
		a11 =  (a22 * a33 - a32 * a23) / d,
		a12 = -(a21 * a33 - a23 * a31) / d,
		a13 =  (a21 * a32 - a22 * a31) / d,
		a21 = -(a12 * a33 - a13 * a32) / d,
		a22 =  (a11 * a33 - a13 * a31) / d,
		a23 = -(a11 * a32 - a12 * a31) / d,
		a31 =  (a12 * a23 - a13 * a22) / d,
		a32 = -(a11 * a23 - a13 * a21) / d,
		a33 =  (a11 * a22 - a12 * a21) / d
		} where d = det a


instance Num t => Monoid (Matrix3 t) where
	mempty = 1
	mappend = (*)

-- |Convert 'Matrix3' into a list of 9 elements.
toList :: Matrix3 t -> [t]
toList Matrix3 {..} = [a11, a12, a13, a21, a22, a23, a31, a32, a33]

-- |Convert a list of 9 elements into 'Matrix3'.
fromList :: [t] -> Matrix3 t
fromList as = Matrix3 {
		a11 = as!!0,
		a12 = as!!1,
		a13 = as!!2,
		a21 = as!!3,
		a22 = as!!4,
		a23 = as!!5,
		a31 = as!!6,
		a32 = as!!7,
		a33 = as!!8
		}

instance Functor Matrix3 where
	fmap f = fromList . List.map f . toList

-- |Divide all elements of the matrix by their greatest common
-- divisor. This is useful for matrices of projective
-- transformations to reduce the magnitude of computations.
normalize :: Integral t => Matrix3 t -> Matrix3 t
normalize m = m' where
	l = toList m
	d = foldl1 gcd l
	m' = if d==0 then m else fromList $ List.map (`div`d) l

-- |Return the maximal element of a matrix.
maximum :: Ord t => Matrix3 t -> t
maximum = List.maximum . toList

-- |Print a matrix, separating rows with new lines and elements
-- with spaces.
prettyMatrix :: Show t => Matrix3 t -> String
prettyMatrix Matrix3 {..} =
	show a11 ++ ' '  :
	show a12 ++ ' '  :
	show a13 ++ '\n' :
	show a21 ++ ' '  :
	show a22 ++ ' '  :
	show a23 ++ '\n' :
	show a31 ++ ' '  :
	show a32 ++ ' '  :
	show a33

-- |Multiplicate a matrix by a vector (considered as a column).
multCol :: Num t => Matrix3 t -> Vector3 t -> Vector3 t
multCol Matrix3 {..} Vector3 {..} = Vector3 {
	a1 = a11 * a1 + a12 * a2 + a13 * a3,
	a2 = a21 * a1 + a22 * a2 + a23 * a3,
	a3 = a31 * a1 + a32 * a2 + a33 * a3
	}



