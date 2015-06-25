{-# LANGUAGE BangPatterns, RecordWildCards, DeriveFunctor, DeriveFoldable, DeriveGeneric #-}
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
module Math.ExpPairs.Matrix3
	( Matrix3 (..)
	, Vector3 (..)
	, fromList
	, toList
	, det
	, multCol
	, normalize
	, usualMult
	, makarovMult
	, makarovMult'
	) where

import Prelude hiding (foldl1)
import Data.Foldable  (Foldable (..), toList)
import GHC.Generics   (Generic (..))
import Data.List      (transpose)
import Control.DeepSeq

-- |Three-component vector.
data Vector3 t = Vector3 {
	a1 :: !t,
	a2 :: !t,
	a3 :: !t
	}
	deriving (Eq, Show, Functor, Foldable, Generic)

instance NFData t => NFData (Vector3 t) where
	rnf = rnf . toList

-- |Matrix of order 3. Instances of 'Num' and 'Fractional'
-- are given in terms of the multiplicative group of matrices,
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
	deriving (Eq, Functor, Foldable, Generic)

instance NFData t => NFData (Matrix3 t) where
	rnf = rnf . toList

diag :: Num t => t -> Matrix3 t
diag n = Matrix3 {
	a11 = n,
	a12 = 0,
	a13 = 0,
	a21 = 0,
	a22 = n,
	a23 = 0,
	a31 = 0,
	a32 = 0,
	a33 = n
	}

instance (Num t, Ord t) => Num (Matrix3 t) where
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

	a * b = (if maximum (toList a) > 10^550 then makarovMult' else usualMult) a b

	negate = fmap negate

	abs = undefined

	signum = diag . signum . det

	fromInteger = diag . fromInteger

usualMult :: Num t => Matrix3 t -> Matrix3 t -> Matrix3 t
usualMult a b = Matrix3 {
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
-- {-# INLINE usualMult #-}
{-# SPECIALIZE usualMult :: Matrix3 Int -> Matrix3 Int -> Matrix3 Int #-}
{-# SPECIALIZE usualMult :: Matrix3 Integer -> Matrix3 Integer -> Matrix3 Integer #-}

makarovMult :: Num t => Matrix3 t -> Matrix3 t -> Matrix3 t
makarovMult
	(Matrix3 k1 b1 c1 k2 b2 c2 k3 b3 c3)
	(Matrix3 a1 a2 a3 k4 k5 k6 k7 k8 k9)
	= Matrix3 r1 r2 r3 r4 r5 r6 r7 r8 r9 where
		m1  = (a3 + c1 - c2) * (k1 + k7 - k8 + k9)
		m2  = (a2 + b1 + b2) * (k2 - k4 + k5 - k6)
		m3  = (a2 + b1 + b3) * (k3 - k4 + k5 - k6)
		m4  = (a3 - c2 - c3) * (k3 - k7 + k8 - k9)
		m5  = (a1 - c1 + c2) * k1
		m6  = (a1 + b1 + b2) * k2
		m7  = (a1 + b1 + b3 + c2 + c3) * k3
		m8  = a2 * (k1 + k4 - k5 + k6)
		m9  = a3 * (k2 + k7 - k8 + k9)
		m10 = b1 * k4
		m11 = c2 * k7
		m12 = (c1 - c2) * (k1 + k7)
		m13 = (b1 + b2) * (k4 - k2)
		m14 = (a2 + b1) * (k4 - k5 + k6)
		m15 = b2 * k6
		m16 = (a3 - c2) * (k7 - k8 + k9)
		m17 = c2 * k8
		m18 = (b3 - c2 - c3) * k6
		m19 = (c1 + c3 - b1 - b3) * k8
		m20 = (b1 + b3) * (k4 - k3 + k6 + k8)
		m21 = (c2 + c3) * (k3 + k6 - k7 + k8)
		m22 = (c2 + c3 - b1 - b3) * (k6 + k8)

		r1  = m5 + m10 + m11 + m12
		r2  = m8 + m10 - m14 + m17 - m18 + m19 - m22
		r3  = m1 - m11 - m12 - m16 + m17 - m18 + m19 - m22
		r4  = m6 - m10 + m11 + m13
		r5  = m2 - m10 + m13 + m14 + m15 + m17
		r6  = m9 - m11 + m15 - m16 + m17
		r7  = m7 - m10 - m11 + m20 - m21 + m22
		r8  = m3 - m10 + m14 - m17 + m18 + m20 + m22
		r9  = m4 + m11 + m16 - m17 + m18 + m21
-- {-# INLINE makarovMult #-}
{-# SPECIALIZE makarovMult :: Matrix3 Int -> Matrix3 Int -> Matrix3 Int #-}
{-# SPECIALIZE makarovMult :: Matrix3 Integer -> Matrix3 Integer -> Matrix3 Integer #-}

makarovMult' :: Num t => Matrix3 t -> Matrix3 t -> Matrix3 t
makarovMult'
	(Matrix3 k1 b1 c1 k2 b2 c2 k3 b3 c3)
	(Matrix3 a1 a2 a3 k4 k5 k6 k7 k8 k9)
	= Matrix3 r1 r2 r3 r4 r5 r6 r7 r8 r9 where
		a3_c2 = a3 - c2
		a2_b1 = a2 + b1
		a1_b1 = a1 + b1
		c1_c2 = c1 - c2
		c2_c3 = c2 + c3
		b1_b3 = b1 + b3

		k1_k7 = k1 + k7
		k8_k9 = k8 - k9
		k2_k4 = k2 - k4
		k5_k6 = k5 - k6
		k6_k8 = k6 + k8

		m1  = (a3_c2 + c1) * (k1_k7 - k8_k9)
		m2  = (a2_b1 + b2) * (k2_k4 + k5_k6)
		m3  = (a2_b1 + b3) * (k3 - k4 + k5_k6)
		m4  = (a3_c2 - c3) * (k3 - k7 + k8_k9)
		m5  = (a1 - c1_c2) * k1
		m6  = (a1_b1 + b2) * k2
		m7  = (a1_b1 + b3 + c2_c3) * k3
		m8  = a2 * (k1 + k4 - k5_k6)
		m9  = a3 * (k2 + k7 - k8_k9)
		m10 = b1 * k4
		m11 = c2 * k7
		m12 = c1_c2 * k1_k7
		m13 = (b1 + b2) * (-k2_k4)
		m14 = a2_b1 * (k4 - k5_k6)
		m15 = b2 * k6
		m16 = a3_c2 * (k7 - k8_k9)
		m17 = c2 * k8
		m18 = (b3 - c2_c3) * k6
		m19 = (c1 + c3 - b1_b3) * k8
		m20 = b1_b3 * (k4 - k3 + k6_k8)
		m21 = c2_c3 * (k3 + k6_k8 - k7)
		m22 = (c2_c3 - b1_b3) * k6_k8

		m10_m11 = m10 + m11
		m10_m14 = m10 - m14
		m11_m15_m17 = m11 + m16 - m17
		m17_m18_m22 = m17 - m18 - m22

		r1  = m5 + m10_m11 + m12
		r2  = m8 + m10_m14 + m17_m18_m22 + m19
		r3  = m1 - m11_m15_m17 - m12 - m18 + m19 - m22
		r4  = m6 - m10 + m11 + m13
		r5  = m2 - m10_m14 + m13 + m15 + m17
		r6  = m9 - m11_m15_m17 + m15
		r7  = m7 - m10_m11 + m20 - m21 + m22
		r8  = m3 - m10_m14 - m17_m18_m22 + m20
		r9  = m4 + m11_m15_m17 + m18 + m21
-- {-# INLINE makarovMult #-}
{-# SPECIALIZE makarovMult' :: Matrix3 Int -> Matrix3 Int -> Matrix3 Int #-}
{-# SPECIALIZE makarovMult' :: Matrix3 Integer -> Matrix3 Integer -> Matrix3 Integer #-}

-- |Compute the determinant of a matrix.
det :: (Num t, Ord t) => Matrix3 t -> t
det Matrix3 {..} =
	a11 * (a22 * a33 - a32 * a23)
	- a12 * (a21 * a33 - a23 * a31)
	+ a13 * (a21 * a32 - a22 * a31)

instance (Fractional t, Ord t) => Fractional (Matrix3 t) where
	fromRational = diag . fromRational

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

-- |Convert a list of 9 elements into 'Matrix3'. Reverse conversion can be done by 'toList' from "Data.Foldable".
fromList :: [t] -> Matrix3 t
fromList [a11, a12, a13, a21, a22, a23, a31, a32, a33] = Matrix3 {
	a11 = a11,
	a12 = a12,
	a13 = a13,
	a21 = a21,
	a22 = a22,
	a23 = a23,
	a31 = a31,
	a32 = a32,
	a33 = a33
	}
fromList _ = error "The list must contain exactly 9 elements"

-- |Divide all elements of the matrix by their greatest common
-- divisor. This is useful for matrices of projective
-- transformations to reduce the magnitude of computations.
normalize :: Integral t => Matrix3 t -> Matrix3 t
normalize a = case foldl1 gcd a of
	0 -> a
	d -> fmap (`div` d) a

instance Show t => Show (Matrix3 t) where
	show = unlines . map unwords . pad . fmap show where
		pad (Matrix3 {..}) = map (zipWith padCell ls) table where
			table = [[a11, a12, a13], [a21, a22, a23], [a31, a32, a33]]
			ls = map (maximum . map length) (transpose table)
			padCell l xs = replicate (l - length xs) ' ' ++ xs

-- |Multiplicate a matrix by a vector (considered as a column).
multCol :: Num t => Matrix3 t -> Vector3 t -> Vector3 t
multCol Matrix3 {..} Vector3 {..} = Vector3 {
	a1 = a11 * a1 + a12 * a2 + a13 * a3,
	a2 = a21 * a1 + a22 * a2 + a23 * a3,
	a3 = a31 * a1 + a32 * a2 + a33 * a3
	}
