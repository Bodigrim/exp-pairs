module ExpPairs.Matrix3 (Matrix3 (..), Vector3 (..), fromList, toList, normalize, prettyMatrix, multCol, det) where

import qualified Data.List as List
import Data.Monoid

data Vector3 t = Vector3 {
	a1 :: t,
	a2 :: t,
	a3 :: t
	}
	deriving (Eq, Show)

data Matrix3 t = Matrix3 {
	a11 :: t,
	a12 :: t,
	a13 :: t,
	a21 :: t,
	a22 :: t,
	a23 :: t,
	a31 :: t,
	a32 :: t,
	a33 :: t
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

	negate a = Matrix3 {
		a11 = - a11 a,
		a12 = - a12 a,
		a13 = - a13 a,
		a21 = - a21 a,
		a22 = - a22 a,
		a23 = - a23 a,
		a31 = - a31 a,
		a32 = - a32 a,
		a33 = - a33 a
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

det :: (Num t) => Matrix3 t -> t
det a =
	a11 a * (a22 a * a33 a - a32 a * a23 a)
	- a12 a * (a21 a * a33 a - a23 a * a31 a)
	+ a13 a * (a21 a * a32 a - a22 a * a31 a)

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

	recip a = Matrix3 {
		a11 =  (a22 a * a33 a - a32 a * a23 a) / d,
		a12 = -(a21 a * a33 a - a23 a * a31 a) / d,
		a13 =  (a21 a * a32 a - a22 a * a31 a) / d,
		a21 = -(a12 a * a33 a - a13 a * a32 a) / d,
		a22 =  (a11 a * a33 a - a13 a * a31 a) / d,
		a23 = -(a11 a * a32 a - a12 a * a31 a) / d,
		a31 =  (a12 a * a23 a - a13 a * a22 a) / d,
		a32 = -(a11 a * a23 a - a13 a * a21 a) / d,
		a33 =  (a11 a * a22 a - a12 a * a21 a) / d
		} where d = det a


instance Num t => Monoid (Matrix3 t) where
	mempty = 1
	mappend = (*)

toList :: Matrix3 t -> [t]
toList a = [a11 a, a12 a, a13 a, a21 a, a22 a, a23 a, a31 a, a32 a, a33 a]

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
	fmap f = fromList . (List.map f) . toList

normalize :: Integral t => Matrix3 t -> Matrix3 t
normalize m = m' where
	l = toList m
	d = foldl1 gcd l
	m' = if d==0 then m else fromList $ List.map (`div`d) l

maximum :: Ord t => Matrix3 t -> t
maximum = List.maximum . toList

prettyMatrix :: (Show t) => Matrix3 t -> String
prettyMatrix m =
	(show $ a11 m) ++ " " ++
	(show $ a12 m) ++ " " ++
	(show $ a13 m) ++ "\n" ++
	(show $ a21 m) ++ " " ++
	(show $ a22 m) ++ " " ++
	(show $ a23 m) ++ "\n" ++
	(show $ a31 m) ++ " " ++
	(show $ a32 m) ++ " " ++
	(show $ a33 m)

multCol :: (Num t) => Matrix3 t -> Vector3 t -> Vector3 t
multCol m v = Vector3 {
	a1 = a11 m * a1 v + a12 m * a2 v + a13 m * a3 v,
	a2 = a21 m * a1 v + a22 m * a2 v + a23 m * a3 v,
	a3 = a31 m * a1 v + a32 m * a2 v + a33 m * a3 v
	}



