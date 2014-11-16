module ExpPairs.Graham where

import qualified ExpPairs.Matrix3 as Mx

import ExpPairs.LinearForm
import ExpPairs.Pair
import ExpPairs.Process

rankinR = k + l where
	(k, l) = markToPair Hux05

abcdef :: (Num t, Eq t) => ProcessMatrix -> RationalForm t -> (t, t, t, t, t, t)
abcdef m (RationalForm num den) = (a, b, c, d, e, f) where
	p = LinearForm (fromInteger $ Mx.a11 m) (fromInteger $ Mx.a12 m) (fromInteger $ Mx.a13 m)
	q = LinearForm (fromInteger $ Mx.a21 m) (fromInteger $ Mx.a22 m) (fromInteger $ Mx.a23 m)
	r = LinearForm (fromInteger $ Mx.a31 m) (fromInteger $ Mx.a32 m) (fromInteger $ Mx.a33 m)
	(LinearForm a b c) = substituteLF (p, q, r) num
	(LinearForm d e f) = substituteLF (p, q, r) den

abcdef2uvw :: Num t => (t, t, t, t, t, t) -> (t, t, t)
abcdef2uvw (a,b,c,d,e,f) = (u,v,w) where
	u = b*f-c*e
	v = a*f-c*d
	w = a*e-b*d

mThet2uvw m rf = abcdef2uvw $ abcdef m rf

uvw2Y (u,v,w) = max (w * rankinR + v - u) (w + v - u)

uvw2Z (u,v,w) = min (w * rankinR + v - u) (w + v - u)

mThet2Y m rf = uvw2Y $ mThet2uvw m rf

mThet2Z m rf = uvw2Z $ mThet2uvw m rf
