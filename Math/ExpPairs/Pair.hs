{-|
Module      : Math.ExpPairs.RatioInf
Description : Initial exponent pairs
Copyright   : (c) Andrew Lelechenko, 2014-2015
License     : GPL-3
Maintainer  : andrew.lelechenko@gmail.com
Stability   : experimental
Portability : POSIX

Provides a set of initial exponent pairs, consisting
of two points (0, 1), (1\/2, 1\/2) and a triangle with vertices in (1\/6, 2\/3), (2\/13, 35\/52) and (32\/205, 269\/410). The triangle is represented as a list of nodes of a net, covering the triangle.

Below /A/ and /B/ stands for van der Corput's processes. See "Math.ExpPairs.Process" for explanations.
-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Math.ExpPairs.Pair
  ( Triangle (..)
  , InitPair' (..)
  , InitPair
  , initPairs
  , initPairToValue
  , initPairToProjValue
  ) where

import Data.Maybe
import Data.Ratio
import GHC.Generics (Generic (..))
import Data.Text.Prettyprint.Doc

-- |Vertices of the triangle of initial exponent pairs.
data Triangle
  -- |Usual van der Corput exponent pair
  -- (1\/6, 2\/3) = /AB/(0, 1).
  = Corput16
  -- |An exponent pair (2\/13, 35\/52) from /Huxley M. N./
  -- `Exponential sums and the Riemann zeta function'
  -- \/\/ Proceedings of the International Number
  -- Theory Conference held at Universite Laval in 1987, Walter de Gruyter, 1989, P. 417-423.
  | HuxW87b1
  -- | An exponent pair (32\/205, 269\/410) from /Huxley M. N./
  -- `Exponential sums and the Riemann zeta function V' \/\/
  -- Proc. Lond. Math. Soc., 2005, Vol. 90, no. 1., P. 1--41.
  | Hux05
  deriving (Show, Bounded, Enum, Eq, Ord, Generic)

instance Pretty Triangle where
  pretty = pretty . show

-- |Type to hold an initial exponent pair.
data InitPair' t
  -- |Usual van der Corput exponent pair
  -- (0, 1).
  = Corput01
  -- |Usual van der Corput exponent pair
  -- (1\/2, 1\/2) = /B/(0, 1).
  | Corput12
  -- |Point from the interior of 'Triangle'.
  -- Exactly
  -- 'Mix' a b = a * 'Corput16' + b * 'HuxW87b1' + (1-a-b) * 'Hux05'
  | Mix !t !t
  deriving (Eq, Ord, Show, Generic)

-- |Exponent pair built from rational fractions of
-- 'Corput16', 'HuxW87b1' and 'Hux05'
type InitPair = InitPair' Rational

instance (Integral a, Show a) => Pretty (Ratio a) where
  pretty = pretty . show

instance (Pretty t, Num t, Eq t) => Pretty (InitPair' t) where
  pretty Corput01 = parens (pretty (0%1) <> comma <+> pretty (1%1))
  pretty Corput12 = parens (pretty (1%2) <> comma <+> pretty (1%2))
  pretty (Mix r1 r2) = cat $ punctuate plus $ mapMaybe f [(r1, Corput16), (r2, HuxW87b1), (1 - r1 - r2, Hux05)] where
    plus = space <> pretty "+" <> space
    f (0, _) = Nothing
    f (1, t) = Just (pretty t)
    f (r, t) = Just (pretty r <+> pretty "*" <+> pretty t)

sect :: Integer
sect = 30

-- |The set of initial exponent pairs. It consists of
-- 'Corput01', 'Corput12' and 496 = sum [1..31] 'Mix'-points,
-- which forms a uniform net over 'Triangle'.
initPairs :: [InitPair]
initPairs = Corput01 : Corput12 : [Mix (r1 % sect) (r2 % sect) | r1 <- [0 .. sect], r2 <- [0 .. sect - r1]]

-- |Convert initial exponent pair from its symbolic representation
-- as 'InitPair' to pair of rationals.
initPairToValue :: InitPair -> (Rational, Rational)
initPairToValue (Mix r1 r2) = (x, y) where
  r3 = 1 - r1 - r2
  (x1, y1) = (1%6, 2%3)
  (x2, y2) = ( 2 %  13,  35 %  52)
  (x3, y3) = (32 % 205, 269 % 410)
  x = x1*r1 + x2*r2 + x3*r3
  y = y1*r1 + y2*r2 + y3*r3
--initPairToValue (Mix r1 r2) = (13 % 1230 * r1 - 6 % 2665 * r2 + 32 % 205, 13 % 1230 * r1 + 181 % 10660 * r2 + 269 % 410)
initPairToValue Corput01 = (0, 1)
initPairToValue Corput12 = (1%2, 1%2)

-- | Same as 'initPairToValue', but immediately convert from Q^2 to PN^3.
initPairToProjValue :: InitPair -> (Integer, Integer, Integer)
initPairToProjValue (Mix r1 r2) = (k `div` d , l `div` d, m `div` d)
  where
    dr1 = denominator r1
    dr2 = denominator r2
    m = 31980 * dr1 * dr2
    k = 338 * numerator r1 * dr2 -  72 * numerator r2 * dr1 +  4992 * dr1 * dr2
    l = 338 * numerator r1 * dr2 + 543 * numerator r2 * dr1 + 20982 * dr1 * dr2

    d = k `gcd` l `gcd` m

initPairToProjValue Corput01 = (0, 1, 1)
initPairToProjValue Corput12 = (1, 1, 2)

{-# INLINABLE initPairToProjValue #-}
