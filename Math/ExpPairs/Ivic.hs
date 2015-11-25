{-|
Module      : Math.ExpPairs.Ivic
Description : Riemann zeta-function
Copyright   : (c) Andrew Lelechenko, 2014-2015
License     : GPL-3
Maintainer  : andrew.lelechenko@gmail.com
Stability   : experimental
Portability : POSIX

Provides functions to compute estimates Riemann zeta-function
ζ in a critical strip, given in  /Ivić A./ `The Riemann zeta-function: Theory and applications',
Mineola, New York: Dover Publications, 2003.

-}
module Math.ExpPairs.Ivic
  ( zetaOnS
  , reverseZetaOnS
  , mOnS
  , reverseMOnS
  , checkAbscissa
  , findMinAbscissa
  , mBigOnHalf
  , reverseMBigOnHalf
  ) where

import Data.Ratio
import Data.List  (minimumBy)
import Data.Ord   (comparing)

import Math.ExpPairs

-- | Compute µ(σ) such that |ζ(σ+it)| ≪ |t|^µ(σ) .
-- See equation (7.57) in Ivić2003.
zetaOnS :: Rational -> OptimizeResult
zetaOnS s
  | s >= 1  = simulateOptimize 0
  | s >= 1%2 = optimize
    [RationalForm (LinearForm 1 1 (-s)) 2]
    [Constraint (LinearForm (-1) 1 (-s)) NonStrict]
  | otherwise = optRes {optimalValue = r} where
    optRes = zetaOnS (1-s)
    r = Finite (1%2 - s) + optimalValue optRes

zetaOnHalf :: Rational
zetaOnHalf = 32%205

-- | An attempt to reverse 'zetaOnS'.
reverseZetaOnS :: Rational -> OptimizeResult
reverseZetaOnS mu
  | mu >= 1%2   = simulateOptimize 0
  | mu > zetaOnHalf = optimize [RationalForm (LinearForm 1 (-1) 1) 1] [Constraint (LinearForm 0 (-2) (1+2*mu)) NonStrict]
  | mu == zetaOnHalf = simulateOptimize (1 % 2)
  | otherwise = optRes {optimalValue = negate $ optimalValue optRes} where
  optRes = optimize [RationalForm (LinearForm 1 (-1) 0) 1] [Constraint (LinearForm 1 0 (-mu)) NonStrict, Constraint (LinearForm (-1) 1 (-1%2)) NonStrict]

lemma82_f :: Rational -> Rational
lemma82_f s
  | s < 1%2   = undefined
  | s<= 2%3   =  2/(3-4*s)
  | s<=11%14  = 10/(7-8*s)
  | s<=13%15  = 34/(15-16*s)
  | s<=57%62  = 98/(31-32*s)
  | otherwise =   5/(1-s)

-- | Compute maximal m(σ) such that ∫_1^T |ζ(σ+it)|^m(σ) dt ≪ T^(1+ε).
-- See equation (8.97) in Ivić2003. Further justification will be published elsewhere.
mOnS :: Rational -> OptimizeResult
mOnS s
  | s < 1%2 = simulateOptimize 0
  | s < 5%8 = simulateOptimize $ 4/(3-4*s)
  | s>= 1   = simulateOptimize' InfPlus
  | otherwise = minimumBy (comparing optimalValue) [x1, x2, simulateOptimize (lemma82_f s * 2)] where

    optRes = zetaOnS s
    muS    = toRational $ optimalValue optRes
    alpha1 = (4-4*s)/(1+2*s)
    beta1  = -12/(1+2*s)
    x1 = optRes {optimalValue = Finite $ (1-alpha1)/muS - beta1}

    --alpha2 = 4*(1-s)*(k+l)/((2*m+4*l)*s-m+2*k-2*l)
    --beta2  = -4*(m+2*k+2*l)/((2*m+4*l)*s-m+2*k-2*l)
    --ratio = (1-alpha2)/muS - beta2
    --numer = numerator ratio
    --denom = denominator ratio
    numer = LinearForm
      (-4*s + (-8*muS + 2))
      (-8*s + (-8*muS + 6))
      (-2*s + (-4*muS + 1))
    denom = LinearForm
      (2*muS)
      (4*muS*s - 2*muS)
      (2*muS*s - muS)

    cons = if s >= 2%3 then [] else [Constraint
      (LinearForm (4*s-2) (8*s-6) (2*s-1)) NonStrict
      ]

    x2' = optimize [RationalForm numer denom] cons
    x2 = x2' {optimalValue = negate $ optimalValue x2'}

data Choice = Least | Median | Greatest

binarySearch :: (Rational -> Bool) -> Choice -> Rational -> Rational -> Rational -> Rational
binarySearch predicate choice precision = go
  where
    go a b
      | b - a < precision = case choice of
                            Least    -> a
                            Median   -> c
                            Greatest -> b
      | predicate c = go a c
      | otherwise   = go c b
      where
        c = (numerator a + numerator b) % (denominator a + denominator b)

mOnSTwoThird :: RationalInf
mOnSTwoThird = optimalValue $ mOnS $ 2 % 3

-- | Try to reverse 'mOnS': for a given precision and m compute minimal possible σ.
-- Implementation is usual try-and-divide search, so performance is very poor.
-- Sometimes, when 'mOnS' gets especially lucky exponent pair, 'reverseMOnS' can miss
-- real σ and returns bigger value.
reverseMOnS :: Rational -> RationalInf -> Rational
reverseMOnS _ InfPlus = 1
reverseMOnS _ (Finite m)
  | m <= 4 = 1 % 2
  | m <= 8 = 3 % 4 - recip m
reverseMOnS prec m
  | m < mOnSTwoThird = go (5 % 8) (2 % 3)
  | otherwise        = go (2 % 3) 1
  where
    go = binarySearch (\c -> optimalValue (mOnS c) > m) Median prec

-- | Check whether ∫_1^T   Π_i |ζ(n_i*σ+it)|^m_i dt ≪ T^(1+ε) for a given list of pairs [(n_1, m_1), ...] and fixed σ.
checkAbscissa :: [(Rational, Rational)] -> Rational -> Bool
checkAbscissa xs s = sum rs < Finite 1 where
  qs = map (\(n,m) -> optimalValue (mOnS (n*s)) / Finite m) xs
  rs = map (\q -> 1/q) qs

-- | Find for a given precision and list of pairs [(n_1, m_1), ...] the minimal σ
-- such that ∫_1^T   Π_i|ζ(n_i*σ+it)|^m_i dt ≪ T^(1+ε).
findMinAbscissa :: Rational -> [(Rational, Rational)] -> Rational
findMinAbscissa prec xs = binarySearch (checkAbscissa xs) Greatest prec (1 % 2 / minimum (map fst xs)) 1

-- | Compute minimal M(A) such that ∫_1^T |ζ(1/2+it)|^A dt ≪ T^(M(A)+ε).
-- See Ch. 8 in Ivić2003. Further justification will be published elsewhere.
mBigOnHalf :: Rational -> OptimizeResult
mBigOnHalf a
  | a < 4     = simulateOptimize 1
  | a < 12    = simulateOptimize $ 1+(a-4)/8
  | a > 41614060315296730740083860226662 % 2636743270445733804969041895717 = simulateOptimize $ 1 + 32*(a-6)/205
  | otherwise = if Finite x >= optimalValue optRes
    then simulateOptimize x
    else optRes where
      optRes = optimize [RationalForm (LinearForm 1 1 0) (LinearForm 1 0 0)]
        [Constraint (LinearForm (4-a) 4 2) NonStrict]
      x = 1 + 32*(a-6)/205
-- Constant 41614060315296730740083860226662 % 2636743270445733804969041895717
-- is produced by
-- optimize [RationalForm (LinearForm 4 4 2) (LinearForm 1 0 0)] [Constraint (LinearForm (-64) (-77) 64) Strict]

-- | Try to reverse 'mBigOnHalf': for a given M(A) find maximal possible A.
-- Sometimes, when 'mBigOnHalf' gets especially lucky exponent pair, 'reverseMBigOnHalf' can miss
-- real A and returns lower value.
reverseMBigOnHalf :: Rational -> OptimizeResult
reverseMBigOnHalf m
  | m <= 2 = simulateOptimize $ (m-1)*8 + 4
  | otherwise = if Finite a <= optimalValue optRes
    then simulateOptimize a
    else optRes where
    a = (m-1)*205/32 + 6
    optRes = optimize [RationalForm (LinearForm 4 4 2) (LinearForm 1 0 0)] [Constraint (LinearForm (1-m) 1 0) NonStrict]


