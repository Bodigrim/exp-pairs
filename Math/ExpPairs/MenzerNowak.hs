{-|
Module      : Math.ExpPairs.MenzerNowak
Description : Asymmetric divisor problem with congruence conditions
Copyright   : (c) Andrew Lelechenko, 2014-2015
License     : GPL-3
Maintainer  : andrew.lelechenko@gmail.com
Stability   : experimental
Portability : POSIX

Let τ_{a, b}(l_1, k_1; l_2, k_2; n) denote the number of integer
(v, w) with v^a w^b = n, v ≡ l_1 (mod k_1), w ≡ l_2 (mod k_2).

Menzer and Nowak
  (/Menzer H., Nowak W. G./ `On an asymmetric divisor problem with
  congruence conditions' \/\/ Manuscr. Math., 1989, Vol. 64, no. 1, P. 107-119)
proved an asymptotic formula for
Σ_{n ≤ x} τ_{a, b}(l_1, k_1; l_2, k_2; n) with an error term of order (x \/ k_1^a \/ k_2^b)^(Θ(a, b) + ε). They provided an expression for Θ(a, b) in terms of exponent pairs.

-}
module Math.ExpPairs.MenzerNowak
  ( menzerNowak
  ) where

import Data.Ratio    ((%))

import Math.ExpPairs

-- |Compute Θ(a, b) for given a and b.
menzerNowak :: Integer -> Integer -> OptimizeResult
menzerNowak a' b' = optimize
  [
    RationalForm (LinearForm 1 1 0) (LinearForm (a+b) 0 (a+b)),
    RationalForm (LinearForm 1 0 0) (LinearForm (a+b) (-a) a)
  ]
  [] where
    a = a'%1
    b = b'%1
