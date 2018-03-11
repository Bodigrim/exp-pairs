{-|
Module      : Math.ExpPairs.ProcessMatrix
Description : Monoidal wrapper for Matrix3
Copyright   : (c) Andrew Lelechenko, 2014-2015
License     : GPL-3
Maintainer  : andrew.lelechenko@gmail.com
Stability   : experimental
Portability : POSIX

Provides types for sequences of /A/- and /B/-processes of van der Corput. A good account on this topic can be found in /Graham S. W.,  Kolesnik G. A./ Van Der Corput's Method of Exponential Sums, Cambridge University Press, 1991, especially Ch. 5.
-}

{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Math.ExpPairs.ProcessMatrix
  ( Process (..)
  , ProcessMatrix ()
  , aMatrix
  , baMatrix
  , evalMatrix
  ) where

#if __GLASGOW_HASKELL__ < 710
import Data.Monoid           (Monoid, mempty, mappend)
#endif
import GHC.Generics          (Generic (..))
import Data.Text.Prettyprint.Doc

import Math.ExpPairs.Matrix3

-- | Since B^2 = id, B 'Corput16' = 'Corput16', B 'Hux05' = 'Hux05' and B 'HuxW87b1' = ???, the sequence of /A/- and /B/-processes, applied to 'initPairs' can be rewritten as a sequence of 'A' and 'BA'.
data Process
  -- | /A/-process
  = A
  -- | /BA/-process
  | BA
  deriving (Eq, Show, Read, Ord, Enum, Generic)

instance Pretty Process where
  pretty = pretty . show

-- | Sequence of processes, represented as a matrix 3x3.
newtype ProcessMatrix = ProcessMatrix (Matrix3 Integer)
  deriving (Eq, Num, Show, Pretty)

instance Monoid ProcessMatrix where
  mempty = 1
  mappend (ProcessMatrix a) (ProcessMatrix b) = ProcessMatrix $ normalize $ a * b

process2matrix :: Process -> ProcessMatrix
process2matrix  A = ProcessMatrix $ Matrix3 1 0 0 1 1 1  2 0 2
process2matrix BA = ProcessMatrix $ Matrix3 0 1 0 2 0 1  2 0 2

-- | Return process matrix for 'A'-process.
aMatrix :: ProcessMatrix
aMatrix = process2matrix A

-- | Return process matrix for 'BA'-process.
baMatrix :: ProcessMatrix
baMatrix = process2matrix BA

-- |Apply a projective transformation, defined by 'Path',
-- to a given point in two-dimensional projective space.
evalMatrix :: Num t => ProcessMatrix -> (t, t, t) -> (t, t, t)
evalMatrix (ProcessMatrix m) = multCol (fmap fromInteger m)
{-# INLINABLE evalMatrix #-}
{-# SPECIALIZE evalMatrix :: ProcessMatrix -> (Integer, Integer, Integer) -> (Integer, Integer, Integer) #-}

