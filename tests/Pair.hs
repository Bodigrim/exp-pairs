module Pair where

import Math.ExpPairs.Pair (InitPair, initPairToValue)

import Data.Ratio
import Test.Tasty
import Test.Tasty.SmallCheck as SC
import Test.Tasty.QuickCheck as QC

import Instances ()

testBounds :: InitPair -> Bool
testBounds ip = k>=0 && k<=1%2 && l>=1%2 && l<=1 where
  (k, l) = initPairToValue ip

testSuite :: TestTree
testSuite = testGroup "Pair"
  [ SC.testProperty "bounds" testBounds
  , QC.testProperty "bounds" testBounds
  ]
