module PrettyProcess where

import Math.ExpPairs.ProcessMatrix
import Math.ExpPairs.PrettyProcess

import Test.Tasty
import Test.Tasty.SmallCheck as SC
import Test.Tasty.QuickCheck as QC hiding (Positive)

import Instances ()

testUglifyPrettify :: [Process] -> Bool
testUglifyPrettify xs = uglify (prettify xs) == xs

testSuite :: TestTree
testSuite = testGroup "PrettyProcess"
  [ SC.testProperty "uglify . prettify == id" testUglifyPrettify
  , QC.testProperty "uglify . prettify == id" testUglifyPrettify
  ]
