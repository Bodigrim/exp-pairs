module Main where

import qualified Data.Matrix as M
import qualified Math.ExpPairs.Matrix3 as M3
import Criterion.Main

toM :: M3.Matrix3 a -> M.Matrix a
toM = M.fromList 3 3 . M3.toList

toM3 :: M.Matrix a -> M3.Matrix3 a
toM3 = M3.fromList . M.toList

k :: Integer
k = 450

testm3 :: M3.Matrix3 Integer
testm3 = M3.fromList $ map (100*10^k `div`) [100..108]

testm :: M.Matrix Integer
testm = toM testm3

main = defaultMain [
  bgroup "mult" [ bench "usualMult"    $ nf (testm3 *) testm3
                , bench "makarovMult"  $ nf (testm3 `M3.makarovMult`) testm3
                , bench "ladermanMult" $ nf (testm3 `M3.ladermanMult`) testm3
                , bench "Data.Matrix"  $ nf (testm *) testm
                ]
  ]
