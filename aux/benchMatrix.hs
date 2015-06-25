module Main where

import qualified Data.Matrix as M
import qualified Math.ExpPairs.Matrix3 as M3
import Criterion.Main

toM :: M3.Matrix3 a -> M.Matrix a
toM = M.fromList 3 3 . M3.toList

toM3 :: M.Matrix a -> M3.Matrix3 a
toM3 = M3.fromList . M.toList

k :: Integer
k = 560

testm3 :: M3.Matrix3 Integer
testm3 = M3.fromList $ map (100*10^k `div`) [100..108]

testm :: M.Matrix Integer
testm = toM testm3

tribo :: Int -> Integer
tribo n = tri !! n where
	tri = 0 : 0 : 1 : zipWith3 (\a b c -> a + b + c) tri (tail tri) (tail $ tail tri)

triboM :: Integer -> Integer
triboM n = M.getElem 1 1 $ (M.fromList 3 3 [1, 1, 1, 0, 0, 1, 1, 0, 0]) ^ (n - 2)

triboM3 :: Integer -> Integer
triboM3 n = M3.a11 $ (M3.fromList [1, 1, 1, 0, 0, 1, 1, 0, 0]) ^ (n - 2)

-- Our benchmark harness.
main = defaultMain [
  bgroup "mult" [ bench "Data.Matrix" $ nf (testm *) testm
                , bench "usualMult"    $ nf (testm3 `M3.usualMult`) testm3
                --, bench "makarovMult"  $ nf (testm3 `M3.makarovMult`) testm3
                , bench "makarovMult"  $ nf (testm3 `M3.makarovMult'`) testm3
                ],
  bgroup "tribo" [ bench "matrix" $ nf triboM 100000000
                 , bench "matrix3" $ nf triboM3 100000000
  							 ]
  ]
