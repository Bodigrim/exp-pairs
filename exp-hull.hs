import Data.Ratio
import Data.Ord
import Data.List
import Data.Monoid

import ExpPairs.Optimize
import ExpPairs.Kratzel
import ExpPairs.MenzerNowak
import ExpPairs.Ivic

main :: IO()
main = print $ searchMinAbscissa [(4,2),(5,4),(6,2),(7,6)]
