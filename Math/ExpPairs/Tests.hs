--module Math.ExpPairs.Tests where

import qualified Math.ExpPairs.Tests.RatioInf as RatioInf (testSuite)
import qualified Math.ExpPairs.Tests.Pair as Pair (testSuite)
import qualified Math.ExpPairs.Tests.Ivic as Ivic (testSuite)
import qualified Math.ExpPairs.Tests.MenzerNowak as MenzerNowak (testSuite)
import qualified Math.ExpPairs.Tests.Matrix3 as Matrix3 (testSuite)
import qualified Math.ExpPairs.Tests.Kratzel as Kratzel (testSuite)
import qualified Math.ExpPairs.Tests.LinearForm as LinearForm (testSuite)

main :: IO ()
main = do
	print "done"
	--RatioInf.testSuite
	--Pair.testSuite
	--Ivic.testSuite
	--MenzerNowak.testSuite
	--Matrix3.testSuite
	--Kratzel.testSuite
	--LinearForm.testSuite
