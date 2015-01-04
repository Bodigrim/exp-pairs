import qualified RatioInf (testSuite)
import qualified Pair (testSuite)
import qualified Ivic as Ivic (testSuite)
--import qualified Math.ExpPairs.Tests.MenzerNowak as MenzerNowak (testSuite)
--import qualified Math.ExpPairs.Tests.Matrix3 as Matrix3 (testSuite)
--import qualified Math.ExpPairs.Tests.Kratzel as Kratzel (testSuite)
--import qualified Math.ExpPairs.Tests.LinearForm as LinearForm (testSuite)

import Test.Tasty
--import Test.Tasty.SmallCheck as SC
--import Test.Tasty.QuickCheck as QC

--import Data.List
--import Data.Ord

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
	[ RatioInf.testSuite
	, Pair.testSuite
	, Ivic.testSuite
	]



