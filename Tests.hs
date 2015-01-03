import qualified ExpPairs.Tests.RatioInf as RatioInf (testSuite)
import qualified ExpPairs.Tests.Pair as Pair (testSuite)
import qualified ExpPairs.Tests.Ivic as Ivic (testSuite)
import qualified ExpPairs.Tests.MenzerNowak as MenzerNowak (testSuite)
import qualified ExpPairs.Tests.Matrix3 as Matrix3 (testSuite)
import qualified ExpPairs.Tests.Kratzel as Kratzel (testSuite)
import qualified ExpPairs.Tests.LinearForm as LinearForm (testSuite)

main = do
	RatioInf.testSuite
	Pair.testSuite
	Ivic.testSuite
	MenzerNowak.testSuite
	Matrix3.testSuite
	Kratzel.testSuite
	LinearForm.testSuite
