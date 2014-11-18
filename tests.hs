import qualified ExpPairs.Tests.RatioInf as RatioInf (testSuite)
import qualified ExpPairs.Tests.Pair as Pair (testSuite)
import qualified ExpPairs.Tests.Ivic as Ivic (testSuite)
import qualified ExpPairs.Tests.MenzerNowak as MenzerNowak (testSuite)

main = do
	RatioInf.testSuite
	Pair.testSuite
	Ivic.testSuite
	MenzerNowak.testSuite
