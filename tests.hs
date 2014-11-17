import qualified ExpPairs.Tests.RatioInf as RatioInf (testSuite)
import qualified ExpPairs.Tests.Pair as Pair (testSuite)
import qualified ExpPairs.Tests.Ivic as Ivic (testSuite)

main = do
	RatioInf.testSuite
	Pair.testSuite
	Ivic.testSuite
