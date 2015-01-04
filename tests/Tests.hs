import qualified LinearForm (testSuite)
import qualified Matrix3 (testSuite)
import qualified RatioInf (testSuite)
import qualified Pair (testSuite)

import qualified Ivic (testSuite)
import qualified Kratzel (testSuite)
import qualified MenzerNowak (testSuite)

import Test.Tasty

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
	[ LinearForm.testSuite
	, Matrix3.testSuite
	, RatioInf.testSuite
	, Pair.testSuite
	, Ivic.testSuite
	, Kratzel.testSuite
	, MenzerNowak.testSuite
	]



