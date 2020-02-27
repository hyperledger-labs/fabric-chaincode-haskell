import           Test.Tasty
import           Test.Tasty.HUnit

import           Stub

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [stubUnitTests]

stubUnitTests :: TestTree
stubUnitTests = testGroup "Stub Unit Tests" [getState]

getState :: TestTree
getState = testCase "getState" $ [1, 2, 3] `compare` [1, 2] @?= GT
