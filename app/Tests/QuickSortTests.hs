module QuickSortTests where

import Test.HUnit
import System.Exit

import Quicksort

testZeroCase = TestCase(assertEqual "Sort Empty List" ([]) (Quicksort.quicksort []))
testListCase = TestCase(assertEqual "Sort List" ([1,2,2,3,3,4,4,5,6,7,8,9,10]) (Quicksort.quicksort [10,2,5,3,1,6,7,4,2,3,4,8,9]))

main :: IO ()
main = do
    counts <- runTestTT ( test [
        testZeroCase,
        testListCase
        ])
    if (errors counts + failures counts == 0)
        then exitSuccess
        else exitFailure