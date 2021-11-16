module Main (main) where

import Test.HUnit
import System.Exit
import Calculator as Calc

testZeroCase = TestCase(assertEqual "Factorial 1" (1) (Calc.factorial 1))
testTenCase = TestCase(assertEqual "Factorial 10" (3628800) (Calc.factorial 10))
testFourCase = TestCase(assertEqual "Factorial 4" (24) (Calc.factorial 4))
testEightCase = TestCase(assertEqual "Factorial 8" (40320) (Calc.factorial 8))


main :: IO ()
main = do
    counts <- runTestTT ( test [
        testZeroCase,
        testTenCase,
        testFourCase,
        testEightCase
        ])
    if (errors counts + failures counts == 0)
        then exitSuccess
        else exitFailure