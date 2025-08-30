import Chapter_03.Ex3_20
import Test.HUnit

-----------------------------------------------------------
-- Exercise 4.37

-- here we are testing the function howManyAboveAverage
-- which counts how many of the three arguments are above
-- the average of the three arguments.

testNAbAvg1 = TestCase (assertEqual "for: NAbAvg 5 5 5" 
                      0 (howManyAboveAverage 5 5 5))
testNAbAvg2 = TestCase (assertEqual "for: NAbAvg 15 21 25"
                      2 (howManyAboveAverage 15 21 25))
testNAbAvg3 = TestCase (assertEqual "for: NAbAvg 1 2 3"
                      1 (howManyAboveAverage 1 2 3))
testNAbAvg4 = TestCase (assertEqual "for: NAbAvg 21 15 25"
                      2 (howManyAboveAverage 21 15 25))
testNAbAvg5 = TestCase (assertEqual "for: NAbAvg 25 21 15"
                      2 (howManyAboveAverage 25 21 15))
testNAbAvg6 = TestCase (assertEqual "for: NAbAvg 1 3 2"
                      1 (howManyAboveAverage 1 3 2))
testNAbAvg7 = TestCase (assertEqual "for: NAbAvg 3 1 2"
                      1 (howManyAboveAverage 3 1 2))

testsNAbAvg = TestList [testNAbAvg1, testNAbAvg2, 
                        testNAbAvg3, testNAbAvg4, 
                        testNAbAvg5, testNAbAvg6,
                        testNAbAvg7]


-----------------------------------------------------------

{-

Testing in GHCi

ghci> :set -package HUnit
ghci> :load Ex4_37
ghci> runTestTT testsNAbAvg
Cases: 7  Tried: 7  Errors: 0  Failures: 0
Counts {cases = 7, tried = 7, errors = 0, failures = 0}

-}