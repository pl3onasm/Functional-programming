import Test.HUnit

-----------------------------------------------------------
-- Exercise 4.33

-- | Function to test (taken from Ex3_10)
allEqual :: Integer -> Integer -> Integer -> Bool
allEqual x y z = (x == y) && (y == z)

-- | Tests
testAllEq1 = TestCase (assertEqual "for: allEqual 1 1 1" 
                      True (allEqual 1 1 1))
testAllEq2 = TestCase (assertEqual "for: allEqual 1 2 1"
                      False (allEqual 1 2 1))
testAllEq3 = TestCase (assertEqual "for: allEqual 1 1 2"
                      False (allEqual 1 1 2))
testAllEq4 = TestCase (assertEqual "for: allEqual 2 1 1"
                      False (allEqual 2 1 1))
testAllEq5 = TestCase (assertEqual "for: allEqual 1 2 3"
                      False (allEqual 1 2 3))

testsAllEq = TestList [testAllEq1, testAllEq2, testAllEq3, 
                       testAllEq4, testAllEq5]

-----------------------------------------------------------

{-

Testing in GHCi

ghci> :set -package HUnit
ghci> :load Ex4_33
ghci> runTestTT testsAllEq
Cases: 5  Tried: 5  Errors: 0  Failures: 0
Counts {cases = 5, tried = 5, errors = 0, failures = 0}


-}

