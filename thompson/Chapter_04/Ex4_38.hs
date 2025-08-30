import Chapter_04.Ex4_32 
import Test.HUnit

-----------------------------------------------------------
-- Exercise 4.38

-- | Tests for the power2 function
testPower1 = TestCase (assertEqual "for: power2 0" 
                      1 (power2 0))
testPower2 = TestCase (assertEqual "for: power2 1"
                      2 (power2 1))
testPower3 = TestCase (assertEqual "for: power2 2"
                      4 (power2 2))
testPower4 = TestCase (assertEqual "for: power2 3"
                      8 (power2 3))
testPower5 = TestCase (assertEqual "for: power2 4"
                      16 (power2 4))
testPower6 = TestCase (assertEqual "for: power2 5"
                      32 (power2 5))
testPower7 = TestCase (assertEqual "for: power2 10"
                      1024 (power2 10))

testsPower = TestList [testPower1, testPower2, testPower3, 
                       testPower4, testPower5, testPower6,
                       testPower7]

-----------------------------------------------------------

{-

Testing in GHCi

ghci> :set -package HUnit
ghci> :load Ex4_38
ghci> runTestTT testsPower
Cases: 7  Tried: 7  Errors: 0  Failures: 0
Counts {cases = 7, tried = 7, errors = 0, failures = 0}

-}