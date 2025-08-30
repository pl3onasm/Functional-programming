import Test.HUnit

-----------------------------------------------------------
-- Exercise 4.36

attempt m n p = (m /= n) && (n /= p)

-- | Tests
testAtt1 = TestCase (assertEqual "for: attempt 1 1 1" 
                      False (attempt 1 1 1))
testAtt2 = TestCase (assertEqual "for: attempt 1 1 2"
                      False (attempt 1 1 2))
testAtt3 = TestCase (assertEqual "for: attempt 1 2 1"
                      False (attempt 1 2 1))
testAtt4 = TestCase (assertEqual "for: attempt 2 1 1"
                      False (attempt 2 1 1))
testAtt5 = TestCase (assertEqual "for: attempt 1 2 3"
                      True (attempt 1 2 3))
testAtt6 = TestCase (assertEqual "for: attempt 2 3 1"
                      True (attempt 2 3 1))
testAtt7 = TestCase (assertEqual "for: attempt 3 1 2"
                      True (attempt 3 1 2))


testsAtt = TestList [testAtt1, testAtt2, testAtt3, 
                    testAtt4, testAtt5, testAtt6, testAtt7]


-----------------------------------------------------------

{-

We use the tests that we devised in Ex4_35 to test the 
attempt function.

Testing in GHCi

ghci> :set -package HUnit
ghci> :load Ex4_36
ghci> runTestTT testsAtt
### Failure in: 2
Ex4_36.hs:13
for: attempt 1 2 1
expected: False
 but got: True
Cases: 7  Tried: 7  Errors: 0  Failures: 1
Counts {cases = 7, tried = 7, errors = 0, failures = 1}

The attempt function fails the test testAtt3 on line 13.
The reason is that the attempt function only checks if
the first two arguments are different, and if the
second and third arguments are different, but does not
check if the first and third arguments are different.
Thus, it returns True for the input (1, 2, 1), which is
incorrect, since not all three arguments are different.

-}