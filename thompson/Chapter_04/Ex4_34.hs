import Test.HUnit

-----------------------------------------------------------
-- Exercise 4.34

solution m n p = ((m + n + p) == 3 * p)

-- | Tests
testSol1 = TestCase (assertEqual "for: solution 1 1 1" 
                      True (solution 1 1 1))
testSol2 = TestCase (assertEqual "for: solution 1 2 1"
                      False (solution 1 2 1))
testSol3 = TestCase (assertEqual "for: solution 1 1 2"
                      False (solution 1 1 2))
testSol4 = TestCase (assertEqual "for: solution 2 1 1"
                      False (solution 2 1 1))
testSol5 = TestCase (assertEqual "for: solution 1 2 3"
                      False (solution 1 2 3))

testsSol = TestList [testSol1, testSol2, testSol3, 
                     testSol4, testSol5]

-----------------------------------------------------------

{-

Testing in GHCi

ghci> :set -package HUnit
ghci> :load Ex4_34
ghci> runTestTT testsSol
Cases: 5  Tried: 5  Errors: 0  Failures: 0
Counts {cases = 5, tried = 5, errors = 0, failures = 0}

Apparently, using our tests from Ex4_33, all tests pass.
This is because our tests from Ex4_33 did not include
scenarios where ordering in the arguments mattered when
all three arguments are different.
If we add the following test, we can see that the
solution function fails:

testSol6 = TestCase (assertEqual "for: solution 1 3 2"
                      False (solution 1 3 2))

Moral: when writing tests, they should be tailored to the
specific function being tested, and not just re-used from
some other function.

-}