import Chapter_07.Ex7_16
import Test.HUnit

-----------------------------------------------------------
-- Exercise 7.18

tests :: Test
tests = TestList [test1, test2, test3, test4, test5,
                  test6, test7, test8]

-- | Test case: empty list
test1 = TestCase (assertEqual "for: iSort2 []"
        [] (iSort2 []))

-- | Test case: singleton list
test2 = TestCase (assertEqual "for: iSort2 [5]"
        [5] (iSort2 [5]))

-- | Test case: all duplicates
test3 = TestCase (assertEqual "for: iSort2 [3,3,3,3]"
        [3] (iSort2 [3,3,3,3]))

-- | Test case: already sorted with duplicates
test4 = TestCase (assertEqual "for: iSort2 [1,2,2,3,4,4,5]"
        [1,2,3,4,5] (iSort2 [1,2,2,3,4,4,5]))

-- | Test case: unsorted with duplicates
test5 = TestCase (assertEqual "for: iSort2 [4,1,3,2,2,4,1]"
        [1,2,3,4] (iSort2 [4,1,3,2,2,4,1]))

-- | Test case: descending order with duplicates
test6 = TestCase (assertEqual "for: iSort2 [9,8,8,7,6,6,5]"
        [5,6,7,8,9] (iSort2 [9,8,8,7,6,6,5]))

-- | Test case: negative numbers with duplicates
test7 = TestCase (assertEqual "for: iSort2 [0,-1,-2,-1,\
        \3,2,2]" [-2,-1,0,2,3] (iSort2 [0,-1,-2,-1,3,2,2]))

-- | Test case: mixed wide range with duplicates
test8 = TestCase (assertEqual "for: iSort2 [100,1,50,100,\
        \-50,0,-50]" [-50,0,1,50,100]
        (iSort2 [100,1,50,100,-50,0,-50]))


-----------------------------------------------------------

{-

Testing in GHCi

ghci> :set -package HUnit
ghci> :load Ex7_18
ghci> runTestTT tests
Cases: 8  Tried: 8  Errors: 0  Failures: 0
Counts {cases = 8, tried = 8, errors = 0, failures = 0}

-}