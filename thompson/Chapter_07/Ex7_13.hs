import Test.HUnit

-----------------------------------------------------------
-- Exercise 7.13

-- | Inserts an element into a sorted list
ins :: Integer -> [Integer] -> [Integer]
ins x [] = [x]
ins x (y : ys)
  | x <= y    = x : (y : ys)
  | otherwise = y : ins x ys

-- | Unit tests for ins
tests :: Test
tests = TestList [test1, test2, test3, test4, test5,
                  test6, test7, test8, test9, test10,
                  test11, test12, test13]

-- | Test case: inserting into an empty list
test1 = TestCase (assertEqual "for: ins 3 []" 
          [3] (ins 3 []))

-- | Test cases: inserting into a singleton list
test2 = TestCase (assertEqual "for: ins 5 [3]" 
          [3,5] (ins 5 [3]))
test3 = TestCase (assertEqual "for: ins 2 [3]" 
          [2,3] (ins 2 [3]))
test4 = TestCase (assertEqual "for: ins 3 [3]" 
          [3,3] (ins 3 [3]))

-- | Test cases: inserting at different positions
test5 = TestCase (assertEqual "for: ins 4 [1,3,5]" 
          [1,3,4,5] (ins 4 [1,3,5]))
test6 = TestCase (assertEqual "for: ins 0 [1,3,5]" 
          [0,1,3,5] (ins 0 [1,3,5]))
test7 = TestCase (assertEqual "for: ins 6 [1,3,5]" 
          [1,3,5,6] (ins 6 [1,3,5]))

-- | Test cases: inserting duplicates
test8 = TestCase (assertEqual "for: ins 3 [1,3,5]" 
          [1,3,3,5] (ins 3 [1,3,5]))
test9 = TestCase (assertEqual "for: ins 5 [1,3,5]" 
          [1,3,5,5] (ins 5 [1,3,5]))
test10 = TestCase (assertEqual "for: ins 1 [1,3,5]" 
          [1,1,3,5] (ins 1 [1,3,5]))

-- | Test cases: inserting netagive numbers
test11 = TestCase (assertEqual "for: ins (-2) [1,3,5]" 
          [-2,1,3,5] (ins (-2) [1,3,5]))
test12 = TestCase (assertEqual "for: ins 3 [-1,0,1]" 
          [-1,0,1,3] (ins 3 [-1,0,1]))
test13 = TestCase (assertEqual "for: ins 0 [-3,-2,-1]" 
          [-3,-2,-1,0] (ins 0 [-3,-2,-1]))


-----------------------------------------------------------

{-

Testing in GHCi

ghci> :set -package HUnit
ghci> :load Ex7_13
ghci> runTestTT tests
Cases: 13  Tried: 13  Errors: 0  Failures: 0
Counts {cases = 13, tried = 13, errors = 0, failures = 0}

-}