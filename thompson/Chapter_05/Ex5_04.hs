import Chapter_05.Ex5_01
import Chapter_05.Ex5_02
import Chapter_05.Ex5_03

import Test.HUnit

-----------------------------------------------------------
-- Exercise 5.4

-- | Tests for Ex5_01
testEx5_01 :: Test
testEx5_01 = TestList [
  TestCase (assertEqual "for: maxThreeOccurs 5 5 5"
            (5, 3) (maxThreeOccurs 5 5 5)),
  TestCase (assertEqual "for: maxThreeOccurs 5 3 5"
            (5, 2) (maxThreeOccurs 5 3 5)),
  TestCase (assertEqual "for: maxThreeOccurs 3 5 5"
            (5, 2) (maxThreeOccurs 3 5 5)),
  TestCase (assertEqual "for: maxThreeOccurs 5 5 3"
            (5, 2) (maxThreeOccurs 5 5 3)),
  TestCase (assertEqual "for: maxThreeOccurs 5 3 3"
            (5, 1) (maxThreeOccurs 5 3 3)),
  TestCase (assertEqual "for: maxThreeOccurs 3 5 3"
            (5, 1) (maxThreeOccurs 3 5 3)),
  TestCase (assertEqual "for: maxThreeOccurs 3 3 5"
            (5, 1) (maxThreeOccurs 3 3 5)),
  TestCase (assertEqual "for: maxThreeOccurs 1 2 3"
            (3, 1) (maxThreeOccurs 1 2 3)),
  TestCase (assertEqual "for: maxThreeOccurs -1 -2 -3"
            (-1, 1) (maxThreeOccurs (-1) (-2) (-3)))
  ]

-- | Tests for Ex5_02
testEx5_02 :: Test
testEx5_02 = TestList [
  TestCase (assertEqual "for: orderTriple 7 5 3"
            (3, 5, 7) (orderTriple 7 5 3)),
  TestCase (assertEqual "for: orderTriple 7 3 5"
            (3, 5, 7) (orderTriple 7 3 5)),
  TestCase (assertEqual "for: orderTriple 5 7 3"
            (3, 5, 7) (orderTriple 5 7 3)),
  TestCase (assertEqual "for: orderTriple 5 3 7"
            (3, 5, 7) (orderTriple 5 3 7)),
  TestCase (assertEqual "for: orderTriple 3 7 5"
            (3, 5, 7) (orderTriple 3 7 5)),
  TestCase (assertEqual "for: orderTriple 3 5 7"
            (3, 5, 7) (orderTriple 3 5 7)),
  TestCase (assertEqual "for: orderTriple 5 5 7"
            (5, 5, 7) (orderTriple 5 5 7)),
  TestCase (assertEqual "for: orderTriple 5 7 5"
            (5, 5, 7) (orderTriple 5 7 5)),
  TestCase (assertEqual "for: orderTriple 7 5 5"
            (5, 5, 7) (orderTriple 7 5 5)),
  TestCase (assertEqual "for: orderTriple 5 5 5"
            (5, 5, 5) (orderTriple 5 5 5)),
  TestCase (assertEqual "for: orderTriple -1 0 1"
            (-1, 0, 1) (orderTriple (-1) 0 1)),
  TestCase (assertEqual "for: orderTriple 1 0 -1"
            (-1, 0, 1) (orderTriple 1 0 (-1))),
  TestCase (assertEqual "for: orderTriple 0 -1 1"
            (-1, 0, 1) (orderTriple 0 (-1) 1)),
  TestCase (assertEqual "for: orderTriple 0 -1 -1"
            (-1, -1, 0) (orderTriple 0 (-1) (-1))),
  TestCase (assertEqual "for: orderTriple -1 0 -1"
            (-1, -1, 0) (orderTriple (-1) 0 (-1))),
  TestCase (assertEqual "for: orderTriple -1 -1 0"
            (-1, -1, 0) (orderTriple (-1) (-1) 0)),
  TestCase (assertEqual "for: orderTriple 0 0 -1"
            (-1, 0, 0) (orderTriple 0 0 (-1))),
  TestCase (assertEqual "for: orderTriple 0 -1 0"
            (-1, 0, 0) (orderTriple 0 (-1) 0)),
  TestCase (assertEqual "for: orderTriple -1 -2 -3"
            (-3, -2, -1) (orderTriple (-1) (-2) (-3)))
  ]

testEx5_03 :: Test
testEx5_03 = TestList [
  TestCase (assertEqual "for: xIntercept 2 4"
            (-2.0, True) (xIntercept 2 4)),
  TestCase (assertEqual "for: xIntercept 4 2"
            (-0.5, True) (xIntercept 4 2)),
  TestCase (assertEqual "for: xIntercept -2 -4"
            (-2.0, True) (xIntercept (-2) (-4))),
  TestCase (assertEqual "for: xIntercept (-2) 4"
            (2.0, True) (xIntercept (-2) 4)),
  TestCase (assertEqual "for: xIntercept 2 (-4)"
            (2.0, True) (xIntercept 2 (-4))),
  TestCase (assertEqual "for: xIntercept 0 4"
            (0, False) (xIntercept 0 4)),
  TestCase (assertEqual "for: xIntercept 2 0"
            (0.0, True) (xIntercept 2 0)),
  TestCase (assertEqual "for: xIntercept 0 0"
            (0, False) (xIntercept 0 0))
  ]

-----------------------------------------------------------

{-

Testing in GHCi

ghci> :set -i..
ghci> :set -package HUnit
ghci> :load Ex5_04

ghci> runTestTT testEx5_01
Cases: 9  Tried: 9  Errors: 0  Failures: 0
Counts {cases = 9, tried = 9, errors = 0, failures = 0}


ghci> runTestTT testEx5_02
### Failure in: 0
Ex5_04.hs:36
for: orderTriple 7 5 3
expected: (3,5,7)
 but got: (3,3,7)
### Failure in: 2
Ex5_04.hs:40
for: orderTriple 5 7 3
expected: (3,5,7)
 but got: (3,3,7)
### Failure in: 11
Ex5_04.hs:58
for: orderTriple 1 0 -1
expected: (-1,0,1)
 but got: (-1,-1,1)
### Failure in: 16
Ex5_04.hs:68
for: orderTriple 0 0 -1
expected: (-1,0,0)
 but got: (-1,-1,0)
### Failure in: 18
Ex5_04.hs:72
for: orderTriple -1 -2 -3
expected: (-3,-2,-1)
 but got: (-3,-3,-1)
Cases: 19  Tried: 19  Errors: 0  Failures: 5

These results revealed bugs in the orderTriple function,
defined in Ex5_02.hs. From the reported failures it 
was clear that it was always the middle number that was 
incorrect. In fact, these errors were due to an incorrect 
implementation of the middleNumber function in Ex4_02.hs, 
where I first only considered the ascending order 
x <= y <= z, and forgot to also check the descending order 
z <= y <= x. So, thanks to these comprehensive tests which 
cover all permutations of the inputs, it was immediately
clear where to look for the bug and how to fix it.

After fixing the code, all tests pass:
ghci> runTestTT testEx5_02
Cases: 19  Tried: 19  Errors: 0  Failures: 0
Counts {cases = 19, tried = 19, errors = 0, failures = 0}


ghci> runTestTT testEx5_03
Cases: 8  Tried: 8  Errors: 0  Failures: 0
Counts {cases = 8, tried = 8, errors = 0, failures = 0}


-}