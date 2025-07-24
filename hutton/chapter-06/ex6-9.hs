-----------------------------------------------------------
-- Exercise 6.9

{- 
  Five-step process for developing a recursive function:
    1. Identify the type of the function
    2. Enumerate the cases
    3. Define the base case(s)
    4. Define the other case(s)
    5. Generalize and simplify
-}

{-  
  Developing a function that takes a list of numbers
  and returns the sum of the numbers in the list

  1. We start with the type sum :: [Int] -> Int
     - The input is a list of integers
     - The output is a single integer: the sum

  2. The cases follow from the structure of lists:
     - empty list
     - non-empty list

  3. The base case is the empty list, which has a sum of 0,
     since 0 is the identity for addition
     So: sum [] = 0

  4. The other case is a non-empty list, which has a sum
     that is the first element plus the sum of the rest of
     the list. 
     So: sum (x : xs) = x + sum xs

  5. Realizing that addition is defined for polymorphic
     numeric types, we can generalize the type to
     sum :: Num a => [a] -> a
-}

sum :: Num a => [a] -> a
sum [] = 0
sum (x : xs) = x + sum xs

{- 
  Developing a recursive function that takes an integer n
  and a list, and returns the first n elements of the list

  1. We start with a simple type, so we can work with 
     an example in mind. We take a list of integers 
     and an integer n, and return a list of the first
     n integers of the original list.
     So: take :: Int -> [Int] -> [Int]

  2. The cases are:
     - n is 0
     - the list to take from is empty
     - n is greater than 0 and the list is non-empty

  3. The base cases, that is, the cases where the recursion
     stops, are:
      - if n is 0, we take zero elements from the list,
        so that the result is the empty list
        So: take 0 xs = []
      - if the list is empty, the result is also the empty
        list, since we cannot take any elements from it
        So: take n [] = []

  4. The other case is when n is greater than 0 and the
     list is non-empty. In this case, we take the first
     element of the list and prepend it to the result of
     taking the first (n - 1) elements of the rest of the
     list.
     So: take n (x : xs) = x : take (n - 1) xs  

  5. We realize that we do not need to restrict the list 
     type to Int, since we do not use any operations that
     require the elements to be of type Int, or even of
     a numeric type. We can generalize the type to
     take :: Int -> [a] -> [a]

     We can also simplify the first two equations by using
     a wildcard pattern, as the body of the equations
     does not depend on the value of the list or n.

      take 0 _ = []
      take _ [] = []
-}

take :: Int -> [a] -> [a]
take 0 _ = []
take _ [] = []
take n (x : xs) = x : take (n - 1) xs

{-
  Developing a recursive function to select the last
  element of a *non-empty* list

  1. Let us start with a simple type. We take a list of
     integers and return the last integer of the list.
     So: last :: [Int] -> Int

  2. Since we are given that the input list is non-empty,
     the cases are:
     - the list has one element
     - the list has more than one element

  3. The base case is when the list has one element, in
     which case the last element is that element itself.
     So: last [x] = x

  4. The other case is when the list has more than one
     element. In this case, the last element is the last
     element of the tail of the list.
     So: last (x : xs) = last xs

  5. We realize that we do not need to restrict the
     list type to Int, since we do not use any operations
     that require the elements to be of type Int, or even
     of a numeric type. We can generalize the type to
     last :: [a] -> a
-}

last :: [a] -> a
last [x] = x
last (x : xs) = last xs

-----------------------------------------------------------