-------------------------------------------------
-- Exercise 6.1

-- Alternative implementation of prime checker
isPrime :: Int -> Bool
isPrime n = aux (n - 1) 
  where
    aux :: Int -> Bool
    aux 0 = False
    aux 1 = True
    aux k = n `mod` k /= 0 && aux (k - 1)

-- Optimized version of isPrime
isPrime' :: Int -> Bool
isPrime' n
  | n < 2 = False
  | n == 2 = True
  | even n = False
  | otherwise = aux 3
  where
    aux :: Int -> Bool
    aux k
      | k * k > n = True
      | n `mod` k == 0 = False
      | otherwise = aux (k + 2)

-------------------------------------------------
-- Exercise 6.2

{- Iterative implementation of prime checker
   in pseudo-code

   isPrimeIterative(n):
     if n < 2 then return False
     for i from n-1 down to 2 do
       if n mod i = 0 then return False
     return True 
-}

-------------------------------------------------
-- Exercise 6.3

-- Recursive definition of power function
power :: Int -> Int -> Int
power _ 0 = 1
power n m = n * power n (m - 1)

-------------------------------------------------
-- Exercise 6.4

-- Returns the number of ones in n's 
-- binary representation
countOnes :: Int -> Int
countOnes 0 = 0
countOnes n = (n `mod` 2) + countOnes (n `div` 2)

-- Optimized version of countOnes by using
-- an accumulator (tail recursion is much more
-- efficient due to minimal stack usage)
countOnes' :: Int -> Int
countOnes' n = aux n 0
  where
    aux :: Int -> Int -> Int
    aux 0 ac = ac
    aux k ac = aux (k `div` 2) (ac + (k `mod` 2))

-------------------------------------------------
-- Exercise 6.5

-- Cheks if a number is perfect
isPerfect :: Int -> Bool
isPerfect n = n > 0 && n == aux (n - 1)
  where
    aux :: Int -> Int
    aux 0 = 0
    aux k
      | n `mod` k /= 0 = aux (k - 1)
      | otherwise = k + aux (k - 1)

-- Optimized version of isPerfect (can be
-- optimized even more by using an accumulator)
isPerfect' :: Int -> Bool
isPerfect' n = n >= 6 &&  -- 6 is smallest pf num
             let k = floor $sqrt $fromIntegral n
             in  n == aux k
  where
    aux :: Int -> Int
    aux 1 = 1
    aux k
      | n `mod` k /= 0 = aux (k - 1)
      | otherwise = 
          let d = n `div` k
          in  k + (if d == k then 0 else d) 
              + aux (k - 1)

-------------------------------------------------
-- Exercise 6.6

-- Finds the smallest prime number >= n
nextPrime :: Int -> Int
nextPrime n
  | n < 2 = 2
  | isPrime' n = n
  | otherwise = nextPrime (n + 1)

-------------------------------------------------
