-----------------------------------------------------------
-- Exercise 5.20

divisors :: Integer -> [Integer]
divisors n = [d | d <- [1..n], n `mod` d == 0]

isPrime :: Integer -> Bool
isPrime n = divisors n == [1,n]


-----------------------------------------------------------

{-

The first function, divisors, takes an integer n and 
returns a list of all its divisors by checking each integer
d from 1 to n to see if it divides n evenly.

The second function, isPrime, checks if n is prime by
comparing the list of its divisors to the list [1,n]. 
If they are equal, n has no divisors other than 1 and 
n itself, so it is prime.

-}