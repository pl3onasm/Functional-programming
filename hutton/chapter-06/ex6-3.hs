import Prelude hiding (^)

-----------------------------------------------------------
-- Exercise 6.3

-- implements the exponentiation operator (^)
(^) :: Int -> Int -> Int
x ^ 0 = 1
x ^ n = x * (x ^ (n - 1))

-----------------------------------------------------------

{-
  Exponentiation is implemented as repeated multiplication.
  The base case is when the exponent is 0, in which case
  the result is 1, the identity of multiplication.
  The recursive case multiplies the base x by the result
  of raising x to the power of (n - 1).

  Evaluation of the expression 2 ^ 3 would proceed as 
  follows:

      2 ^ 3
  =     { applying ^ }
      2 * (2 ^ 2)
  =     { applying ^ }
      2 * (2 * (2 ^ 1))
  =     { applying ^ }
      2 * (2 * (2 * (2 ^ 0)))
  =     { applying ^ }
      2 * (2 * (2 * 1))
  =     { applying * }
      2 * (2 * 2)
  =     { applying * }
      2 * 4
  =     { applying * }
      8

-}







