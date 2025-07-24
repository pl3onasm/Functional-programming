import Data.Char

-----------------------------------------------------------
-- Exercise 7.4

-- converts a list of digits into an integer
dig2int :: [Int] -> Int
dig2int = foldl (\acc x -> 10*acc + x) 0

-- converts a decimal number given as a string into an 
-- integer
dec2int :: String -> Int 
dec2int = foldl (\acc x -> 10*acc + ord x - ord '0') 0

-----------------------------------------------------------

{-
  We have: foldl λ v

  Like foldr, foldl traverses the input list from left to 
  right (starting at the head). The difference lies in how 
  the result expression is built: foldl constructs a 
  left-associative (left-nested) expression.

      foldl λ v [a, b, c, d]

    = (((v `λ` a) `λ` b) `λ` c) `λ` d

  At each step, the accumulator is combined with the 
  current element using the function λ, and the result 
  becomes the new accumulator passed into the next step.

  Example with dig2int (λ acc x = 10 * acc + x, v = 0) 
  and list [a, b, c, d]:

      10 * (10 * (10 * (10 * 0 + a) + b) + c) + d

  This evaluates to:

      a * 1000 + b * 100 + c * 10 + d


  Note the contrast with foldr:
  - foldr builds the expression *from the right*, nesting
    the function applications outward from the end of the 
    list
  - foldl builds the expression *from the left*, nesting 
    inward from the beginning of the list

  Another key difference is strictness:
  foldl always traverses the entire list before producing
  a result. It does NOT short-circuit, even if the result
  could be known earlier. Therefore, foldl does not work 
  well on infinite lists. However, it is ideal for strict 
  accumulation of numeric or aggregated values in finite 
  lists, such as computing sums, products, lengths, or 
  constructing values like numbers from digits.

  In practice, to avoid building up large chains of 
  unevaluated expressions (called thunks), it is often 
  better to use the strict version foldl', especially for 
  numerical accumulation. This ensures that the 
  intermediate results are evaluated immediately,
  preventing stack overflow or excessive memory usage.

  Summary
  
  Use foldl (or foldl') for:
  - left-associative accumulation
  - stepwise aggregation from the front of the list
  - you are processing finite lists
  - the operator is strict (like (+), (*), etc.)

-}