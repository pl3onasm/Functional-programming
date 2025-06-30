-----------------------------------------------------------
-- Exercise 8.3

-- Rotates the input triple to the left
rotateLeft :: (a, b, c) -> (b, c, a)
rotateLeft (x, y, z) = (y, z, x)  

-----------------------------------------------------------
-- Exercise 8.4

-- Recursively sorts a 4-tuple in ascending order
sort4 :: (Ord a) => (a, a, a, a) -> (a, a, a, a)
sort4 (a, b, c, d)
  | a <= b && b <= c && c <= d = (a, b, c, d)
  | a > b = sort4 (b, a, c, d)
  | b > c = sort4 (a, c, b, d)
  | otherwise = sort4 (a, b, d, c)

-----------------------------------------------------------
-- Exercise 8.5

-- We use two functions as defined in the notes

-- Constructor for a fraction
mkFrac :: Int -> Int -> (Int, Int)
mkFrac m n | n /= 0 = (m, n)
           | otherwise = error "Denominator cannot be zero"
           -- a safer approach would be to use Maybe 

-- Returns the fraction in simplified form
simplFrac :: (Int, Int) -> (Int, Int)
simplFrac (a, b) = (a `div` d, b `div` d)
  where d = gcd a b

-- Converts a fraction to a floating point
fracToFloat :: (RealFrac a) => (Int, Int) -> a
fracToFloat (a, b) = fromIntegral a 
                   / fromIntegral b

-- Converts a floating point to a fraction
floatToFrac :: (RealFrac a) => a -> (Int, Int) 
floatToFrac x = simplFrac (round (x * factor), 
                           round factor)
  where factor = 10000 

-----------------------------------------------------------
-- Exercise 8.6

-- We use a wrapper type to represent complex numbers,
-- just like we did for fractions. For a more idiomatic
-- approach, see exercise 15.1

-- Constructor for a complex number 
mkComp :: (Num a) => a -> a -> (a, a)
mkComp r i = (r, i)

-- Adds two complex numbers
addComp :: (Num a) => (a, a) -> (a, a) -> (a, a)
addComp (r1, i1) (r2, i2) = (r1 + r2, i1 + i2)

-- Multiplies two complex numbers
multComp :: (Num a) => (a, a) -> (a, a) -> (a, a)
multComp (r1, i1) (r2, i2) = (r1 * r2 - i1 * i2, 
                              r1 * i2 + i1 * r2)

-- Subtracts two complex numbers
subComp :: (Num a) => (a, a) -> (a, a) -> (a, a)
subComp (r1, i1) (r2, i2) = (r1 - r2, i1 - i2)

-- Divides two complex numbers
divComp :: (Fractional a) => (a, a) -> (a, a) -> (a, a)
divComp (r1, i1) (r2, i2) = 
  let denom = r2^2 + i2^2
  in ( (r1 * r2 + i1 * i2) / denom,
       (i1 * r2 - r1 * i2) / denom )

-----------------------------------------------------------
