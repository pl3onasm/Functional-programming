-----------------------------------------------------------
-- Exercise 13.1

-- Defines predecessor as a specialiazation of (+)
predr :: Num a => a -> a
predr = (+) (-1)

-- This is short for the same function
-- predecessor = \x -> x + (-1)

-- Alternatively:
predr' :: Num a => a -> a
predr' = (-1 +)

{- The first version of predecessor is obtained by partial
   application of the binary operator (+): we apply the
   function (+) to the first argument -1, and get back a
   function that takes one argument x and returns x + (-1).
   This works because all functions in Haskell are curried.

   The last version of predecessor is defined using an
   operator section, which is special syntax that turns a
   binary operator into a unary function by fixing one of
   its arguments. In this case, we fix the first argument
   to -1, and get a function that takes x and returns
   -1 + x.
-}

-----------------------------------------------------------
-- Exercise 13.2

-- Defines the function negate as a specialization of (-)
neg :: Num a => a -> a
neg = (-) 0

-- This is short for:
-- negate = \x -> 0 - x

-- Alternatively, we can define it using a section:
neg' :: Num a => a -> a
neg' = (0 -)

-----------------------------------------------------------
-- Exercise 13.3

-- Defines recip as a specialization of (/)
recp :: Fractional a => a -> a
recp = (/) 1

-- This is short for:
-- recip = \x -> 1 / x

-- Alternatively:
recp' :: Fractional a => a -> a
recp' = (1 /)