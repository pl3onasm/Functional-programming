-----------------------------------------------------------
-- Exercise 4.7

-- Given the definition for mult:
--    mult :: Int -> Int -> Int -> Int
--    mult x y z = x * y * z
-- this looks like a function of three arguments, but 
-- in Haskell, functions are curried by default.
-- We can make this explicit by using nested lambdas.
mult :: Int -> Int -> Int -> Int
mult = \x -> (\y -> (\z -> x * y * z))

-----------------------------------------------------------

{-
  It shows how currying works: mult takes an integer x,
  then returns a function that takes an integer y,
  which in turn returns another function that takes an
  integer z, and which computes the product x * y * z.
-}