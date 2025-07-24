-----------------------------------------------------------
-- Exercise 5.8

-- takes a key k and a list of key-value pairs, returns the 
-- list of all values associated with k 
find :: Eq a => a -> [(a,b)] -> [b]
find k t = [v | (k',v) <- t, k == k']

-- takes an input value x and a list xs having values of  
-- the same type, returns a list of indices where x occurs  
positions :: Eq a => a -> [a] -> [Int]
positions x xs = [i | (x',i) <- zip xs [0..], x == x']

-- redefines the given function `positions` using the 
-- function `find`
positions' :: Eq a => a -> [a] -> [Int]
positions' x xs = find x (zip xs [0..])

-----------------------------------------------------------

{- Examples

  ghci> positions' True (replicate 10 True)
  [0,1,2,3,4,5,6,7,8,9]

  ghci> positions' '1' "10001110001010101"
  [0,4,5,6,10,12,14,16]

-}