import Prelude hiding (take)

-----------------------------------------------------------
-- Exercise 7.21

-- | Returns the first n elements of a list
take :: Int -> [a] -> [a]
take 0 _      = []
take _ []     = []
take n (x : xs) 
  | n > 0     = x : take (n-1) xs
  | otherwise = error "take: negative argument"

-- | Returns an error if n is negative, also when the list 
-- is empty
take' :: Int -> [a] -> [a]
take' n _ | n < 0 = error "take': negative argument"
take' 0 _         = []
take' _ []        = []
take' n (x : xs)  = x : take' (n-1) xs


-----------------------------------------------------------

{-

If we call: take (-3) [], the result is []. To fix this, 
we move the guard for negative n to the top, so that it is
checked first. Pattern matching is sequential, so the first
pattern that matches is used. This is what we have done in
the definition of take'. 

Testing in GHCi

ghci> :load Ex7_21
ghci> take (-3) []
[]
ghci> take' (-3) []
*** Exception: take': negative argument

-}