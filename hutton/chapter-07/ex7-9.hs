-----------------------------------------------------------
-- Exercise 7.9

-- takes two functions of the same typing and an input list
-- generates a new list by alternately applying the two
-- functions to the elements in the input
altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap f g xs = 
  [if even i then f x else g x | (x, i) <- zip xs [0..]]

-- point-free definition:
altMap' :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap' f g = 
  zipWith (\i x -> if even i then f x else g x) [0..]

-----------------------------------------------------------

{- Examples:

ghci> altMap (+1) (*2) [1,2,3,4]
[2,4,4,8]

ghci> altMap' (+10) (+100) [0,1,2,3,4]
[10,101,12,103,14]

-}