-----------------------------------------------------------
-- Exercise 4.2

-- returns the third element in the input list, using the
-- functions head and tail
third :: [a] -> a
third xs = head (tail (tail xs))

-- same, but using the index operator. Note that the index
-- operator starts counting from 0, so to get the third
-- element, we need to grab the element at index 2
third' xs = xs !! 2

-- same, but using pattern matching
third'' (_ : _ : x : _) = x

-----------------------------------------------------------

{- Examples:

  ghci> :l ex4-2.hs
  ghci> third ['h','e','l','l','o','w','o','r','l','d']
  'l'
  ghci> third [0,1,2,3,4,5]
  2

-}