-----------------------------------------------------------
-- Exercise 7.1

{-
  We are given the list comprehension:
  
      [f x | x <- xs, p x]
  
  This expression builds a list by:
    1. Taking each element x from the list xs
    2. Filtering out elements for which the predicate p x 
        is False
    3. Applying the function f to the remaining elements

  In other words, it maps f over all elements of the list 
  xs that satisfy the predicate p.

  The same functionality can thus also be accomplished by:

      map f (filter p xs)

  So we can see that list comprehensions in Haskell are, 
  in fact, syntactic sugar for combinations of map and 
  filter.

  Example:

  f = (*2)
  p = even
  xs [1..5]

  Then we have:
    [f x | x <- xs, p x]

  = [(*2) x | x <- [1..5], even x]

  = [4,8]

  This is the same as:

    map (*2) (filter even [1..5])

-}

-----------------------------------------------------------