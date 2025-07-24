import Prelude hiding (map, filter)

-----------------------------------------------------------
-- Exercise 7.3

-- definition of map using foldr
map :: (a -> b) -> [a] -> [b]
map f = foldr (\x acc -> f x : acc) []

-- definition op filter using foldr
filter :: (a -> Bool) -> [a] -> [a]
filter p = foldr (\x acc -> if p x then x:acc else acc) []

-----------------------------------------------------------

{-
  We have: foldr λ v

  foldr traverses the input list from left to right 
  (starting at the head). At each step, it applies the
  function λ to the current element and the result of 
  folding the rest of the list.

  This builds a right-associative (right-nested) 
  expression:

      foldr λ v [a,b,c,d]

    = a `λ` (b `λ` (c `λ` (d `λ` v)))

  So while the list is traversed left to right, the
  construction of the result expression happens right 
  to left.

  Example with map (λ x acc = f x : acc, v = []) as
  defined above and a list [a, b, c, d]:

      f a : (f b : (f c : (f d : [])))

  Note how the function f is applied to each element
  before being prepended to the accumulator. This is why
  foldr is often used for operations that need to
  process elements in a right-associative manner and why
  it is ideal for constructing new lists in the same order 
  as the input. It is also why λ takes the current element 
  as its first argument and the accumulator as its second.

  It is important to realize that foldr can 
  short-circuit if λ is lazy in its second argument 
  (like (&&), (||), ..). This allows early termination 
  and working with infinite lists.

  Summary
  
  foldr is preffered for:
  - right-associative construction
  - short-circuiting behavior
  - lazy evaluation over potentially infinite lists

-}