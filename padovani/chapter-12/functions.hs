import Data.List (nub)

-----------------------------------------------------------
-- Exercise 12.1

apply :: (a -> b) -> a -> b
apply = id

{- The apparent absurdity of this definition lies in the
   fact that it does not take any arguments. It seems to 
   treat functions as values that can simply be passed 
   around. Moreover, it does not seem to perform any
   computation: `apply` is defined as `id`, the identity
   function, which just returns its argument unchanged.
   So, is applying the same as doing nothing at all?
   
   This does not seem to make sense. After all, the
   functions have different types:

      apply :: (a → b) → a → b
      id   :: c → c
  
   However, this mismatch disappears once we remember that
   the arrow → is right-associative. So we can rewrite
   the type of `apply` as:

      apply :: (a → b) → (a → b)

   Now the definition makes more sense: `apply` is a
   function that takes a function of type (a → b) and 
   returns that same function unchanged. It is essentially 
   `id`, but used at the level of functions, not values.

   In fact, the type variable c in `id :: c → c` is 
   instantiated as (a → b), making the definition valid.
   
   The absurdity is only apparent, and results from the 
   fact that in curried form, the function application:

      apply f x

   is really:

      (apply f) x

   and since `apply = id`, we get:

      (id f) x = f x

   So the behavior of `apply` is exactly what we expect.
-}

-----------------------------------------------------------
-- Exercise 12.2

-- Takes a function and a number n, and returns a list of
-- length n in which the i-th element is the function f
-- applied i times, i.e. f⁽ⁱ⁾ for i = 0, 1, ..., n-1.
-- In other words, the function returns the first n
-- iterates of the function f.
makeList :: (a -> a) -> Int -> [a -> a]
makeList _ 0 = []
makeList f 1 = [id]
makeList f n = 
  let fs = makeList f (n - 1)
      g  = last fs
  in fs ++ [f . g]

{- This is a recursive definition of `makeList`. The base
   case is when n is 0, in which case it returns the
   empty list. For n = 1, it returns a list containing
   only the identity function (`id`, which is f⁽⁰⁾). For 
   n > 1, it constructs the list by recursively calling 
   `makeList` with n - 1, and then appending a new 
   function that applies `f` to the last function in the 
   list (which is f⁽ⁿ⁻¹⁾).

   A possible test case is:

    map ($ 2) (makeList (^2) 5)

   resulting in a list of functions that apply the
   squaring function 0 to 4 times to the number 2, so
   that the output is:

    [2, 4, 16, 256, 65536]
-}

-----------------------------------------------------------
-- Exercise 12.3

-- Same as before, but using map
makeList2 :: (a -> a) -> Int -> [a -> a]
makeList2 _ 0 = []
makeList2 f n = id : map (f .) (makeList2 f (n - 1))

{- This is a recursive definition of `makeList`. The base
   case is when n is 0, in which case it returns an
   empty list. For n > 0, it constructs the list by
   prepending the identity function (`id`, which is f⁽⁰⁾) 
   to the result of recursively calling `makeList2` with 
   n - 1, and mapping the function f over that list, 
   effectively creating a list of functions where each 
   function is the previous one composed with f.
-}

-- As yet another alternative, we could use `iterate`.
-- Even though we have not yet seen infinite lists at
-- this point.
makeList3 :: (a -> a) -> Int -> [a -> a]
makeList3 f n = take n $ iterate (f .) id

{- The function `makeList3` uses `iterate` to generate an
   infinite list of functions, where each function is the
   result of applying f to the previous function. The
   first element is the identity function, and each
   subsequent element is the previous one composed with 
   f. The `take n` part limits the list to the first n 
   elements.
   Note that the test case for the previous exercise
   returns the same result for both `makeList2` and
   `makeList3`.
-}

-----------------------------------------------------------
-- Exercise 12.4

-- Takes a predicate p and a list, and returns the longest
-- prefix of the list that satisfies the predicate.
takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' p [] = []
takeWhile' p (x : xs)
   | p x = x : takeWhile' p xs
   | otherwise = []

-- Alternatively, we can use foldr:
tkWhile :: (a -> Bool) -> [a] -> [a]
tkWhile p = foldr (\x ac -> if p x then x : ac else []) []

{- The alternative definition using `foldr` works by
   folding the list with a function that checks each
   element against the predicate p. If an element 
   satisfies the predicate, it is included in the result;
   otherwise, an empty list is returned, effectively 
   halting further processing.

   Importantly, this does not mean that `foldr` starts
   traversing the list from the end. Rather, `foldr` 
   associates to the right but begins evaluating elements 
   from the front. Thanks to Haskell's laziness, this 
   allows early termination: the fold can stop as soon as 
   it encounters an element that does not satisfy the 
   predicate.

   A test case for both definitions would be:

      takeWhile' (< 3) [1, 2, 3, 4, 5]

   which returns:

      [1, 2]
-}

-----------------------------------------------------------
-- Exercise 12.5

-- Non-recursive function that takes a list of integers,
-- and verifies whether all numbers are even or not.
allEven :: [Int] -> Bool
allEven = all even

-- Alternatively, we can use `foldr`:
allEven' :: [Int] -> Bool
allEven' = foldr (\x acc -> even x && acc) True

{- The function `allEven` uses the `all` function from the
   Prelude, which checks if all elements in a list satisfy
   a given predicate (in this case, `even`). 

   The alternative definition uses `foldr` to traverse the
   from left to right, checking whether each element is 
   even. If an element is odd, the result becomes False 
   and further evaluation is short-circuited. The initial 
   accumulator is True, and the folding function combines 
   the result of `even x` with the accumulator using 
   logical AND (&&).
-}

-----------------------------------------------------------
-- Exercise 12.6

-- Non-recursive function that takes a list of tuples whose
-- first element is an integer, and verifies whether all
-- integers are even or not.
allEven2 :: [(Int,a)] -> Bool
allEven2 = all even . map fst

{- The function `allEven2` uses `map` to extract the first
   element of each tuple in the list. This results in a
   list of integers, which is then passed to the `all`
   function to check if all integers are even.  

   The use of function composition (.) allows us to
   combine the two operations into a single function.
-}

-----------------------------------------------------------
-- Exercise 12.7

-- Non-recursive function that removes the last element
-- from a list, if it exists.
rmLast :: [a] -> [a]
rmLast = reverse . drop 1 . reverse

{- The function `rmLast` works by reversing the list,
   dropping the first element (which is the last element
   of the original list), and then reversing it back to
   restore the original order without the last element.
-}

-- Alternatively, we can use `init`:
rmLast' :: [a] -> [a]
rmLast' = init

-- However, this is not a good idea, as it will throw an 
-- error if the list is empty. So we should define a safe 
-- version:
rmLastSafe :: [a] -> [a]
rmLastSafe [] = []
rmLastSafe xs = init xs

-----------------------------------------------------------
-- Exercise 12.8

-- Non-recursive function that takes a list and removes all  
-- elements that occur at odd indices (1-based indexing)
rmOddIdxElms :: [a] -> [a]
rmOddIdxElms xs = [x | (x, i) <- zip xs [1..], even i]

-- Alternatively, we can define it as follows:
rmOddIdxElms' :: [a] -> [a]
rmOddIdxElms' xs = map fst f 
   where f = filter (\(_, i) -> even i) (zip xs [1..])

{- The function `rmOddIdxElms` uses list comprehension to
   iterate over the list `xs` and its indices (starting 
   from 1). It constructs a new list containing only the
   elements at even indices. The `zip` function pairs each
   element with its index, and the filter condition checks
   if the index is even.      
   The alternative definition uses `filter` to keep only
   the pairs where the index is even, and then `map` to
   extract the first element of each pair, resulting in a
   list of elements at even indices.      
-}

-----------------------------------------------------------
-- Exercise 12.9

-- Non-recursive function to check whether the input list
-- is a palindrome
isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = xs == reverse xs

-----------------------------------------------------------
-- Exercise 12.10

-- Non-recursive function that takes a key and an 
-- associative list, and returns the list of all the values
-- associated with that key.
lookupAll :: Eq a => a -> [(a, b)] -> [b]
lookupAll key xs = [v | (k, v) <- xs, k == key] 

{- The function `lookupAll` uses list comprehension to
   iterate over the associative list xs, which is a list
   of pairs (tuples). It constructs a new list containing
   all values v where the key k matches the input key.
   This allows us to retrieve all values associated with a
   given key in the associative list.
-}

-----------------------------------------------------------
-- Exercise 12.11

-- Non-recursive function that takes a list, and returns a
-- frequency list, that is, a list where each element
-- is the number of occurrences of the corresponding
-- element in the input list.
freqList :: Eq a => [a] -> [Int]
freqList xs = [length (filter (== x) xs) | x <- xs]

{- The function `freqList` uses list comprehension to 
   iterate over the input list xs. For each element x, 
   it counts the number of occurrences of x in xs by
   using `filter` to keep only the elements equal to x
   and then calculating the length of that filtered list.

   A possible test case is:

      freqList [1, 0, 1, 2, 1, 2, 1, 1, 3, 2, 0, 2]

   which returns:

      [5,2,5,4,5,4,5,5,1,4,2,4]
-}

-- Alternatively, we can use `map`:
freqList' :: Eq a => [a] -> [Int]
freqList' xs = map (\x -> count x xs) xs
  where
    count x = length . filter (== x)

{- This definition does the same thing as the previous one,
   but uses `map` to apply a counting function to each
   element in the list. The counting function is defined
   in the same way as before, by computing the length of
   the filtered list where elements are equal to x.
-}

-----------------------------------------------------------
-- Exercise 12.12

-- Implementation of all, which determines whether all
-- elements in the input list satisfy a given predicate
all' :: (a -> Bool) -> [a] -> Bool
all' p = foldl (\acc x -> p x && acc) True 

{- This function uses `foldl` to traverse the list from
   left to right, checking whether each element satisfies
   the predicate p. The initial accumulator is True,
   and the folding function combines the result of p x
   with the accumulator using logical AND (&&). If any
   element does not satisfy the predicate, the result
   becomes False.
-}

-- Alternatively, we can use foldr:
all'' :: (a -> Bool) -> [a] -> Bool
all'' p = foldr (\x acc -> p x && acc) True

{- So what's the difference between these two definitions?
   Does it even matter which one we use?
   
   The difference lies in the evaluation behavior. The 
   first version is NOT short-circuiting: it constructs a 
   chain of deferred expressions (thunks) like
     (...((True && p x1) && p x2) && ...)
   and only evaluates the result once the end of the list
   is reached. This can be inefficient for large lists,
   or even non-terminating for infinite lists.

   In contrast, the `foldr` version DOES short-circuit due 
   to Haskell's lazy evaluation: in the expression
     p x && acc
   the second argument (`acc`) is not evaluated unless
   p x is True. So as soon as p x is False, the
   entire expression evaluates to False, and no further
   elements are processed.

   To convince yourself, try this test case with an 
   infinite list:

      all' even (2:3:[4..])
      all'' even (2:3:[4..])

   The first one hangs indefinitely, because it tries to
   traverse the whole list before evaluating. The second 
   one returns False immediately, since it encounters 
   3 and stops.

   Why does this happen? Let's look at the definitions:

   foldl :: (b → a → b) → a → [b] → a
   foldl _ acc [] = acc
   foldl f acc (x : xs) = foldl f (acc `f` x) xs

   foldr :: (a → b → b) → b → [a] → b
   foldr _ acc [] = acc
   foldr f acc (x : xs) = x `f` foldr f acc xs

   `foldl` generates (acc `f` x) at each step, building up
   a left-nested structure that accumulates as a big thunk 
   (deffered computation) until the end of the list is
   reached, because it needs the full accumulator in
   order to evaluate even the first step.

   `foldr`, on the other hand, allows partial evaluation
   *as it traverses the list*. The expression
     p x && acc
   is only as strict as needed: if p x is False, then
   the result is immediately False, and Haskell never
   evaluates acc.

   This illustrates that Haskell's laziness is not about
   deferring everything until the end, but rather about
   evaluating just enough to satisfy the outermost demand.
   With `foldr`, that means it can stop early. With 
   `foldl`, it must build the whole structure before it can
   give you an answer.
-}

-----------------------------------------------------------
-- Exercise 12.13

-- Non-recursive function that takes a list of functions
-- and returns a function that is the composition of
-- all the functions in the list in the order they appear.
composeAll :: [a -> a] -> (a -> a)
composeAll = foldr (\x acc -> x . acc) id

{- Note that we do not have the liberty to use foldl here
   (like we had in the previous exercise), since the 
   composition operator is right-associative (whereas &&
   is associative tout court). Using `foldl` would compose 
   the functions in reverse order, which would produce a 
   different result.
-}

-- The definition can be shortened to:
composeAll' :: [a -> a] -> (a -> a)
composeAll' = foldr (.) id

{- This definition uses the function composition operator
   (.) directly, which is more concise. The extra bit
   `(\x acc -> x . acc)` is not needed, since (.) is
   already doing the same thing: it takes two functions
   and returns their composition.

   The result is the same: it composes all functions in the
   list from right to left, starting with the identity 
   function as the base. 
-}

-----------------------------------------------------------
-- Exercise 12.14

-- Defines a filter as a combination of `foldr` and `map`
filter' :: (a -> Bool) -> [a] -> [a]
filter' p xs = foldr include [] (map (\x -> (x, p x)) xs)
  where
    include (x, t) acc = if t then x : acc else acc

-- Using the composition operator:
filter'' :: (a -> Bool) -> [a] -> [a]
filter'' p = foldr include [] . map (\x -> (x, p x))
  where
    include (x, t) acc = if t then x : acc else acc

-- Using `foldl` instead of `foldr`:
filterL :: (a -> Bool) -> [a] -> [a]
filterL p = foldl include [] . map (\x -> (x, p x))
  where
    include acc (x, t) = if t then acc ++ [x] else acc

{- The function `filter'` uses `foldr` to iterate over a
   list of pairs, where each pair consists of an element
   and a Boolean indicating whether it satisfies the
   predicate p. The helper function `include` checks the
   Boolean and either includes the element in the result
   list or skips it. The use of `foldr` preserves the 
   original order and supports short-circuiting (although 
   this version does not take advantage of that).

   The second version, uses function composition to express
   the same idea more concisely.

   The third definition, `filterL`, uses `foldl` instead of
   `foldr`. Since `foldl` processes the list from left to 
   right, and prepending would reverse the order, the 
   result is built using the append operator (++) to 
   preserve the original order. However, this comes at the
   cost of performance: using ++ inside a left fold 
   results in quadratic time complexity. A more efficient 
   version would use `x : acc` and reverse the result once 
   at the end.

   That said, none of the above definitions are 
   particularly efficient, since they use map to create a 
   list of pairs, which introduces an unnecessary traversal 
   and intermediate structure. A more efficient definition 
   would avoid map altogether and use only foldr:

   filter :: (a -> Bool) -> [a] -> [a]
   filter p = foldr (\x acc -> 
                     if p x then x : acc else acc) []
-}

-----------------------------------------------------------
-- Exercise 12.15

-- Function that takes an associative list (a list of
-- pairs) and returns a list of pairs where each key is
-- associated with a list of all values that were
-- associated with that key in the input list.
pack_assoc :: Eq k => [(k, v)] -> [(k, [v])]
pack_assoc = foldl insert []
  where
    insert [] (k, v) = [(k, [v])]
    insert ((k', vs) : xs) (k, v)
      | k == k'   = (k', vs ++ [v]) : xs
      | otherwise = (k', vs) : insert xs (k, v)

-- Function that takes an associative list (a list of
-- pairs where each key is associated with a list of
-- values) and returns a list of pairs where each key is
-- associated with a single value, effectively unpacking
-- the values from the lists.
unpack_assoc :: [(k, [v])] -> [(k, v)]
unpack_assoc = concatMap expand
  where
    expand (k, vs) = map (\v -> (k, v)) vs

-----------------------------------------------------------
-- Exercise 12.16

-- Computes a list of all the permutations of the elements
-- in the input list.
perms :: Eq a => [a] -> [[a]]
perms [] = [[]]
perms xs = [x : ys | x <- xs, ys <- perms (remove x xs)]
  where
    remove _ [] = []
    remove y (z : zs)
      | y == z    = zs
      | otherwise = z : remove y zs

-- The definition above preserves possible duplicates in 
-- the input list, so it generates all permutations of the
-- elements, including those that are not unique. 
-- If we want only the distinct permutations (as when 
-- treating the input as a multiset), we can filter out
-- duplicates *after* generating all permutations, by using
-- `nub` from Data.List:
uniquePerms :: Eq a => [a] -> [[a]]
uniquePerms = nub . perms

{- The function `perms` generates all permutations of a
   list by recursively selecting each element x from the
   list xs, and then generating all permutations of the
   remaining elements after removing the first occurrence
   of x from xs. 
   The base case is when the input list is empty, in which
   case the result is a list containing the empty list
   [[]], which is the only permutation of an empty 
   list.
-}

------------------------------------------------------------