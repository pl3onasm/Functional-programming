> import Prelude hiding (replicate)

-----------------------------------------------------------
             RESIT FUNCTIONAL PROGRAMMING 2014               
-----------------------------------------------------------

You may use the standard arithmetic/Boolean operators and 
the standard Haskell functions given in the file 
functions.md throughout the entire exam.

___________________________________________________________

1. Types 
___________________________________________________________

Question 1.1:
What is the type of the following Haskell function wtel?

  wtel [] = []
  wtel (x : xs) = if x == [] then wxs else x : wxs
    where wxs = wtel xs

--------
Answer: 

From the base case it is clear that wtel is a function 
that takes a list and outputs a list. As the empty list is
polymorphic, it has type [a] for any type a. So we have
wtel :: [a] -> [a] for now.
Looking at the recursive case, we see that the elements of
the input list are lists themselves, which are compared to 
the empty list. So, the type a must come with the con-
straint that it must be comparable for equality, i.e. it 
must be a member of the Eq class. The reason for this is 
that lists are comparable if and only if their elements
are comparable.
It also becomes clear what wtel actually does: it removes 
empty lists from the input, which is a list of lists.
Putting this all together, we obtain the type:

  wtel :: Eq a => [[a]] -> [[a]]


--------------------------------
Question 1.2:
What is the type of the following Haskell function cl?

  cl ps = ps ++ [(p,s) | (p,q) <- ps, (r,s) <- ps, q == r]

--------
Answer:

The function cl takes a single argument ps. From the list
comprehension we see that ps is a list of pairs, so ps has
type [(a, b)] for some types a and b, since tuples do not
require their components to have the same type.
We also see that the list comprehension compares the second
and first components of the pairs in ps, which means that
a = b, and b must be a member of the Eq class, so we obtain 
the following signature for cl:

  cl :: Eq a => [(a, a)] -> [(a, a)]


--------------------------------
Question 1.3:
What is the type of the standard Haskell indexing 
operator !! (as an example [0..10]!!3 = 3)?

--------
Answer: 

The indexing operator !! takes a list and an integer as
arguments, and returns the element at the given index of
the list. The elements of the list can be of any type,
so we have the following type signature:

  (!!) :: [a] -> Int -> a


--------------------------------
Question 1.4:
What is the type of the following Haskell function map2?

  map2 f [] [] = []
  map2 f (x : xs) (y : ys) = (f x y) : map2 f xs ys

--------
Answer: 

The function map2 takes three arguments: a function f and
two lists. From the recursive case we see that f is a 
binary function that takes an element of the first list
and an element of the second list, and produces an element
of the output list. The base case shows that the output is
a list. So, if the first list has elements of type a, the
second list has elements of type b, and the output list
has elements of type c, then f has type a -> b -> c.

Thus, we obtain the following type signature:

  map2 :: (a -> b -> c) -> [a] -> [b] -> [c]

The function map2 is in fact the zipWith function from
the standard Prelude.


--------------------------------
Question 1.5:
What is the type of the following Haskell function tw?

  tw = (\f -> (\x -> (f.f) x))

--------
Answer: 

The function tw is a lambda function taking a function f 
and an argument x. Since f is composed with itself, it must
be of type a -> a, and so x must have type a. The output 
of tw then, which is (f.f) x, must also have type a. 
Therefore, tw has the following signature:

  tw :: (a -> a) -> a -> a


___________________________________________________________

2. Programming in Haskell
___________________________________________________________

Consider a positive integer N. We denote its decimal 
digits by X₀, X₁, ..., Xₖ. The number N is called a funny 
number if you can select at most three (but at least one) 
of its digits such that N is a divisor of the number
(X₀ + X₁ + ... + Xₖ - S)^S , where S is the sum of the 
selected digits. As an example, 1458 is a funny number 
since ((1 + 4 + 5 + 8) - (1 + 5))^{1+5} = 12^6 = 2985984 
is divisible by 1458. Note that we selected the two 
digits 1 and 5.

Write a Haskell function isFunny (including its type) that 
takes an integer number as its argument, and returns 
True if and only if this argument is a funny number.

--------
Answer:

> -- computes all sublists of a list
> sublists :: [a] -> [[a]]
> sublists [] = [[]]
> sublists (x:xs) = let ss = sublists xs
>                   in ss ++ map (x:) ss

> -- checks if a number is funny
> isFunny :: Integer -> Bool
> isFunny n = any funnyCandidate [xs | xs <- subs, 
>                                 length xs <= 3, xs /= []]
>   where
>     ds = digits n
>     digitSum = sum ds
>     subs = sublists ds
>     digits 0 = [] 
>     digits n = digits (n `div` 10) ++ [n `mod` 10]
>     funnyCandidate xs = let s = sum xs 
>                         in (digitSum - s)^s `mod` n == 0

This is a straightforward implementation of the definition
of funny numbers. The helper function sublists computes all
sublists of a list. This function is used in isFunny to
compute all sublists of the list of digits of the input
number n, which is then filtered to keep only those 
sublists of length at most 3 and at least 1.
For each of these sublists xs, it computes its sum s, and 
checks if (sum of all digits - s)^s is divisible by n. 
If this is the case for at least one sublist, then n is a 
funny number.

The downside of this implementation is that it is not very
efficient, as it computes all sublists of the list of 
digits, while we only need sublists of length at most 3.
A more efficient implementation would generate only the
sublists of length at most 3 directly, without generating
all sublists first. Below is such an implementation.

> -- generates all subsets of exactly size k
> subsetOfSize :: Int -> [a] -> [[a]]
> subsetOfSize 0 _      = [[]]
> subsetOfSize _ []     = []
> subsetOfSize k (x:xs) = map (x:) (subsetOfSize (k-1) xs) 
>                         ++ subsetOfSize k xs

> -- generates all non-empty subsets of size at most n
> subsUpTo :: Int -> [a] -> [[a]]
> subsUpTo n xs = concat [subsetOfSize k xs | k <- [1..n]]
> 
> -- checks if a number is funny
> isFunny' :: Integer -> Bool
> isFunny' n = any funnyCandidate (subsUpTo 3 ds)
>   where
>     ds = digits n
>     digitSum = sum ds
>     digits 0 = [] 
>     digits n = digits (n `div` 10) ++ [n `mod` 10]
>     funnyCandidate xs = let s = sum xs 
>                         in (digitSum - s)^s `mod` n == 0 


___________________________________________________________

3. List comprehensions
___________________________________________________________

Question 3.1:
Use a list comprehension to define a function 
inverse::[(a, b)]->[(b, a)] such that elem (x,y) ps if
and only if elem (y,x) (inverse ps).

--------
Answer:

> inverse :: [(a, b)] -> [(b, a)]
> inverse ps = [(y, x) | (x, y) <- ps]

The list comprehension iterates over all pairs (x, y) in
the input list ps, and for each such pair it swaps the
components to create the pair (y, x), which is then 
included in the output list.


--------------------------------
Question 3.2:
Use a list comprehension to make your own implementation 
of the standard Haskell function replicate. The call 
replicate n x yields a list of length n with x being the 
value of every element. So, replicate 5 'a' returns
"aaaaa".

--------
Answer:

> replicate :: Int -> a -> [a]
> replicate n x = [x | _ <- [1..n]]

The generator _ <- [1..n] in the list comprehension is 
merely used as a counter, indicating how many times x
should be included in the output list. 
The underscore indicates that we do not care about the
actual values produced by the generator, only about the
number of values, which is exactly n.


--------------------------------
Question 3.3:
Define a function doubleReverse which takes a list of 
strings as its argument and reverses each element of the 
list and then reverses the resulting list. 
The implementation of doubleReverse must use a list 
comprehension. As an example: 

  doubleReverse ["hello", "world"] = ["dlrow", "olleh"]

--------
Answer:

> doubleReverse :: [[String]] -> [[String]]
> doubleReverse xss = reverse [reverse xs | xs <- xss]

By iterating over all elements xs of the input list xss,
and reversing each such element, we obtain a list of the
reversed elements. Finally, we also reverse this list to
obtain the final result.


___________________________________________________________

4. Infinite lists
___________________________________________________________

Question 4.1:
The function powers n returns the infinite list 
[n^0, n^1, n^2, n^3, ...]. Give a recursive Haskell 
implementation (including its type) of the function powers.

--------
Answer:

> powers :: Integer -> [Integer]
> powers n = 1 : [x * n | x <- powers n]

Aternatively, we can use map instead of a list 
comprehension:

> powers' n = 1 : map (*n) (powers' n)

In both implementations, the base case is n^0 = 1, which
is listed first. The rest of the list is obtained by
multiplying each element of the list by n, which yields
the next power of n.


--------------------------------
Question 4.2:
The sequence aₖ is defined as follows:

  a₀ = 1 
  a₁ = 2
  aₖ = 3aₖ₋₁ + 2aₖ₋₂ for integer k > 1 
  
Define the infinite list seqa, which is the list 
[a₀, a₁, a₂, a₃, a₄, ...], so seqa!!k should yield aₖ.

--------
Answer:

> seqa :: [Integer]
> seqa = 1 : 2 : zipWith (\x y = 2*x + 3*y) seqa (tail seqa)

The base cases a₀ and a₁ are listed first. The rest of 
the list is obtained by zipping the list with its tail, 
which yields pairs (aₖ₋₂, aₖ₋₁) for k > 1. The lambda 
function then computes aₖ from these pairs.


--------------------------------
Question 4.3:
In the figure included in this exam folder you see the 
first 5 rows of Pascal's triangle (see file Pascal.png)
To build the triangle, we start with the row [1] at the top
(we call this row 0), then continue placing numbers below 
it in a triangular pattern. Each row consists of elements 
that are the sum of the two numbers above it (except for 
the boundaries, which are all 1). In the figure, it is 
highlighted that the 4 in row 4 is obtained by adding the 
numbers 1 and 3 from row 3.

Give a definition of the infinite list 
pascalTriangle ::[[Integer]], such that pascalTriangle!!n
yields the nth row of Pascal's triangle 
So:   pascalTriangle!!4 = [1,4,6,4,1]

--------
Answer:

> pascalTriangle :: [[Integer]]
> pascalTriangle = [1] : [zipWith (+) (0 : xs) (xs ++ [0]) 
>                         | xs <- pascalTriangle]

The base case is the first row [1]. Each subsequent row
is obtained by zipping the previous row with itself, but
shifted left and right by one position, padding with 0s.
This way, each element in the new row is the sum of the
two elements above it in the previous row, with the
boundaries being 1, as required.


___________________________________________________________

5. ADT module
___________________________________________________________

The abstract data type (ADT) Set tp implements a data type 
for the storage of sets of the type tp, where tp is of the 
class Ord (i.e. the elements are ordered).

Implement a module Set that exports the ADT Set. You can 
choose a concrete implementation yourself, however this
implementation must be hidden from the user of this module.

The following operations on the data type Set must be 
implemented:
• empty returns an empty set.
• isEmpty returns True for an empty set, otherwise False.
• insert: returns the set after insertion of an element.
• delete: returns the set after removal of an element.
• union: returns the union of two sets.
• intersection: returns the intersection of two sets.

--------
Answer:

To turn the below code into a module, you would create
a file named Set.hs and start it with the following
module declaration:

module Set (
    Set,        -- export the abstract data type Set
    empty,
    isEmpty,
    insert,
    delete,
    union,
    intersection
) where

This line exports the abstract data type Set and its 
associated functions, but does not export the constructor
ST of the data type, thus hiding the concrete 
implementation details.

> data Set a = ST [a]

> -- Show instance for pretty printing of sets
> instance (Show a) => Show (Set a) where
>   show (ST xs) = "{" ++ showSet xs ++ "}"
>     where
>       showSet []     = " "
>       showSet [x]    = show x
>       showSet (x:xs) = show x ++ "," ++ showSet xs

> -- creates an empty set
> empty :: Set a
> empty = ST []

> -- checks if a set is empty
> isEmpty :: Set a -> Bool
> isEmpty (ST []) = True
> isEmpty _       = False

> -- inserts an element into the set
> -- (no duplicates allowed)
> insert :: Ord a => a -> Set a -> Set a
> insert x (ST xs) = ST (ins x xs)
>   where
>     ins x [] = [x]
>     ins x (y : ys)
>       | x < y     = x : y : ys
>       | x == y    = y : ys            -- no duplicates
>       | otherwise = y : ins x ys

> -- deletes an element from the set (if it exists)
> delete :: Ord a => a -> Set a -> Set a
> delete x (ST xs) = ST (del x xs)
>   where
>     del _ [] = []
>     del x (y : ys)
>       | x < y     = y : ys            -- x not in set
>       | x == y    = ys
>       | otherwise = y : del x ys

> -- returns the union of two sets
> union :: Ord a => Set a -> Set a -> Set a
> union (ST xs) (ST ys) = ST (uMerge xs ys)
>   where
>     uMerge [] ys = ys
>     uMerge xs [] = xs
>     uMerge (x : xs) (y : ys)
>       | x < y     = x : uMerge xs (y : ys)
>       | x == y    = x : uMerge xs ys  -- no duplicates
>       | otherwise = y : uMerge (x : xs) ys

> -- returns the intersection of two sets
> intersection :: Ord a => Set a -> Set a -> Set a
> intersection (ST xs) (ST ys) = ST (iMerge xs ys)
>   where
>     iMerge [] _ = []
>     iMerge _ [] = []
>     iMerge (x : xs) (y : ys)
>       | x < y     = iMerge xs (y : ys)
>       | x == y    = x : iMerge xs ys  -- no duplicates
>       | otherwise = iMerge (x : xs) ys 


Note that the helper functions uMerge and iMerge only 
differ in how they handle the base cases. We could have 
combined them into a single higher-order merge function,
but that would have made the code less readable.

Example usage:

ghci> x = empty
ghci> x
{}
ghci> isEmpty x
True
ghci> y = insert 5 (insert 4 (insert 1 (insert 9 x)))
ghci> y
{1,4,5,9}
ghci> z = insert 2 (insert 3 (insert 6 (insert 4 x)))
ghci> z
{2,3,4,6}
ghci> union y z
{1,2,3,4,5,6,9}
ghci> intersection y z
{4}
ghci> delete 4 y
{1,5,9}


___________________________________________________________

7. Proof by induction
___________________________________________________________

Given are the following Haskell definitions of the 
functions f and g:

  f :: Integer -> Integer
  f 0 = 0
  f 1 = 1
  f n = 5*(f (n-1)) - 6*(f (n-2))

  g :: Integer -> Integer -> Integer
  g n 0 = 1
  g n e = n*(g n (e - 1))

Prove for all natural numbers n: 

  p(n): f n = g 3 n - g 2 n

--------
Answer:

We prove the property p(n) by natural induction on n.

------------------------------------
Base cases: prove p(0) and p(1)
------------------------------------

    {LHS of p(0)}
  f 0
=   {applying f}
  0
=   {arithmetic}
  1 - 1
=   {unapplying g}
  g 3 0 - g 2 0
    {RHS of p(0)}

    {LHS of p(1)}
  f 1
=   {applying f}
  1
=   {arithmetic}
  3 - 2
=   {unapplying g}
  g 3 1 - g 2 1
    {RHS of p(1)}

------------------------------------
Induction step: prove p(n) => p(n+1)
------------------------------------

  Induction hypothesis:
    p(n): f n = g 3 n - g 2 n

    {LHS of p(n+1)}
  f (n + 1)
=   {applying f}
  5 * (f n) - 6 * (f (n - 1))
=   {induction hypothesis p(n) and p(n-1)}
  5 * (g 3 n - g 2 n) - 6 * (g 3 (n - 1) - g 2 (n - 1)) 
=   {arithmetic}
  5 * (g 3 n - g 2 n) 
  - 2 * 3 * g 3 (n - 1) + 3 * 2 * g 2 (n - 1)
=   {unapplying g for last two terms}
  5 * (g 3 n - g 2 n) - 2 * (g 3 n) + 3 * (g 2 n)
=   {arithmetic}
  3 * (g 3 n) - 2 * (g 2 n)
=   {unapplying g}
  g 3 (n + 1) - g 2 (n + 1)
    {RHS of p(n+1)}

□


___________________________________________________________

8. Proof on lists
___________________________________________________________

Given are the definitions of the Haskell functions sum, 
and reverse:

  sum :: [Integer] -> Integer
  sum [] = 0
  sum (x:xs) = (sum xs) + x

  reverse :: [a] -> [a]
  reverse [] = []
  reverse (x:xs) = reverse xs ++ [x]

Prove the following property p: 

  p(xs):  sum (reverse xs) = sum xs 
          for all finite lists xs.

[Note: If you need one or more lemmas to complete the 
 proof, then prove these lemmas separately. 
 Refer to functions.md for the definition of ++]

--------
Answer:

We prove the property p by structural induction 
on the list xs.

----------------------------------------
Base case: prove p([])
----------------------------------------

    {LHS of p([])}
  sum (reverse [])
=   {applying reverse}
  sum []
    {RHS of p([])}

----------------------------------------
Induction step: prove p(xs) => p((x:xs))
----------------------------------------

  Induction hypothesis:
    p(xs): sum (reverse xs) = sum xs

    {LHS of p((x:xs))}
  sum (reverse (x : xs))
=   {applying reverse}
  sum (reverse xs ++ [x])
=   {applying lemma q}
  sum (reverse xs) + sum [x]
=   {applying sum: sum [x] = sum (x : []) 
                 = sum [] + x = 0 + x = x}
  sum (reverse xs) + x
=   {induction hypothesis p(xs)}
  (sum xs) + x
=   {unapplying sum}  
  sum (x : xs)
    {RHS of p((x:xs))}

□

----------------------------------------
Lemma q: For all finite lists xs and ys:
  sum (xs ++ ys) = sum xs + sum ys
----------------------------------------

We prove the property q by structural  
induction on the list xs.

----------------------------------------
Base case: prove q([])
----------------------------------------

    {LHS of q([])}
  sum ([] ++ ys)
=   {applying (++)}
  sum ys
=   {arithmetic}
  0 + sum ys
=   {unapplying sum}  
  sum [] + sum ys
    {RHS of q([])}

----------------------------------------
Induction step: prove q(xs) => q((x:xs))
----------------------------------------

    Induction hypothesis:
      q(xs): sum (xs ++ ys) = sum xs + sum ys

    {LHS of q((x:xs))}
  sum ((x : xs) ++ ys)
=   {applying (++)}
  sum (x : (xs ++ ys))
=   {applying sum}
  (sum (xs ++ ys)) + x
=   {induction hypothesis q(xs)}
  (sum xs + sum ys) + x
=   {associativity and commutativity of +}
  (sum xs + x) + sum ys
=   {unapplying sum}  
  sum (x : xs) + sum ys
    {RHS of q((x:xs))}

□

___________________________________________________________