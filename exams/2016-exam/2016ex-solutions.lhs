> import Prelude hiding (concat)

-----------------------------------------------------------
             EXAM FUNCTIONAL PROGRAMMING 2016               
-----------------------------------------------------------

You may use the standard arithmetic/Boolean operators and 
the standard Haskell functions given in the file 
functions.md throughout the entire exam.

___________________________________________________________

1. Types 
___________________________________________________________

Question 1.1:
What is the type of the following expression?

  (42, [42], [[42]])

--------
Answer: 

This is a tuple with three components. As numeric literals
are polymorphic, and tuples do not require their components
to have the same type, the literal 42 can be of a different
type in each of the three components as long as it is of a
type that is an instance of the class Num. Therefore, the
type of the expression is:

  (Num a, Num b, Num c) => (a, [b], [[c]])


--------------------------------
Question 1.2:
What is the most general type of the function f?

  f = filter (== 'A')

--------
Answer:

The type of the function filter is:

  (a -> Bool) -> [a] -> [a]

The function (== 'A') has the type:
  Char -> Bool

The function f is a partial application of filter, which 
means that the first argument is fixed to (== 'A'), and
the argument type a is unified with Char. Therefore, the
most general type of f is:

  f :: [Char] -> [Char]


--------------------------------
Question 1.3:
What is the most general type of the function g?

  g = (\x -> (\y -> (y,x)))

--------
Answer: 

The function g is a lambda function that takes two 
arguments x and y and returns a tuple containing these
arguments in reversed input order. Tuples do not require
their components to be of the same type, so x and y 
should be represented by different type variables.
Therefore, the most general type of the function g is:

  g :: a -> b -> (b, a)


--------------------------------
Question 1.4:
What is the type of the function foldr?

--------
Answer: 

Foldr is a right-associative function that takes a binary
function f as its first argument, followed by an initial 
accumulator value and an input list. This is the same for
is counterpart foldl. Where it differs from foldl is that
the accumulator appears on the right in f, yielding the
type:

  foldr :: (a -> b -> b) -> b -> [a] -> b

Note that the function f takes as first argument an element
of type a, which is an element of the input list [a], and
as second argument the accumulator of type b. It returns a 
new accumulator of type b, which is also the output type
of foldr itself.


--------------------------------
Question 1.5:
What is the type of the following Haskell function h?

  h = (\f -> map f "Text" == [1,2,3,4])

--------
Answer: 

The function h is a lambda function that takes a function f
that is used as map's first argument, creating a function 
that maps f over an input list. Here, that list is a list 
of Chars ("Text"), meaning that the input for f must be of 
type Char. 
The output of h is a Bool, since it is the result of
comparing the output of map f "Text" with the list
[1,2,3,4] using the equality operator (==). This means that
the output type of f must be the same as the element type
of the list [1,2,3,4], which is of any type that is an
instance of the class Num, since numeric literals are
polymorphic.

Therefore, the most general type of h is:
  h :: Num a => (Char -> a) -> Bool


___________________________________________________________

2. Programming in Haskell
___________________________________________________________

The increasing list [1,2,3,4,5] has 9 non-empty increasing 
sublists that contain as many even numbers as odd numbers.

Write a Haskell function balancedSublists (including its 
type) that takes an increasing list and returns the list of 
its non-empty increasing sublists that have as many even 
numbers as odd numbers. The order of the sublists is 
irrelevant.

For example, balancedSubLists [1,2,3,4,5] 
may return the list:

  [[4,5],[3,4],[2,5],[2,3],[2,3,4,5],
   [1,4],[1,2],[1,2,4,5],[1,2,3,4]]

--------
Answer:

> -- returns the list of all sublists of a list
> subs :: [a] -> [[a]]
> subs [] = [[]]
> subs (x:xs) = subs xs ++ map (x:) (subs xs)

> -- checks whether a list has as many even as odd numbers 
> isBal :: [Integer] -> Bool
> isBal xs = length (filter even xs) 
>            == length (filter odd xs)

> -- returns the list of non-empty balanced sublists of
> -- its input list 
> balancedSublists :: [Integer] -> [[Integer]]
> balancedSublists xs = [ys | ys <- subs xs, isBal ys,
>                             ys /= []]

The function isBal checks whether a list has as many even 
as odd numbers. It can be made a bit more efficient by 
using foldr instead of filter, like this:

> isBal' xs = evens == odds
>   where
>     (evens, odds) = foldr cnt (0, 0) xs
>     cnt x (e, o) | even x    = (e + 1, o)
>                  | otherwise = (e, o + 1)

This implementation only traverses the input list once,
instead of twice, and avoids constructing intermediate 
lists with filter.

The function sublists generates all sublists of its input
list by peeling off the head of the list and recursively
generating all sublists of the tail xs. The final result is
the concatenation of the sublists of the tail xs and the
sublists of the tail with the head x prepended to each of
them. The base case is the list containing the empty list,
which is the only sublist of the empty list.

The function balancedSublists combines these two helper
functions in a list comprehension that generates the list
of all sublists of its input list that are balanced and
non-empty.


___________________________________________________________

3. Higher-order functions
___________________________________________________________

Question 3.1:
Write a function isEqual (including its type) that accepts 
three arguments: the first two arguments are functions
(both having the same type), which can be applied to each 
element of a list (the third argument). The function should
return True if and only if applying both functions to each 
element of the third argument yields the same result.
For example, isEqual (+1) (1+) [1,2,3] should yield True, 
while isEqual (^2) (2^) [1,2,3] should yield False. 
Your are not allowed to use recursion.

--------
Answer:

> isEqual :: Eq b => (a -> b) -> (a -> b) -> [a] -> Bool
> isEqual f g xs = (map f xs) == (map g xs)

The function works by mapping both input functions over the
input list, producing two lists of results. It then
compares these two lists for equality using the (==)
operator, which returns True if the lists are identical
and False otherwise. The type constraint Eq b ensures that
the output type of the functions can be compared for
equality.


--------------------------------
Question 3.2:
The function concat concatenates the elements of a list 
of lists. For example:

  concat [[1,2],[3],[4,2,3]] = [1,2,3,4,2,3] 

Give an implementation of the function concat (including 
its type) using foldr.

--------
Answer:

> concat :: [[a]] -> [a]
> concat = foldr (++) []

This implementation of concat uses foldr to traverse the
input list of lists from right to left. The binary function
(++) is used to concatenate each sublist with the
accumulated result, starting with the empty list [] as the
initial accumulator. The result is a single list containing
all the elements from the input sublists in their original
order.

Let us illustrate this with the example that was given:

  concat [[1,2],[3],[4,2,3]]
= foldr (++) [] [[1,2],[3],[4,2,3]]
= [1,2] ++ (foldr (++) [] [[3],[4,2,3]])
= [1,2] ++ ([3] ++ (foldr (++) [] [[4,2,3]]))
= [1,2] ++ ([3] ++ ([4,2,3] ++ (foldr (++) [] [])))
= [1,2] ++ ([3] ++ ([4,2,3] ++ []))
= [1,2] ++ ([3] ++ [4,2,3])
= [1,2] ++ [3,4,2,3]
= [1,2,3,4,2,3]


--------------------------------
Question 3.3:
Write a function mulinceven (including its type) that takes
a list of Integers, and returns the product of one plus 
every number in the input that is at least 4. 
For example, mulinceven [7,3,2,4,5] returns 240, because
(7+1)*(4+1)*(5+1) = 240. Your implementation must make use 
of map, filter, and foldr.

--------
Answer:

> mulinceven :: [Integer] -> Integer 
> mulinceven xs = foldr (*) 1 (map (+1) (filter (>= 4) xs))

First, the function filter (>= 4) is used to select only
those elements of the input list that are at least 4.
Next, the function map (+1) increments each of these
filtered elements by 1.
Finally, foldr (*) 1 multiplies all elements in the 
resulting list, starting with 1 as the initial value. 
The final result is the desired product.


___________________________________________________________

4. List comprehensions
___________________________________________________________

Question 4.1:
Write a function oddeven (including its type) that takes a 
list of pairs and returns a list containing the first 
element from each of the pairs in even-numbered positions 
and the second element from each of the pairs in odd-
numbered positions, where numbering of list elements begins 
from 0.

Examples:
            oddeven [(1,2),(3,4),(5,6),(7,8)] 
          = [1,4,5,8]
            
            oddeven [("hello","world"),("from","Venus")] 
          = ["hello", "Venus"].

The implementation of oddeven must be a list 
comprehension.

--------
Answer:

> oddeven :: [(a, a)] -> [a]
> oddeven xs = [if i `mod` 2 == 0 then x else y 
>               | ((x, y), i) <- zip xs [0..]]

The function works by zipping the input list with an
infinite list of indices starting from 0. The list
comprehension then iterates over these pairs of elements
and their indices, selecting the first element x if the
index i is even, and the second element y if the index i
is odd.


--------------------------------
Question 4.2:
Write a function removeRepetition (including its type) 
that removes all but one occurrence of consecutive  
repeated elements from its input list.

Examples:   removeRepetition [1,2,2,3,3,3,4,5,1,1]
          = [1,2,3,4,5,1]
          
            removeRepetition "Haaassskkkell"  
          = "Haskel"

The definition of the function removeRepetition must make 
use of a list comprehension.

--------
Answer:

> remRepetition :: Eq a => [a] -> [a]
> remRepetition [] = []
> remRepetition (x:xs) = x : [y | (p, y) <- zip (x:xs) xs,
>                                  p /= y]

The function works by zipping the input list with its tail,
producing pairs of consecutive elements. The list com-
prehension then filters out those elements y that are equal 
to their predecessor p, effectively removing consecutive 
duplicates while preserving the first occurrence of each 
group. The head of the input list is prepended to the 
result, since it is always included in the output.

Let us illustrate this with the second example:

  remRepetition "Haaassskkkell"

= 'H' : [y | (p, y) <- zip "Haaassskkkell" "aaassskkkell",
             p /= y]

= 'H' : [y | (p, y) <- [('H','a'),('a','a'),('a','a'),
                        ('a','s'),('s','s'),('s','s'),
                        ('s','k'),('k','k'),('k','k'),
                        ('k','e'),('e','l'),('l','l')],
             p /= y]

= 'H' : "askel"

= "Haskel"


--------------------------------
Question 4.3:
Write a function sublists (including its type) that takes
a list and returns the list of all its possible sublists 
(the order of the sublists is irrelevant). 
Use a list comprehension in combination with recursion.
For example:
  sublists [1,2,3] may return 
  [[],[1],[2],[3],[1,2],[1,3],[2,3],[1,2,3]]

--------
Answer:

> sublists :: [a] -> [[a]]
> sublists []     = [[]]
> sublists (x:xs) = sublists xs ++ [x:s | s <- sublists xs]

The function sublists generates all sublists of its input
list by peeling off the head of the list and recursively
generating all sublists of the tail xs. The final result is
the concatenation of the sublists of the tail xs and the
sublists of the tail with the head x prepended to each of
them. The base case is the list containing the empty list,
which is the only sublist of the empty list.

To illustrate this, consider the example that was given:

  sublists [1,2,3]
= sublists [2,3] ++ [1:s | s <- sublists [2,3]]
= (sublists [3] ++ [2:s | s <- sublists [3]]) 
  ++ [1:s | s <- sublists [2,3]]
= ((sublists [] ++ [3:s | s <- sublists []])
  ++ [2:s | s <- sublists [3]])
  ++ [1:s | s <- sublists [2,3]]
= (([[]] ++ [3:s | s <- [[]]])
  ++ [2:s | s <- sublists [3]])
  ++ [1:s | s <- sublists [2,3]]
= (([[]] ++ [[3]])
  ++ [2:s | s <- sublists [3]])
  ++ [1:s | s <- sublists [2,3]]
= ([[], [3]]
  ++ [2:s | s <- sublists [3]])
  ++ [1:s | s <- sublists [2,3]]
= ([[], [3]]
  ++ [2:s | s <- [[], [3]]])
  ++ [1:s | s <- sublists [2,3]]
= ([[], [3]]
  ++ [[2], [2,3]])
  ++ [1:s | s <- sublists [2,3]]
= ([[], [3], [2], [2,3]])
  ++ [1:s | s <- sublists [2,3]]
= ([[], [3], [2], [2,3]])
  ++ [1:s | s <- [[], [3], [2], [2,3]]]
= ([[], [3], [2], [2,3]])
  ++ [[1], [1,3], [1,2], [1,2,3]]
= [[], [3], [2], [2,3], [1], [1,3], [1,2], [1,2,3]]


___________________________________________________________

5. Infinite lists
___________________________________________________________

Question 5.1:
Given is the infinite list of prime numbers, defined as 
follows:

> primes :: [Integer] 
> primes = sieve [2..]
>   where
>   sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p /= 0]
  
Write a function isPrime such that isPrime n returns True 
if and only if n is in the list primes.

--------
Answer:

> isPrime :: Integer -> Bool
> isPrime n = n == head(dropWhile (< n) primes)

This solution works because the list primes is strictly
increasing. The function dropWhile (< n) first drops all 
primes that are less than n, leaving a list that begins 
with the smallest prime greater than or equal to n.
If n is prime, then it will be the head of this list,
otherwise the head will be a prime greater than n.


--------------------------------
Question 5.2:
The infinite list ones is defined as:

> ones = 1 : ones

Use only ones, arithmetic operators, and zipWith to create 
two mutually recursive definitions of the infinite lists
evens and odds, where evens = [0,2,4,6,8,..] and 
odds = [1,3,5,7,9,..]. Mutual recursive means that evens 
(but not odds) can appear in the definition of odds and 
odds (but not evens) can appear in the definition of evens.

--------
Answer:

> evens = 0 : zipWith (+) odds ones
> odds  = zipWith (+) evens ones

We start with evens, since that list begins with 0. 
Every subsequent even number is then obtained by adding 1 
to each number in odds, using zipWith. Conversely, the odds 
list is generated by adding 1 to each number in evens.

This defines two mutually recursive infinite lists. 
The lists are generated by going back and forth between the
two definitions, each time adding 1 to the last number 
generated in the other list.


--------------------------------
Question 5.3:
Define the function multiples :: [Integer] -> [Integer], 
that takes a finite list of Integers and produces the 
infinite sorted list (without repetitions) of all 
multiples of the numbers in the input list.

For example: 
    take 10 (multiples [2,3,5]) 
  = [0,2,3,4,5,6,8,9,10,12]

--------
Answer:

> multiples :: [Integer] -> [Integer]
> multiples xs = foldr merge [] [[0,x..] | x <- xs]

> merge :: Ord a => [a] -> [a] -> [a]
> merge [] ys = ys
> merge xs [] = xs
> merge (x : xs) (y : ys)
>   | x < y     = x : merge xs (y : ys)
>   | x > y     = y : merge (x : xs) ys
>   | otherwise = x : merge xs ys

The list comprehension generates a list of increasing lists
of multiples for each integer x in xs, starting from 0 and 
incrementing by x each time.
The foldr merge then takes these input streams of multiples
and merges them into a single sorted infinite list, whilst
removing duplicates (last guard in merge). The merge 
function is similar to the merge step in the merge sort
algorithm, efficiently combining two sorted lists into one.


___________________________________________________________

6. ADT module
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

7. Proof on lists
___________________________________________________________

Given is the recursive definition of the function drop:

  drop :: Int -> [a] -> [a]
  drop 0 xs = xs
  drop n [] = []
  drop n (x:xs) = drop (n-1) xs

Prove the following property p: 

  p(xs):  drop m (drop n xs) = drop (m+n) xs 
          for all finite lists xs and m, n ≥ 0

--------
Answer:

We prove the property p by structural induction on the list
xs.

----------------------------------------
Base case: prove p([])
----------------------------------------

    {LHS of p([])}
  drop m (drop n [])
=   {applying drop}
  drop m []
=   {applying drop}
  []
=   {unapplying drop}
  drop (m+n) []
    {RHS of p([])}

----------------------------------------
Induction step: prove p(xs) => p((x:xs))
----------------------------------------

    Induction hypothesis:
      p(xs): drop m (drop n xs) = drop (m+n) xs

  Since the definition of drop distinguishes the 
  cases n = 0 and n > 0 in the case of a non-empty list,
  we must perform a case analysis on n, and prove both
  cases separately. 

Case 1: n = 0

    {LHS of p((x:xs))}
  drop m (drop 0 (x:xs))
=   {applying drop}
  drop m (x:xs)
=   {arithmetic}
  drop (m+0) (x:xs)
    {RHS of p((x:xs))}

Case 2: n > 0
  
    {LHS of p((x:xs))}
  drop m (drop n (x:xs))
=   {applying drop}
  drop m (drop (n-1) xs)
=   {induction hypothesis p(xs)}  
  drop (m+(n-1)) xs
=   {arithmetic}
  drop ((m+n)-1) xs
=   {unapplying drop}
  drop (m+n) (x:xs)
    {RHS of p((x:xs))}  

□


___________________________________________________________

8. Proof on trees
___________________________________________________________

Given is the data type Tree and the functions lrorder, 
rlorder, and mirror:

  data BinTree a = Empty | Node a (BinTree a) (BinTree a)

  mirror :: BinTree a -> BinTree a
  mirror Empty = Empty
  mirror (Node x l r) = Node x (mirror r) (mirror l)

  lrorder, rlorder :: BinTree a -> [a]
  lrorder Empty = []
  lrorder (Node x l r) = lrorder l ++ [x] ++ lrorder r

  rlorder Empty = []
  rlorder (Node x l r) = rlorder r ++ [x] ++ rlorder l

Prove for all finite trees t: 

  p(t): lrorder t = rlorder (mirror t)

The following properties of (++) may be used without proof:

  (xs ++ ys) ++ zs = xs ++ (ys ++ zs)
  xs ++ [] = xs

[Note: refer to the file functions.md for the 
 definition of (++)]

--------
Answer:

We prove the property p by structural induction on the
tree t.

--------------------------------------
Base case: prove p(Empty)
--------------------------------------

    {LHS of p(Empty)}
  lrorder Empty
=   {applying lrorder}
  []
=   {unapplying rlorder}
  rlorder Empty
=   {unapplying mirror}
  rlorder (mirror Empty)
    {RHS of p(Empty)}

--------------------------------------
Induction step: prove p(l) ∧ p(r)
                      => p(Node x l r)
--------------------------------------

    Induction hypothesis:
      p(l): lrorder l = rlorder (mirror l)
      p(r): lrorder r = rlorder (mirror r)

    {LHS of p(Node x l r)}
  lrorder (Node x l r)
=   {applying lrorder}
  lrorder l ++ [x] ++ lrorder r
=   {induction hypothesis p(l) and p(r)}
  rlorder (mirror l) ++ [x] ++ rlorder (mirror r)
=   {unapplying rlorder}
  rlorder (Node x (mirror r) (mirror l))
=   {unapplying mirror}
  rlorder (mirror (Node x l r))
    {RHS of p(Node x l r)}

□

___________________________________________________________