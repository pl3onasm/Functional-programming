> import Prelude hiding (reverse, zip, (++))

-----------------------------------------------------------
             RESIT FUNCTIONAL PROGRAMMING 2021               
-----------------------------------------------------------

You may use the standard arithmetic/Boolean operators and 
the standard Haskell functions given in the file 
functions.md throughout the entire exam.

___________________________________________________________

1. Types 
___________________________________________________________

Question 1.1:
Is the following expression type correct? 
If YES, then give the most general type of the expression.

[[],[[]]]

--------
Answer: 

Yes, this is type correct. The first element of the outer
list is the empty list, so its type is [b] for some type b.
The second element of the outer list is a list containing
the empty list, so its type is [[a]] for some type a.
Since the elements of a list must have the same type, the
type checking algorithm will unify the two types, yielding
b = [a]. Hence, the type of the expression becomes [[[a]]],
where a is a type variable that can be instantiated to any
type.


--------------------------------
Question 1.2:
Is the following expression type correct? 
If YES, then give the most general type of the expression.

[not, id]

--------
Answer:

Yes, this is type correct. The first element of the list
is the function not, which has type Bool -> Bool. The 
second element is the function id, which has type a -> a
for some type a. Since the elements of a list must have the
same type, the type checking algorithm will unify the two
types, yielding a = Bool. Hence, the expression represents
a list of functions of type Bool -> Bool. The most general
type of the expression is [Bool -> Bool].


--------------------------------
Question 1.3:
Is the following expression type correct? 
If YES, then give the most general type of the expression.

[(+), (:)]

--------
Answer: 

No, this expression is not type correct. The first element
of the list is the binary operator (+), which has the type 
Num a => a -> a -> a. The second element is the binary
cons operator (:), which has the type b -> [b] -> [b].
For these two types to be unified, the type checker first
needs to instantiate the type variable a to type b, so we
have a = b. However, in order to resolve the second 
argument of the operator, the type checker then needs to
instantiate the type variable b to [b], which is not
possible. Note that unification fails due to the type
structures, not due to the type constraints.


--------------------------------
Question 1.4:
What is the most general type of the following function f?

f = foldr (&&)

--------
Answer: 

The function foldr has the following type:
foldr :: (a -> b -> b) -> b -> [a] -> b
In this case, we partially apply foldr to the binary
operator (&&). The type of (&&) is Bool -> Bool -> Bool.
This means that we have a = Bool, b = Bool, and the type
of the function becomes: f :: Bool -> [Bool] -> Bool
It is a function that takes a Bool as its first argument,
a list of Bool as its second argument, and returns a
summary Bool value.


--------------------------------
Question 1.5:
What is the most general type of the following function g?

g = map map

--------
Answer: 

This is a partially applied function that applies the
function map to itself. The type of the inner map is:
map :: (a -> b) -> [a] -> [b]
The type of the outer map is:
map :: (c -> d) -> [c] -> [d]

The outer map expects a function of type (c -> d) as its
first argument. This function is the inner map. So the
type checker will try to unify the types of the inner and
outer map, yielding: c -> d = (a -> b) -> [a] -> [b]
So let: c = a -> b and d = [a] -> [b]
And then the partially applied function g has the type:
  g :: [a -> b] -> [[a] -> [b]]

In other words, g does not take a single function as its
argument anymore, because it has already been partially 
applied. It has become a function that takes a list of 
functions of type a -> b and returns a list of functions  
of type [a] -> [b]. It is a function that lifts the
function map to a higher level, where it can be applied to
a list of functions.


___________________________________________________________

2. Programming in Haskell
___________________________________________________________

Consider the following iterative process. Given two 
positive integers n, and g (where 0 < n ≤ g), we try to 
reach g starting from n, where we are allowed to 
iteratively use the operations n → 3 x n, n → 2 x n, and 
n → n + 1. 

For example, let n = 1, and g = 42. We can reach 42 in 5 
steps in the following way:

1 → (+1) → 2 → (x3) → 6 → (+1) → 7 → (x3) → 21 → (x2) → 42

Clearly, several other possibilities exist to reach 42, 
for example:
    1 → (+1) → 2 → (+1) → 3 → (x2) → 6 
    → (+1) → 7 → (x3) → 21 → (x2) → 42 

However, there is no way to reach 42 (starting in 1) with 
fewer than 5 steps.

Write a Haskell function minSteps :: Int -> Int -> Int such
that the call minSteps n g returns the minimum number of 
computational steps to get from n to g. So, minSteps 1 42 
returns 5.

--------
Answer:

> minSteps :: Int -> Int -> Int
> minSteps n g
>   | n   == g  = 0                       
>   | 3*n <= g  = 1 + minim third halve 
>   | 2*n <= g  = 1 + halve              
>   | otherwise = g - n   -- remaining steps of (-1)        
>       where 
>         minim a b = if a < b then a else b
>         halve = minSteps n (g `div` 2) + g `mod` 2 
>         third = minSteps n (g `div` 3) + g `mod` 3 

This solution takes a reverse-recursive approach, starting 
from the goal g and working back toward the starting point 
n. Since we are moving backward, we use the inverse 
operations of the forward problem: division by 3, division 
by 2, and subtraction by 1.

The idea is to reduce g as quickly as possible. If g is at 
least three times n, we compare the results of dividing by 
3 and dividing by 2, and choose the better option using 
minim. The expressions (g `mod` 3) and (g `mod` 2) account 
for any 'adjustment' steps of 1 needed to make g divisible 
before performing the division. If g is at least twice n 
but less than three times n, we only consider halving.

Finally, when g is less than twice n, division no longer 
helps, so we simply subtract 1 until we reach n. This is 
done efficiently by returning g - n, the exact number of 
remaining subtraction steps.

This approach is efficient with a time complexity on the
order of O(log g), as each recursive call reduces g by a
factor of 2 or 3, leading to a logarithmic number of
steps in relation to g. 

Another, more straightforward approach would be to use a
breadth-first search (BFS) to explore all possible paths
from n to g, but this would be less efficient: a naive BFS
would have exponential complexity in the depth of the 
search tree. With pruning using a visited set, it could
be made more efficient and run in O(g - n), since each
integer in the range [n, g] would be visited at most once.


___________________________________________________________

3. Higher-order functions
___________________________________________________________

Question 3.1:
What will be the output if you enter the expression 
  foldr (++) [0] (map (\x -> [x]) [1..10]) 
in the Haskell interpreter?

--------
Answer:

The inner map function essentially wraps each number
from 1 to 10 in a singleton list, resulting in the list:
[[1], [2], [3], [4], [5], [6], [7], [8], [9], [10]].

Next, foldr (++) [0] combines these lists from right to 
left using (++), starting with [0] as the base case:

[1] ++ ([2] ++ ([3] ++ (... ++ ([10] ++ [0]))...))

This results in the concatenation of all these lists,
yielding the final output:

[1,2,3,4,5,6,7,8,9,10,0]


--------------------------------
Question 3.2:
Give an implementation of the function reverse that 
makes use of foldr.

--------
Answer:

> reverse :: [a] -> [a]
> reverse xs = foldr (\x acc -> acc ++ [x]) [] xs

This implementation mimics the behavior of the reverse 
function as given in the functions.md file: it traverses 
the input list xs from left to right, and constructs a 
new list by appending each element x to the accumulator 
acc, which starts as an empty list. The result is a new 
list that is the reverse of the input list xs.

Note: This is not the most efficient way to reverse a 
list, because (++) is O(n) and is called for every element, 
leading to quadratic time complexity. An O(n) version 
would use foldl instead:

> reverse' xs = foldl (\acc x -> x : acc) [] xs


--------------------------------
Question 3.3:
Give an implementation (and the most general type) of the 
function zip that makes use of the function zipWith.

--------
Answer:

> zip :: [a] -> [b] -> [(a, b)]
> zip = zipWith (\x y -> (x, y))

The lambda function just wraps each pair of elements
from the two input lists into a tuple (x, y). This produces 
exactly the behavior of zip, pairing corresponding elements 
from the two lists until one of the lists runs out.


--------------------------------
Question 3.4:
Give an implementation of the operator ++ that makes use 
of foldr.

--------
Answer:

> (++) :: [a] -> [a] -> [a]
> xs ++ ys = foldr (:) ys xs

The function foldr traverses the first list xs from left
to right, and prepends each element to the accumulator 
which starts as ys. This constructs a new list by adding 
all elements of xs in order to the front of ys, producting
the concatenation of the two input lists. 


___________________________________________________________

4. List comprehensions
___________________________________________________________

Question 4.1:
What will be the output if you enter the following
expression in the Haskell interpreter? 

  [(x, y, x + y) | x <-[0, 1, 2], y <-[3, 4]] 

--------
Answer:

The expression is build in nested loop style, where for
each value of x, the inner loop iterates over all values of
y. So we obtain the following output:

  [(0,3,3), (0,4,4), (1,3,4), (1,4,5), (2,3,5), (2,4,6)]


--------------------------------
Question 4.2:
The function heads takes a list of lists and returns a list 
containing the heads of those lists.
For example: 

  heads [[1,2,3],[4,5],[],[6,7,8]] 
  should yield: [1,4,6]

Give an implementation of heads (and it most general type) 
that makes use of a list comprehension.

--------
Answer:

We can use pattern matching in a list comprehension to 
extract the heads of the lists:

> heads :: [[a]] -> [a]
> heads xss = [x | (x : _) <- xss]

More verbosely, we could also write:

> heads' xss = concat [take 1 xs | xs <- xss]


--------------------------------
Question 4.3:
Implement the function zipWith using a list comprehension.

--------
Answer:

> zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
> zipWith' f xs ys = [f x y | (x, y) <- zip xs ys]

This implementation uses the zip function to pair elements
from the two input lists xs and ys, and then applies the
function f to each pair (x, y) using a list comprehension.


--------------------------------
Question 4.4:
Use a list comprehension and the function zip to write a 
Haskell function locations n xs that returns the list of
all indexes i such that the ith element of xs is n 
(i.e. xs!!i == n). Note that the first element of a list 
has index 0.

For example: 
  locations 0 [x `mod` 10 | x <- [1..50]] 
  should yield: [9,19,29,39,49] 
  
You are not allowed to use the indexing operator !! in the
implementation.

--------
Answer:

> locations :: Eq a => a -> [a] -> [Int] 
> locations n xs = [i | (x, i) <- zip xs [0..], x == n] 

The implementation uses zip to pair each element x with its
index i and then filters the pairs where x == n. 


--------------------------------
Question 4.5:
Deﬁne a function doubleReverse which takes a list of lists 
as its argument and reverses each element of the list and 
then reverses the resulting list. The implementation of 
doubleReverse must use a list comprehension. 
As an example: 

  doubleReverse ["palindrome", "word"] 
  = ["drow", "emordnilap"]
  
--------
Answer:

This implementation first reverses each sublist using a 
list comprehension and then reverses the resulting list:

> doubleReverse :: [[a]] -> [[a]]
> doubleReverse xss = reverse [reverse xs | xs <- xss]

Alternatively, we can first reverse the input list xss and
then reverse each sublist on its own:

> doubleReverse' xss = [reverse xs | xs <- reverse xss]


___________________________________________________________

5. Inﬁnite lists
___________________________________________________________

Question 5.1:
Give a Haskell expression that produces the inﬁnite string 
"abbaaabbbbaaaaabbbbbbaaaaaaa...", i.e one a, two bs, three 
as, four bs, etc.

--------
Answer:

> abList :: String
> abList = concat [replicate n 'a' ++ replicate (n + 1) 'b' 
>                  | n <- [1,3..]]

This expression uses a list comprehension to generate the
infinite string abList. The list [1,3..] generates the
sequence of odd numbers n starting from 1. For each n, it 
creates a string with n 'a's followed by (n+1) 'b's using
the replicate function. The concat function then combines
all these strings into a single infinite string.


--------------------------------
Question 5.2:
Give a definition of the infinite list factorials of 
factorial numbers.
For example:
    take 10 factorials 
    should yield: [1,1,2,6,24,120,720,5040,40320,362880]

--------
Answer:

> factorials :: [Integer]
> factorials = 1 : [n * i | (n, i) <- zip factorials [1..]]

This definition uses a list comprehension to generate the
infinite list factorials. The first element is 1 (0! = 1),
and for each subsequent element, it multiplies the previous
factorial by the next integer in the sequence. The zip
function pairs each factorial with its index (starting 
from 1), allowing us to compute the next factorial based on 
the previous one.


--------------------------------
Question 5.3:
Give a deﬁnition of the infinite list pals of non-empty 
palindromic strings that consist of the letters 'a' and
'b'. For example: 
  take 8 pals 
  may return: ["a","b","aa","bb","aaa","bab","aba","bbb"]

The list pals must be organized such that a test like 
elem "abba" pals terminates.

--------
Answer:

> pals :: [String]
> pals = base ++ [x : mid ++ [x] | mid <- pals, x <- "ab"]
>   where base = ["a", "b", "aa", "bb"]

This definition starts with a base list of the simplest
palindromes: ["a", "b", "aa", "bb"]. Each next palindrome
is then built by taking an existing palindrome mid from
pals and adding an 'a' or 'b' to both ends, thus preserving
the symmetric pattern of palindromes.
Since the list pals generates palindromes in order of
increasing length, the test: elem "abba" pals, will
terminate after a finite number of steps. 


___________________________________________________________

6. ADT module
___________________________________________________________

The abstract data type (ADT) Set tp implements a data type 
for the storage of sets of the type tp, where tp is of the 
class Ord (i.e. the elements are ordered).

Implement a module Set that exports the ADT Set. In the 
concrete implementation you must make use of an ordered 
list of set elements, however this concrete implementation 
must be hidden from the user of this module.

The following operations on the data type Set must be 
implemented:
• empty returns an empty set.
• isEmpty returns True for an empty set, otherwise False.
• isElement x returns True if x is a member of the set, 
  otherwise False.
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
  Set, empty, isEmpty, isElement, insert, 
  delete, union, intersection
) where

This line exports the abstract data type Set and the 
functions, but does not export the constructor ST, thus
hiding the concrete implementation details.

> data Set a = ST [a]  

> empty :: Set a
> empty = ST []

> isEmpty :: Set a -> Bool
> isEmpty (ST []) = True
> isEmpty _       = False

> isElement :: Ord a => a -> Set a -> Bool
> isElement x (ST []) = False
> isElement x (ST (y : ys))
>   | x == y    = True
>   | x < y     = False
>   | otherwise = isElement x (ST ys)

> insert :: Ord a => a -> Set a -> Set a
> insert x (ST xs) = ST (ins x xs)
>   where
>     ins x [] = [x]
>     ins x (y : ys)
>       | x < y     = x : y : ys
>       | x == y    = y : ys            -- no duplicates
>       | otherwise = y : ins x ys

> delete :: Ord a => a -> Set a -> Set a
> delete x (ST xs) = ST (del x xs)
>   where
>     del _ [] = []
>     del x (y : ys)
>       | x < y     = y : ys             -- x not in set
>       | x == y    = ys
>       | otherwise = y : del x ys

> union :: Ord a => Set a -> Set a -> Set a
> union (ST xs) (ST ys) = ST (uMerge xs ys)
>   where
>     uMerge [] ys = ys
>     uMerge xs [] = xs
>     uMerge (x : xs) (y : ys)
>       | x < y     = x : uMerge xs (y : ys)
>       | x == y    = x : uMerge xs ys  -- no duplicates
>       | otherwise = y : uMerge (x : xs) ys

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


___________________________________________________________

7. Proof on lists
___________________________________________________________

In this problem we use the following deﬁnition of sum (the 
definitions of reverse and ++ are in the file functions.md):

  sum [] = 0
  sum (x : xs) = x + sum xs

Prove the following property p:   

  p(xs): sum xs = sum(reverse xs) 
         for any finite list xs

[Note: find the definition of reverse in the file
       functions.md included in this exam folder]

--------
Answer:

We will prove this property by structural induction on the
list xs.
---------------------------------
Base case: prove p([])
---------------------------------

    {RHS of p([])}
  sum (reverse [])
=   {applying reverse}
  sum []
    {LHS of p([])}

---------------------------------
Inductive case: prove p((x : xs))
---------------------------------
    Induction hypothesis:
      p(xs): sum xs = sum (reverse xs)

    {RHS of p((x : xs))}
  sum (reverse (x : xs))
=   {applying reverse}
  sum (reverse xs ++ [x])
=   {applying lemma}
  sum (reverse xs) + sum [x]
=   {using induction hypothesis}
  sum xs + sum [x]
=   {applying sum}
  sum xs + x + sum []
=   {applying sum}
  sum xs + x + 0
=   {properties of +}
  x + sum xs
=   {unapplying sum}
  sum ((x : xs))
    {LHS of p((x : xs))}

□

-----------------------------------------------
Lemma:  q(xs): sum (xs ++ ys) = sum xs + sum ys
-----------------------------------------------
We will prove this lemma by structural induction on xs.
[Note the similarity with: 
  length (xs ++ ys) = length xs + length ys]

---------------------------------
Base case: prove q([])
---------------------------------

    {RHS of q([])}
  sum [] + sum ys
=   {applying sum}
  0 + sum ys
=   {properties of +}
  sum ys
=   {unapplying ++}
  sum ([] ++ ys)
    {LHS of q([])}

---------------------------------
Inductive case: prove q((x : xs))
---------------------------------
    Induction hypothesis:
      q(xs):  sum (xs ++ ys) = sum xs + sum ys

    {LHS of q((x : xs))}
  sum ((x : xs) ++ ys)
=   {applying ++}
  sum (x : (xs ++ ys))
=   {applying sum}
  x + sum (xs ++ ys)
=   {using induction hypothesis}
  x + (sum xs + sum ys)
=   {associativity of +}
  (x + sum xs) + sum ys
=   {applying sum}
  sum (x : xs) + sum ys
    {RHS of q((x : xs))}

□


___________________________________________________________

8. Proof on trees
___________________________________________________________

Given is the data type Tree and the functions mirror, and 
size:

  data Tree a = Empty | Node a (Tree a) (Tree a)

  mirror :: Tree a -> Tree a
  mirror Empty = Empty
  mirror (Node x l r) = Node x (mirror r) (mirror l)

  size :: Tree a -> Integer
  size Empty = 0
  size (Node x l r) = 1 + size l + size r

Prove for all ﬁnite trees t: 

  p(t) : size (mirror t) = size t

--------
Answer:

We will prove this property by structural induction on the
tree t.

-----------------------------------
Base case: prove p(Empty)
-----------------------------------

    {LHS of p(Empty)}
  size (mirror Empty)
=   {applying mirror}
  size Empty
=   {applying size}
  0
=   {unapplying size}
  size Empty
    {RHS of p(Empty)}

-----------------------------------
Inductive case: prove p(Node x l r)
-----------------------------------
    Induction hypothesis:
      p(l): size (mirror l) = size l
      p(r): size (mirror r) = size r

    {LHS of p(Node x l r)}
  size (mirror (Node x l r))
=   {applying mirror}
  size (Node x (mirror r) (mirror l))
=   {applying size}
  1 + size (mirror r) + size (mirror l)
=   {using induction hypothesis}
  1 + size r + size l
=   {unapplying size}
  size (Node x l r)
    {RHS of p(Node x l r)}

□

___________________________________________________________