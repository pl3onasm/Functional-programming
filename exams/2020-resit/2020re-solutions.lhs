> import Prelude hiding (reverse)

-----------------------------------------------------------
             RESIT FUNCTIONAL PROGRAMMING 2020               
-----------------------------------------------------------

You may use the standard arithmetic/Boolean operators and 
the standard Haskell functions given in the file 
functions.md throughout the entire exam.

___________________________________________________________

1. Types 
___________________________________________________________

Question 1.1:
Is the following expression type correct? 
If YES, then give the type of the expression.

[not,(&& True)]

--------
Answer: 

Yes, this expression is type correct. The second element of
the list is a partially applied binary function (&&) with 
the argument True. The first element is the function not, 
a unary function of type Bool -> Bool. Thus, both elements 
of the list have the same type Bool -> Bool, and the list
can be constructed. The type of the expression is:  

[(Bool -> Bool)]


--------------------------------
Question 1.2:
Is the following expression type correct? 
If YES, then give the type of the expression.

[[(*)],(+)]

--------
Answer:

No, this expression is not type correct. The first element 
of the list is a list of binary functions of type
[(Num a) => (a -> a -> a)], while the second element is a
single binary function of type (Num a) => (a -> a -> a).
As the two elements of the list have different types, the
list cannot be constructed.


--------------------------------
Question 1.3:
Is the following expression type correct? 
If YES, then give the type of the expression.

(42 - ) . (+ (42::Int))

--------
Answer: 

Yes, this expression is type correct. The composition 
operator (.) takes two unary functions and returns a
unary function. The first function is a partially applied
binary function (42 -) of type (Num a) => (a -> a).
The second function is a partially applied binary function
((+) (42::Int)) of type (Int -> Int) enforced by the
explicit type annotation (42::Int).
As the inner function has an output type of Int, the 
outer function must have an input type of Int. This means 
that the composed function has type (Int -> Int), and that
the entire expression is therefore type correct.


--------------------------------
Question 1.4:
What is the type of the following function g?

g f = (:).f

--------
Answer: 

We know that the function composition operator (.) has the 
type (.) :: (b -> c) -> (a -> b) -> (a -> c), and that the
function (:) has the type (:) :: a -> [a] -> [a]. The
latter function just takes an element of type a and a list
of type [a] and returns a list of type [a], i.e. the 
orignal list with the element prepended to it. This mean 
that the composed function g takes a function f of type
f :: a -> b, an element of type a, and a list of type [b]
and returns a list of type [b]: the original list with the
element f x prepended to it. Thus the type of g is: 

g :: (a -> b) -> a -> [b] -> [b]


--------------------------------
Question 1.5:
What is the most general type of the following function f?

f = \ (x,y) z -> (x (x y), x z)

--------
Answer: 

From the expression we see that x is a unary function since
it is applied to y and z: so the type of x is x :: a -> b.
We also see that x is applied to its own output, so the 
type of x must be such that b is of the same type as a and
therefore x :: a -> a. The function f is a lambda function
which takes a tuple (x,y) and a value z, and returns a
tuple (x (x y), x z). Therefore the most general type of f
is:

f :: (a -> a, a) -> a -> (a, a)

___________________________________________________________

2. Programming in Haskell
___________________________________________________________

Implement a function 

  longestPalsub :: Eq a => [a] -> [a] 
  
such that the call longestPalSub xs returns the longest 
subsequence of xs which is a palindrome. Here, a 
subsequence consists of a consecutive run of elements from 
xs. The time complexity of your solution should not exceed 
O(n^3), where n is the length of xs. You are not allowed to 
use the indexing operator (!!). 

For example:  

  longestPalsub "Be careful to step on no pets he said."

should return: " step on no pets " 
  
  longestPalsub [3,1,4,1,5,9,2,6,5] 
  
should return: [1,4,1]

--------
Answer:

> longestPalsub :: Eq a => [a] -> [a]
> longestPalsub xs = longest (filter isPalin (allSubs xs))

> -- generates all contiguous sublists of a list
> allSubs :: [a] -> [[a]]
> allSubs []       = []
> allSubs (x : xs) = prefs xs [x] ++ allSubs xs
>   where
>     prefs []       acc = [acc]
>     prefs (y : ys) acc = acc : prefs ys (acc ++ [y])

> -- palindrome checker
> isPalin :: Eq a => [a] -> Bool
> isPalin xs = xs == reverse xs

> -- finds the longest list
> longest :: [[a]] -> [a]
> longest []       = []
> longest (x : xs) = longer x (longest xs)
>   where 
>     longer a b = if length a >= length b then a else b


___________________________________________________________

3. Higher-order functions
___________________________________________________________

Question 3.1:
Write a function splitWhen (including its type) which takes 
a predicate p and a list xs and returns a tuple (x,ys,zs) 
such that p x is True, xs = ys++[x]++zs, and p y is False 
for all y in ys. You may assume that p x is True for at 
least one element of xs. 

For example: 

  splitWhen even [1,3,4,5,2,1] 

  should return: (4,[1,3],[5,2,1])

--------
Answer:

> splitWhen :: (a -> Bool) -> [a] -> (a, [a], [a])
> splitWhen p xs = split [] xs
>  where
>    split _ [] = error ("Should not happen: "
>                        ++ "no element satisfies p") 
>    split ys (z : zs)
>      | p z       = (z, reverse ys, zs)
>      | otherwise = split (z : ys) zs

--------------------------------
Question 3.2:
Give an implementation (and its type) of the standard 
Haskell function curry.

--------
Answer:

> curry :: ((a, b) -> c) -> a -> b -> c
> curry f x y = f (x, y)

The function curry transforms a function f that takes a 
tuple (a, b) into a function that takes two arguments x 
and y separately. When calling curry f x y, the function 
f is applied to the pair (x, y).


--------------------------------
Question 3.3:
Implement a funtion map2 (including its type) which takes
a function f and a list of lists xss and outputs the list 
of lists that is obtained by applying f to the elements of 
the lists in xss. For example:

  map2 (*2) [[],[1,2],[5,6]]
  
  should return: [[],[2,4],[10,12]]

--------
Answer:

> map2 :: (a -> b) -> [[a]] -> [[b]]
> map2 f = map (map f)

The outer map applies the inner function to each sublist 
in the list of lists xss, while the inner map f applies f 
to every element of each sublist.


--------------------------------
Question 3.4:
The function count is recursively deﬁned as:

count _ [] = 0
count p (x : xs)
  | p x = 1 + count p xs
  | otherwise = count p xs

Give an implementation of count (including its type) that
does not use recursion nor a list comprehension.

--------
Answer:

> count :: (a -> Bool) -> [a] -> Int
> count p = foldr (\x acc -> if p x then 1+acc else acc) 0

This implementation uses foldr to traverse the list once.
The accumulator is incremented only when the predicate
holds.

Another possible implementation:

> count' p = length . filter p

This last definition can be less efficient for large
inputs as filter first constructs an intermediate list.


--------------------------------
Question 3.5:
Implement the function reverse using foldr.

--------
Answer:

> reverse :: [a] -> [a]
> reverse = foldr (\x acc -> acc ++ [x]) []

Using foldr, we traverse the list and build a new list
by appending each element to the end of the accumulator.


___________________________________________________________

4. List comprehensions
___________________________________________________________

Question 4.1:
Implement the function isSorted :: [Int] -> Bool such that 
isSorted xs is True if and only if the list xs is ascending 
(i.e. each element is less or equal to its successor). 
Make use of a list comprehension together with the function 
and. For example, isSorted [1,2,3,3,4] should return True 
while isSorted [2,1] should return False.

--------
Answer:

> isSorted :: [Int] -> Bool
> isSorted [] = True
> isSorted (x : xs) = 
>   and [x <= y | (x, y) <- zip xs (tail xs)]

This implementation uses a list comprehension to generate 
a list of Boolean values that check whether each element 
is less than or equal to its successor. The zip function 
pairs each element with its successor, and the and 
function checks if all these comparisons are True.

If we are not allowed to use tail, we can simply replace
(tail xs) with (drop 1 xs) 

--------------------------------
Question 4.2:
Use a list comprehension to implement the function 
locations :: Eq a => a -> [a] -> [Int] which takes an 
item x and a list xs ands returns a list of indexes at 
which x is found in xs. Note that the ﬁrst element of a 
list has index 0. For example:

  locations 1 [3,1,4,1,5,9,2,6,5,1] 
  should return: [1,3,9] 

You are not allowed to use the indexing operator (!!).

--------
Answer:

> locations :: Eq a => a -> [a] -> [Int]
> locations x xs = [i | (z, i) <- zip xs [0..], z == x]

In this definition zip xs [0..] pairs each element of xs 
with its index. The list comprehension then filters
those pairs where the element equals x, and constructs a
list of indices i where the condition holds.


--------------------------------
Question 4.3:
Given is the following function fun.

fun p n = concat (map f (filter p [1..n]))
  where f x = map (\y -> (x,y)) [1..x]

Give an equivalent implementation using a list 
comprehension. 
You are not allowed to use concat, filter or map.
Also, give the type of the function fun.

--------
Answer:

> fun :: (Int -> Bool) -> Int -> [(Int, Int)]
> fun p n = [(x, y) | x <- [1..n], p x, y <- [1..x]]


--------------------------------
Question 4.4:
Matrices can be represented in Haskell as lists of lists. 
For example, [[1,2,3],[4,5,6]] represents the 2 x 3
matrix of which the ﬁrst row is [1,2,3] and the second row 
is [4,5,6]. Write a function transpose that takes a matrix 
(i.e. a lists of lists) and returns the transposed matrix. 
For example:

  transpose [[1,2,3],[4,5,6]] 
  should return: [[1,4],[2,5],[3,6]] 

Your solution must make use of list comprehensions combined 
with recursion. You may assume that the input matrix is 
rectangular (i.e. each row has the same length). You are 
not allowed to use the indexing operator (!!).

--------
Answer:

We could easily implement transpose using recursion,
map, and tail like this:

> transp ([] : _) = []
> transp xss      = (map head xss) : transp (map tail xss)

But since the question requires a solution using list
comprehensions, we replace the two parts in the recursive 
case with list comprehensions:

> transpose :: [[a]] -> [[a]]
> transpose ([] : _) = []
> transpose xss      = [x | (x : _)  <- xss] : 
>                       transpose [xs | (_ : xs) <- xss]

___________________________________________________________

5. Inﬁnite lists
___________________________________________________________

Question 5.1:
Given is the inﬁnite list of prime numbers, defined as 
follows:

> primes :: [Integer] 
> primes = sieve [2..]
>   where
>   sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p /= 0]

Use it to deﬁne the inﬁnite list composites::[Integer] 
which is the list of all positive integers which are not 
prime.

--------
Answer:

> composites :: [Integer]
> composites = filterComps [1..] primes
>   where
>     filterComps (x : xs) (p : ps)
>       | x == p    = filterComps xs ps
>       | x < p     = x : filterComps xs (p : ps)
>       | otherwise = filterComps (x : xs) ps

The function filterComps takes two lists: the list of
positive integers and the list of primes. It checks each
element of the first list against the primes and filters
out the primes, returning only the composite numbers. This
implementation is efficient because it only traverses
the list of primes as needed, keeping in lockstep with the
list of positive integers. 


--------------------------------
Question 5.2:
Using zip or zipWith, give a deﬁnition of the inﬁnite list 
fs which is the list of numbers which are deﬁned as:

  F (0) = 0 
  F (1) = 1
  F (n) = 2F (n - 1) + F (n - 2) for n ≥ 2

So, the expression take 10 fs equals 
  [0,1,2,5,12,29,70,169,408,985]. 
  
Your implementation should be such that take n fs has 
an O(n) time complexity.

--------
Answer:

Using zip:

> fs :: [Integer]
> fs = 0 : 1 : [2 * y + x | (x, y) <- zip fs (tail fs)]

Using zipWith:

> fs' = 0 : 1 : zipWith (\x y -> 2 * y + x) fs' (tail fs')

If not allowed to use (tail fs), we can simply replace it
with (drop 1 fs)

--------------------------------
Question 5.3:
Implement the ordered inﬁnite list ds23 of all positive 
integers that can be expressed as 2i · 3j (where i and j 
are non-negative integers). For example, take 15 ds23 
equals [1,2,3,4,6,8,9,12,16,18,24,27,32,36,48].

--------
Answer:

> ds23 :: [Integer]
> ds23 = 1 : merge (map (2*) ds23) (map (3*) ds23)
>   where
>     merge (x : xs) (y : ys)
>       | x < y     = x : merge xs (y : ys)
>       | x > y     = y : merge (x : xs) ys
>       | otherwise = x : merge xs ys

We start with 1, corresponding to 2^0 * 3^0. Then we
recursively generate two infinite lists: one by multiplying
the previous elements by 2, and the other by multiplying
by 3. The merge function combines these two lists while
keeping them ordered and removing duplicates (last case).


___________________________________________________________

6. ADT module
___________________________________________________________

The type Fifo a is an Abstract Data Type (ADT) for FIFO 
queues containing elements of type a. Recall that a Fifo 
queue is a container that works according the First-In-
First-Out principle. Implement a module Fifo which exports
the abstract data type but hides the concrete 
implementation. You may choose yourself a suitable data 
representation for Fifo queues.

The following operations on queues need to be implemented:
• empty: returns an empty queue.
• isEmpty: returns True for an empty queue, 
  otherwise False.
• insert: inserts an element in a Fifo queue.
• retrieve: returns the 'oldest' element from 
  a non-empty Fifo queue.
• delete: returns the ﬁfo that is obtained by removing 
  the 'oldest' element from the queue.
• size: returns the number of elements of the Fifo queue.

--------
Answer:

To turn the below code into a module, you would create
a file named Fifo.hs and start it with the following
module declaration:

module Fifo (
      Fifo, empty, isEmpty, insert, 
      retrieve, delete, size
) where

This line exports the abstract data type Fifo and the 
functions, but does not export the constructor Fo, thus
hiding the concrete implementation details.

Then, you would implement the data type and functions as 
follows:

> data Fifo a = Fo [a]

> empty :: Fifo a
> empty = Fo []

> isEmpty :: Eq a => Fifo a -> Bool
> isEmpty (Fo xs) = xs == []

> insert :: a -> Fifo a -> Fifo a
> insert x (Fo xs) = Fo (xs ++ [x])

> retrieve :: Fifo a -> a
> retrieve (Fo [])       = error "empty queue"
> retrieve (Fo (x : xs)) = x 

> delete :: Fifo a -> Fifo a
> delete (Fo [])       = error "empty queue"
> delete (Fo (_ : xs)) = Fo xs 

> size :: Fifo a -> Int
> size (Fo xs) = length xs

A more efficient implementation of the Fifo queue
could use two lists to represent the queue, one for the
elements that are inserted and one for the elements that
are retrieved. This would allow both insertion and removal
in amortized O(1) time. However, the above implementation
is simpler and meets the requirements of the question.


___________________________________________________________

7. Proof of equality
___________________________________________________________

Consider the following Haskell functions:

f xs ys zs = g xs (ys ++ zs)

g [] ys = []
g (x : xs) ys = ys ++ g xs ys

Prove that 

  p(xs) : length (f xs ys zs) = 
          length xs * length ys + length xs * length zs 

for all ﬁnite lists xs, ys, and zs.

--------
Answer:

We will prove this property by structural induction on the
list xs.

1. Base case:  p([])

    {LHS of p([])}
  length (f [] ys zs)
=   {applying f}
  length (g [] (ys ++ zs))
=   {applying g}
  length ([])
=   {applying length}
  0
=   {0 is absorbing element for * 
     and neutral element for +}
  0 * length ys + 0 * length zs
=   {unapplying length}
  length [] * length ys + length [] * length zs
    {RHS of p([])}

2. Inductive step: p((x : xs))

  Induction hypothesis:
    p(xs) : length (f xs ys zs) = 
            length xs * length ys + length xs * length zs

    {LHS of p((x : xs))}
  length (f (x : xs) ys zs)
=   {applying f}
  length (g (x : xs) (ys ++ zs))
=   {applying g}
  length ((ys ++ zs) ++ g xs (ys ++ zs))
=   {applying lemma}
  length (ys ++ zs) + length (g xs (ys ++ zs))
=   {applying f}
  length (ys ++ zs) + length (f xs ys zs)
=   {induction hypothesis}
  length (ys ++ zs)  
  + length xs * length ys + length xs * length zs
=   {applying lemma}
  length ys + length zs
  + length xs * length ys + length xs * length zs
=   {distributivity}
  (1 + length xs) * length ys + (1 + length xs) * length zs
=   {unapplying length}
  (length (x : xs)) * length ys 
  + (length (x : xs)) * length zs
    {RHS of p(x : xs)}

3. Lemma: 
      q(xs): length (xs ++ ys) = length xs + length ys

  We will prove this by structural induction on xs

  3.1 Base case: q([])
  
    {LHS of q([])}
  length [] + length ys
=   {applying the definition of length}
  0 + length ys
=   {neutral element for +}
  length ys
=   {unapplying the definition of ++}
  length ([] ++ ys)
    {RHS of q([])}

  3.2 Inductive step: q((x : xs))

    Induction hypothesis:
      q(xs): length xs + length ys = length (xs ++ ys)

    {LHS of q((x : xs))}
  length (x : xs) + length ys
=   {applying the defintion of length}
  1 + length xs + length ys
=   {applying induction hypothesis}
  1 + length (xs ++ ys)
    {unapplying the definition of length}
= length (x : (xs ++ ys))
    {unapplying the definition of ++}
= length ((x : xs) ++ ys)
    {RHS of q((x : xs))}

□

 
___________________________________________________________

8. Proof on trees
___________________________________________________________

Given is the data type Tree and the functions inorder, 
and mirror:

  data Tree a = Empty | Node a (Tree a) (Tree a)

  inorder :: Tree a -> [a]
  inorder Empty = []
  inorder (Node x l r) = inorder l ++ [x] ++ inorder r

  mirror :: Tree a -> Tree a
  mirror Empty = Empty
  mirror (Node x l r) = Node x (mirror r) (mirror l)

Prove for all ﬁnite trees t: 

  p(t): reverse(inorder(mirror t)) = inorder t

[Note: If you need one or more lemmas to complete the 
 proof, then prove these lemmas separately. You may use 
 without proof that ++ is an associative operator, and 
 that xs ++ [] = xs.]

--------
Answer:

We will prove property p by structural induction on t.

1. Base case: p(Empty)

    {LHS of p(Empty)}
  reverse(inorder(mirror Empty))
=   {applying mirror}
  reverse(inorder(Empty))
=   {applying inorder}
  reverse([])
=   {applying reverse}
  []
=   {unapplying inorder}
  inorder Empty
    {RHS of p(Empty)}

2. Inductive step: p(Node x l r)

  Induction hypothesis:
    p(l): reverse(inorder(mirror l)) = inorder l
    p(r): reverse(inorder(mirror r)) = inorder r

    {LHS of p(Node x l r)}
  reverse(inorder(mirror (Node x l r)))
=   {applying mirror}
  reverse(inorder(Node x (mirror r) (mirror l)))
=   {applying inorder}
  reverse(inorder(mirror r) ++ [x] ++ inorder(mirror l))
=   {associativity of ++, given without proof}
  reverse ((inorder(mirror r) ++ [x]) ++ inorder(mirror l))
=   {applying lemma reverse(us ++ vs) with 
     us = inorder(mirror r) ++ [x], vs = inorder(mirror l)}
  reverse(inorder(mirror l)) 
  ++ reverse (inorder(mirror r) ++ [x])
=   {applying lemma once more}
  reverse(inorder(mirror l))
  ++ (reverse [x] ++ reverse(inorder(mirror r)))
=   {induction hypothesis}
  inorder l ++ reverse [x] ++ inorder r
=   {applying reverse for singleton [x]}
  inorder l ++ [x] ++ inorder r
=   {unapplying inorder}
  inorder (Node x l r)
    {RHS of p(Node x l r)}

3. Lemma: 
      q(xs) : reverse(xs ++ ys) = reverse ys ++ reverse xs

We will prove this lemma by structural induction on xs.

3.1 Base case: q([])

    {LHS of q([])}
  reverse ([] ++ ys)
=   {applying ++}
  reverse ys
=   {as noted, we may use without proof that xs = xs ++ [],
     with xs = reverse ys}
  reverse ys ++ []
=   {unapplying reverse}
  reverse ys ++ reverse []
    {RHS of q([])}

3.2 Inductive step: q((x : xs))

    Induction hypothesis:
      q(xs) : reverse (xs ++ ys) = reverse ys ++ reverse xs

    {LHS of q((x : xs))}
  reverse ((x : xs) ++ ys)
=   {applying ++}
  reverse (x : (xs ++ ys))
=   {applying reverse}
  reverse (xs ++ ys) ++ [x]
=   {applying induction hypothesis}
  reverse ys ++ reverse xs ++ [x]
=   {unapplying reverse}
  reverse ys ++ reverse (x : xs)
    {RHS of q((x : xs))}

□

___________________________________________________________