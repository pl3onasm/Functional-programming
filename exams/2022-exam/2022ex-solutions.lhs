> import Prelude hiding (map)

-----------------------------------------------------------
             EXAM FUNCTIONAL PROGRAMMING 2022               
-----------------------------------------------------------

You may use the standard arithmetic/Boolean operators and 
the standard Haskell functions given in the file 
functions.md throughout the entire exam.

___________________________________________________________

1. Types 
___________________________________________________________

Question 1.1:
What is the most general type of the following expression?

[([],[]), ([],[])]

--------
Answer: 

Tuples do not require the elements to be of the same type,
and since the empty list [] can be of any type, the most 
general type for the tuple is ([a], [b]) where a and b can 
be any types. Lists, on the other hand, do require their
elements to be of the same type, so the most general type
of the entire expression is: [([a], [b])]


--------------------------------
Question 1.2:
What is the most general type of the following expression?

[([],"ABC"), ("DEF",[])]

--------
Answer:

The two tuples in the list have different types for their
first and second components as the empty list [] can be of
any type, while the string "ABC" and "DEF" are of type
[Char]. The first tuple has the type ([a], [Char]) 
where a can be any type, and the second tuple has the type 
([Char], [b]) where b can also be any type. 
As lists require their elements to be of the same type, the
type checker will try to unify these types, yielding
a = [Char] and b = [Char].
Hence, the most general type of the entire expression is
[([Char], [Char])]


--------------------------------
Question 1.3:
What is the most general type of the function f?

f g = \ (a,b) -> g a b

--------
Answer: 

We see that the function g is applied to two arguments a
and b, so g must be a function that takes two arguments.
The type of g can be expressed as g :: a -> b -> c, where 
a, b, and c are type variables that can be any type.
The function f takes g as an argument and returns a new
function that takes a tuple (a, b) as an argument.
The type of the tuple has to match the types of the 
arguments of g, so we can express the type of f as:

  f :: (a -> b -> c) -> (a, b) -> c


--------------------------------
Question 1.4:
What is the type of the standard Haskell function foldr?

--------
Answer: 

The function foldr is a higher-order function that takes
three arguments: a binary function, an initial accumulator
value b, and a list [a]. It differs from foldl in how the
accumulator accumulates the result. Both foldr and foldl
visit the input list elements from left to right, and both 
reduce the list to a single value using an accumulator
function. The difference lies in how the accumulator is 
combined with the list elements:

•  foldr applies the function starting from the rightmost 
   element, so the accumulator appears on the right side 
   of the function.

•  foldl applies the function starting from the leftmost 
   element, so the accumulator appears on the left side 
   of the function.

This right-associative behavior of foldr is reflected 
in its type signature:

  foldr :: (a -> b -> b) -> b -> [a] -> b


--------------------------------
Question 1.5:
What is the type of the following Haskell function h?

h f = map f "123" == [['1'],['2'],['3']]

--------
Answer: 

In order to determine the type of the function h, the type
checker will use all the information available in the 
function body. In particular, it will infer the type of
the function f based on how it is used in througout the
expression of the function body, in order to obtain the
most general type of h, which takes f as an argument.

This function f is repeately applied to the characters of 
the string "123" by the map function. Since the equality 
operator (==) is used, the result of the map function must
be of the same type as the list [['1'],['2'],['3']].
Therefore, each application of f must produce a singleton
list of Char, and so the type of f can be expressed as:
  f :: Char -> [Char]

Since the result of the map function is compared to a
list of lists of Char, the type of the entire body of h
is a Bool, and so the function h returns a Bool.
Thus, the type of h can be expressed as:
  h :: (Char -> [Char]) -> Bool


___________________________________________________________

2. Programming in Haskell
___________________________________________________________

In number theory, a Leyland number is a number of the form 
x^y + y^x, where x and y are integers greater than 1. 
The first few Leyland numbers are: 

  8, 17, 32, 54, 57, 100, 145, 177, 320, 368, 
  512, 593, 945, 1124

Write a Haskell function leyland :: Integer -> [Integer] 
such that leyland n yields the ascending list of Leyland 
numbers x^y + y^x, where 1 < x ≤ n and 1 < y ≤ n. 
Note that the list should not contain any duplicates.

--------
Answer:

> merge :: Ord a => [a] -> [a] -> [a]
> merge [] ys = ys
> merge xs [] = xs
> merge (x : xs) (y : ys)
>   | x < y = x : merge xs (y : ys)
>   | x == y = x : merge xs ys
>   | otherwise = y : merge (x : xs) ys

> leyland :: Integer -> [Integer]
> leyland n = foldr merge [] streams
>   where 
>   streams = map (\x-> map (\y-> x^y + y^x) [x..n]) [2..n]

In order to generate Leyland numbers efficiently without 
producing duplicates, we loop over x from 2 to n, and for 
each x, loop over y from x to n. This uniquely covers all 
pairs (x, y) where x ≤ y, avoiding repeats caused by the 
symmetry in the formula x^y + y^x, which would otherwise 
cause the same number to be generated twice (once as
x^y + y^x and once as y^x + x^y).

This nested loop structure is implemented using a double 
map, where the outer map iterates over x and the inner 
map iterates over y. The result is a list of lists, where
each inner list is a strictly increasing sequence of 
Leyland numbers (a stream) for a specific value of x. 

To obtain a single strictly increasing list of Leyland 
numbers from the list of streams, we use foldr in combi-
nation with the merge function. The foldr starts with an
empty list and repeatedly merges in one stream at a time. 
The merge function, in turn, is defined to merge two 
sorted lists into one sorted list, ensuring that the final
result is a single strictly increasing list of Leyland 
numbers.

Note that the second guard in merge removes duplicates, 
which in this case, is technically not necessary since we 
only generate unique Leyland numbers by construction and
work with strictly increasing streams as a result.


___________________________________________________________

3. Higher-order functions
___________________________________________________________

Question 3.1:
Make use of the function map to define a function ahead 
(including its type) which takes a value x of some type, 
and a list of lists of that same type, and it returns the 
list of lists that is obtained by placing x at the front of
every component list. 

For example: 

  ahead 7 [[1,2], [], [3]] = [[7,1,2], [7], [7,3]]

--------
Answer:

> ahead :: a -> [[a]] -> [[a]]
> ahead z = map (z :)

This function uses map to apply the partially applied (:) 
operator to each inner list. It prepends the input element 
z to every list in the input list of lists, producing a 
new list of lists with z at the front of each component.


--------------------------------
Question 3.2:
Consider the following Haskell function digits:

digits n = if n < 10 
           then [n] 
           else n `mod` 10 : digits (n `div` 10)

The function returns the digits of an Integer in reverse 
order, i.e. digits 120 = [0,2,1]. Using the higher-order 
function foldr, define a function dig2int (including its 
type) which is the inverse of the function digits.
So, dig2int (digits 120) = 120.

--------
Answer:

> dig2int :: [Integer] -> Integer
> dig2int = foldr (\x acc -> x + 10 * acc) 0

This function uses foldr to reconstruct the original
integer from the list of digits, which are ordered from
least significant to most significant. The lambda function
essentially shifts the accumulated value one digit position
to the left (by multiplying it by 10) and adds the current
digit x to it, effectively computing the integer value
as follows:

  dig2int [0,2,1]
    = 0 + 10 * (dig2int [2,1])
    = 0 + 10 * (2 + 10 * (dig2int [1]))
    = 0 + 10 * (2 + 10 * (1 + 10 * 0))
    = 0 + 10 * (2 + 10 * 1)
    = 0 + 10 * (2 + 10)
    = 0 + 10 * 12
    = 120


--------------------------------
Question 3.3:
Using function composition (.), foldr, and filter write a 
function reveven (including its type) which takes a list 
of integers, removes all odd numbers, and reverses the 
result. So, reveven [1..10] = [10,8,6,4,2].

--------
Answer:

> reveven :: [Integer] -> [Integer]
> reveven = foldr (\x acc -> acc ++ [x]) [] . (filter even)

The inner function (filter even) first removes all odd 
numbers from the input list. Next, foldr reverses this 
filtered list by appending each element to the end of the 
accumulator. 


___________________________________________________________

4. List comprehensions
___________________________________________________________

Question 4.1:
Give an implementation of the standard Haskell function map 
(including its type) as a list comprehension.

--------
Answer:

> map :: (a -> b) -> [a] -> [b]
> map f xs = [f x | x <- xs]

The list comprehension iterates over each element x in the
input list xs and applies the function f to it, producing a 
new list of the results, having the output type of the 
function f as its element type.


--------------------------------
Question 4.2:
Write a function unary that converts a list of non-negative 
integers into a list of unary 'numbers'. In the unary number 
system, an integer n is represented by a list containing 
n 1s. Your implementation must make use of a list 
comprehension.

As an example:
  unary [1,2,3,5] = [[1],[1,1],[1,1,1],[1,1,1,1,1]]

--------
Answer:

> unary :: [Int] -> [[Int]]
> unary xs = [replicate x 1 | x <- xs]

For each integer x in the input list xs, the list 
comprehension calls replicate x 1, producing a list 
of x ones.


--------------------------------
Question 4.3:
Consider the following function sum4:

  sum4 (w:ws) (x:xs) (y:ys) (z:zs) = 
    w + x + y + z : sum4 ws xs ys zs
  sum4 _ _ _ _ = []

Give an equivalent implementation of sum4 that does not use
recursion, but the function zipWith instead.

--------
Answer:

> sum4 :: Num a => [a] -> [a] -> [a] -> [a] -> [a]
> sum4 as bs cs ds = 
>   zipWith (+) (zipWith (+) as bs) (zipWith (+) cs ds)

The given function sum4 takes four lists and sums the
corresponding elements from each list, producing a new
list of sums. The implementation using zipWith first
sums the first two lists (as and bs) and the last two
lists (cs and ds) separately, and then sums the results
of these two intermediate sums together.


___________________________________________________________

5. Inﬁnite lists
___________________________________________________________

Question 5.1:
Give a Haskell definition for the infinite list 
  ups= [1,1,2,1,2,3,1,2,3,4,1,2,3,4,5,1,2,3,4,5,6,..]

--------
Answer:

> ups :: [Integer]
> ups = concat [[1..n] | n <- [1..]]

A list comprehension is used to generate a list of lists,
where each inner list contains the numbers from 1 to n for
each n starting from 1. The concat function is then used
to flatten this list of lists into a single infinite list.


--------------------------------
Question 5.2:
The Pell numbers P (n) are defined by the recurrence
relation:

  P (0) = 0 
  P (1) = 1
  P (n) = 2P (n - 1) + P (n - 2) for n > 1

Give an expression for the infinite list pells of Pell 
numbers that makes use of zipWith, map, and tail. So:

  take 6 pells = [0,1,2,5,12,29]

--------
Answer:

> pells :: [Integer]
> pells = 0 : 1 : 
>         zipWith (\p2 p1 -> 2*p1 + p2) pells (tail pells)

This definition uses the first two Pell numbers (0 and 1)
as the base cases, and then uses zipWith to compute the
subsequent Pell numbers.
The zipWith function is used to combine pairs of Pell
numbers p2 and p1, where p2 is the Pell number two steps
back in the sequence and p1 is the immediately previous
Pell number. The lambda function takes these two numbers
and applies the recurrence relation to produce the next
Pell number.

The question, however, also asks to use map in the
definition. This can be achieved by zipping with the
(+) operator and using map to apply the recurrence
relation:

> pells' :: [Integer]
> pells' = 0 : 1 :
>          zipWith (+) (map (*2) (tail pells)) pells


--------------------------------
Question 5.3:
Give a Haskell definition for the infinite list natpairs of
pairs (x, y), where x and y are natural numbers such that
x ≥ y. An additional requirement is that if (m,n) is a pair
of natural numbers with m ≥ n, then elem (m,n) natpairs
should return True in finite time. An implementation of 
natpairs that violates this requirement is considered a
non-solution.

--------
Answer:

> natpairs :: [(Integer, Integer)]
> natpairs = [(m, n) | m <- [0..], n <- [0..m]]

The list is ordered by increasing the first component m of
the tuple (m, n) from 0 to infinity. For each m, the 
second component n is iterated from 0 to m, ensuring that
the pairs (m, n) satisfy the condition m ≥ n.

The construction of the list natpairs guarantees that
the pairs are generated in a way that allows for finite
time membership testing: for any pair (m, n), it is 
certain that it will appear after at most m * (m + 1) / 2 
steps, since the number of pairs up to and including m is
the sum of the first m natural numbers.


___________________________________________________________

6. ADT module
___________________________________________________________

The abstract data type (ADT) Fifo tp implements a simple 
data type for the storage of elements of the type tp, from 
which elements are retrieved in the same order as in which 
they are inserted: FIFO stands for First In First Out 
queue.

Implement a module Fifo such that the concrete 
implementation of the type Fifo is hidden from the user.

The following operations on the data type Fifo must be 
implemented:
• empty returns an empty queue.
• isEmpty returns True for an empty queue, otherwise False.
• insert: returns the queue that is the result of inserting 
  an element.
• top: returns the 'oldest' element of the queue 
  (you may assume that the queue is non-empty).
• remove: returns the queue that is obtained by removing 
  the 'oldest' element (you may assume that the queue is
  non-empty).

--------

Answer:

To turn the below code into a module, you would create
a file named Fifo.hs and start it with the following
module declaration:

module Fifo (
  Fifo, empty, isEmpty,
  insert, top, remove
) where

This line exports the abstract data type Fifo and the 
functions, but does not export the constructor FO, thus
hiding the concrete implementation details.

> data Fifo a = FO [a]

> empty :: Fifo a
> empty = FO []

> isEmpty :: Fifo a -> Bool
> isEmpty (FO []) = True
> isEmpty (FO _) = False

> insert :: a -> Fifo a -> Fifo a
> insert x (FO xs) = FO (xs ++ [x])

> top :: Fifo a -> a
> top (FO (x : _)) = x

> remove :: Fifo a -> Fifo a
> remove (FO (_ : xs)) = FO xs


Note: A more efficient ADT would use two lists 
(front and rear) to achieve amortized constant time
complexity for insertions and removals, but the above
implementation meets the requirements.


___________________________________________________________

7. Proof on lists
___________________________________________________________

Prove the following property p:

  p(xs): foldr f e (xs ++ ys) = foldr f (foldr f e ys) xs 
         for all finite lists xs and ys.

[Note: find the definitions of foldr and ++ in the file
       functions.md included in this exam folder]

--------
Answer:

We will prove this property by structural induction on the 
list xs.

-------------------------------
Base case: prove p([])
-------------------------------

    {LHS of p([])}
  foldr f e ([] ++ ys)
=   {applying ++}
  foldr f e ys
=   {unapplying foldr}
  foldr f (foldr f e ys) []
    {RHS of p([])}

-------------------------------
Recursive case: prove p((x:xs))
-------------------------------

    Induction hypothesis:
      p(xs): foldr f e (xs ++ ys) 
             = foldr f (foldr f e ys) xs

    {LHS of p((x : xs))}
  foldr f e ((x : xs) ++ ys)
=   {applying ++}
  foldr f e (x : (xs ++ ys))
=   {applying foldr}
  f x (foldr f e (xs ++ ys))
=   {using induction hypothesis}
  f x (foldr f (foldr f e ys) xs)
=   {unapplying foldr}
  foldr f (f x (foldr f e ys)) (x : xs)
    {RHS of p((x : xs))}

□


___________________________________________________________

8. Proof on trees
___________________________________________________________

Given is the data type Tree and the function mapTree:

  data Tree a = Empty | Node a (Tree a) (Tree a)

  mapTree :: (a -> b) -> Tree a -> Tree b
  mapTree f Empty = Empty
  mapTree f (Node x t1 t2) = 
    Node (f x) (mapTree f t1) (mapTree f t2)

Prove for all finite trees t: 
  
  p(t) : mapTree f (mapTree g t) = mapTree (f.g) t

--------
Answer:

We will prove this property by structural induction on the
tree t.

-----------------------------------
Base case: prove p(Empty)
-----------------------------------

    {LHS of p(Empty)}
  mapTree f (mapTree g Empty)
=   {applying mapTree}
  mapTree f Empty
=   {applying mapTree}
  Empty
=   {RHS of p(Empty)}

-----------------------------------
Recursive case: prove p(Node x l r)
-----------------------------------

    Induction hypothesis:
      p(l): mapTree f (mapTree g l) = mapTree (f . g) l
      p(r): mapTree f (mapTree g r) = mapTree (f . g) r

    {LHS of p(Node x l r)}
  mapTree f (mapTree g (Node x l r))
=   {applying mapTree}
  mapTree f (Node (g x) (mapTree g l) (mapTree g r))
=   {applying mapTree}                        
  Node (f (g x)) (mapTree f l) (mapTree f r)
=   {using induction hypothesis}
  Node (f (g x)) (mapTree (f . g) l) (mapTree (f . g) r)
=   {function composition to obtain (f . g)}
  mapTree (f . g) (Node x l r)
    Node ((f . g) x) (mapTree (f . g) l) (mapTree (f . g) r)
=   {unapplying mapTree}
  mapTree (f . g) (Node x l r)
    {RHS of p(Node x l r)}

□

___________________________________________________________