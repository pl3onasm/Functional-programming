-----------------------------------------------------------
             EXAM FUNCTIONAL PROGRAMMING 2015               
-----------------------------------------------------------

You may use the standard arithmetic/Boolean operators and 
the standard Haskell functions given in the file 
functions.md throughout the entire exam.

___________________________________________________________

1. Types 
___________________________________________________________

Question 1.1:
What is the type of the following Haskell expression?
  
  (1,'2',"3")

--------
Answer: 

Tuples do not require their components to have the same 
type, so this is a valid tuple whose first component is 
a numeric literal of type Num a => a, the second is a
character literal of type Char, and the third is a string
literal of type [Char]. Therefore, we obtain the type:

  Num a => (a, Char, [Char])


--------------------------------
Question 1.2:
What is the most general type of the below Haskell 
function f? 

  f = map not 

--------
Answer:

The function f partially applies map to the Boolean
negation operator not, which has type Bool -> Bool.
The type of map is given by:

  map :: (a -> b) -> [a] -> [b]

Thus, unifying the types, we obtain a = Bool and
b = Bool, resulting in the following signature:

  f :: [Bool] -> [Bool]


--------------------------------
Question 1.3:
What is the most general type of the function g?

  g = (\ (a,b) -> a + b)

--------
Answer: 

The function g is a lambda function that takes a tuple 
and outputs the sum of its components. Since the type
of the (+) operator is  (+) :: Num a => a -> a -> a 
requiring both arguments to be of the same numeric 
type, we can deduce that the components of the tuple
must also be of the same numeric type. Therefore, we
obtain the following type signature:

  g :: Num a => (a, a) -> a


--------------------------------
Question 1.4:
What is the type of the standard Haskell operator (.) 
that is used for function composition?

--------
Answer: 

The (.) operator takes two unary functions as arguments
and produces a new unary function. If the inner function
has type a -> b, then the outer function must have the 
type b -> c, so that the output type of the inner function
matches the input type of the outer function. 
The new function produced by (.) then has type a -> c.
Therefore, the signature of (.) is:

  (.) :: (b -> c) -> (a -> b) -> a -> c


--------------------------------
Question 1.5:
What is the type of the following Haskell function h? 

  h = head . ( : ['a']) 

--------
Answer: 

The cons operator (:) takes an element of a certain type 
and a list of elements of the same type, and prepends the
element to the input list, resulting in the type:

  (:) :: a -> [a] -> [a]

The function h partially applies this operator to a list of
Char, so that the type becomes: 

  (: ['a']) :: Char -> [Char]

This function is composed with the function head, which 
retrieves the first element of the input list, and has 
type:  head :: [b] -> b 

In order for the composition to work, the output type of 
the inner function must match the input type of the outer
function, so that we have b = Char. The result of the 
composition is then a unary function that takes a Char and
outputs a Char:

  h :: Char -> Char 

That is, h expects an element of type Char, which it 
prepends to the list ['a'] and then retrieves it again from
the list that was just expanded.


___________________________________________________________

2. Programming in Haskell
___________________________________________________________

Write a Haskell function middleElement (including its type) 
that computes the middle element of a list. The function 
should return Nothing in case there is no middle element, 
and Just m in case m is the middle element, so

• middleElement [1,2,3] should return Just 2
• middleElement [1,2] should return Nothing

You are not allowed to use the function length 
(nor are you allowed to implement it yourself).

[Hint: A possible solution uses a recursive helper 
function mid xs ys, of which the first argument 'shrinks' 
faster than the second argument during the recursion. 
Define middleElement xs = mid xs xs where mid = .... ]

--------
Answer:

> middleElement :: [a] -> Maybe a
> middleElement xs = mid xs xs
>   where
>     mid []            _       = Nothing 
>     mid [_]          (z : _)  = Just z 
>     mid (_ : _ : ys) (_ : zs) = mid ys zs 

The function middleElement takes a list xs and calls the
recursive helper function mid with the same list for both
arguments. The first argument is the list that is 'shrunk'
by two elements at each recursive call, while the second
argument is the list that is 'shrunk' by one element at 
each recursive call.

The base case of mid is when the first argument is empty,
which occurs when the input list has an even number of
elements. In that case, the function returns Nothing.
The second base case occurs when the first argument has
exactly one element, which occurs when the input list has
an odd number of elements. In that case, the function 
returns Just z, where z is the first element of the second 
argument.

The recursive case of mid is when the first argument has
at least two elements, in which case the function calls
itself with the first argument 'shrunk' by two elements
and the second argument 'shrunk' by one element. This
ensures that the first argument will eventually become
empty, and at that point, the second argument will have
the middle element of the input list as its first element
(if the input list has an odd number of elements).


___________________________________________________________

3. Programming using list comprehensions and 
   higher-order functions
___________________________________________________________

Counting sort is a well-known algorithm for sorting a list 
of small integers. In this problem, you may assume that the 
input is a list of digits (i.e. [0..9]). The algorithm 
consists of two passes. In the first part, a histogram of 
the input is computed: for each possible value, the number 
of occurrences of this value is computed. In the second 
pass, using the histogram from the first pass, the sorted 
output is produced.

Give an implementation of counting sort that uses solely 
list comprehensions and higher order functions. The use of
recursion is not allowed.

Example: 

  countingSort [5,2,1,0,5,4,3,2,5,6,7,8,9,0,9,8,7,8,9,7,6]
  = [0,0,1,2,2,3,4,5,5,5,6,6,7,7,7,8,8,8,9,9,9]

--------
Answer:

> countingSort :: [Int] -> [Int]
> countingSort xs = 
>   concat [replicate n d | (d, n) <- histogram xs]

> histogram :: [Int] -> [(Int,Int)]
> histogram ds = [(d, length $ filter (== d) ds)
>                 | d <- [0..9]]

The function countingSort first computes the histogram of
the input list xs. The histogram is a list of tuples, where
each tuple contains a digit d and the number of occurrences
n of that digit in the input list. It is important to note 
that the histogram is ordered by the digit values, so that
the first tuple corresponds to the digit 0, the second to
the digit 1, and so on up to the digit 9.

This histogram is then used to construct the sorted output 
list by extracting each digit d and replicating it n times,
where n is the number of occurrences of d in the input list.
The resulting lists of replicated digits are then 
concatenated together to form the final sorted list.

Since concat is not listed in the file functions.md, we can 
simply replace it with: foldr ++ []


___________________________________________________________

4. List comprehensions
___________________________________________________________

Question 4.1:
Write a function palindromes (including its type) that 
accepts as its input a list of strings, and returns a list 
of all strings which are palindromes that can be con-
structed by concatenating two strings from the input list.

For example, palindromes ["a", "ab", "abb", "ac", "ca"] 
should return a list containing the strings (in any order):
"aa","aca","aba","abba","aca","acca", and "caac" 

The implementation must be a list comprehension.

--------
Answer:

> palindromes :: [String] -> [String]
> palindromes ss = [z | x <- ss, y <- ss, let z = x ++ y,
>                       z == reverse z]

The list comprehension considers all possible pairs of 
strings (x, y) from the input list ss, concatenates them 
to form the string z, and includes z in the output list if 
z is equal to its reverse, indicating that it is a 
palindrome.


--------------------------------
Question 4.2:
Give a definition of the list fun42 which contains 42 
elements. These elements are functions of the type 
Integer -> Integer. The first element is the function that 
adds 0 to its argument, the second adds 1 to its argument, 
and so on: the i-th element adds i - 1 to its argument. 
The definition of fun42 must be a list comprehension.
For example, (fun42!!5) 10 should return 15.

--------
Answer:

> fun42 :: [Integer -> Integer]
> fun42 = [(+ d) | d <- [0..41]]

The list comprehension iterates d over the range [0..41],
and for each d, it creates the partially applied function
(+ d), which adds d to its argument. This results in a list
of 42 functions, where the i-th function adds i - 1 to its
argument.


--------------------------------
Question 4.3:
Write a function triples (including its type) that accepts 
as its input an integer n, and returns the lexico-
graphically sorted list of all triples (a,b,c) such that 
a + b + c = n and 0 ≤ a < b < c. The implementation must 
be a list comprehension.
Example: 
          triples 8 
        = [(0,1,7),(0,2,6),(0,3,5),(1,2,5),(1,3,4)]

[Note: you can earn 3 points for a correct implementation, 
or 4 points for an efficient correct implementation]

--------
Answer:

> triples :: Integer -> [(Integer,Integer,Integer)]
> triples n = [(x, y, n - x - y) | x <- [0..n `div` 3], 
>               y <- [x+1..(n-x) `div` 2]]

To make the implementation more efficient, we can restrict
the ranges of x and y. If we fix the a value to x, then 
the smallest possible values for b and c are x+1 and x+2,
respectively. Thus, we have the inequality:
  x + (x+1) + (x+2) ≤ n ⟺ 3x + 3 ≤ n ⟺ x ≤ (n-3)/3
  ⟺ x ≤ n `div` 3
This means that the range of x can be restricted to
[0..n `div` 3]. This ensures that there is always
enough 'room' for b and c to be strictly larger than x.

Once x is fixed, the sum condition becomes x + b + c = n, 
so that c = n - x - b. This means that the inequality 
b < c now becomes: 
  b < (n - x) - b ⟺ 2b < n - x ⟺ b < (n - x) `div` 2 

Since b must also be strictly larger than x, we can set 
the range of b to be [x+1..(n-x) `div` 2]. 
The third element c is then computed as c = n - x - y. 
We only need to ensure that c > y, but this is already 
guaranteed by the upper bound on y, so no further 
condition is needed.


___________________________________________________________

5. Infinite lists
___________________________________________________________

Question 5.1:
Give a definition of the infinite list 
[[1], [1,2], [1,2,3], [1,2,3,4], ....]

--------
Answer:

> nums :: [[Integer]]
> nums = map (\x -> [1..x]) [1..]

This function uses map to apply a lambda function to each
integer x in the infinite list [1..]. This lambda function
replaces x with the list of integers from 1 to x, resulting
in the desired infinite list of lists.


--------------------------------
Question 5.2:
Define the infinite list fibs, which is the infinite list of 
fibonacci numbers. Recall that: 

  fib(0) = 0 
  fib(1) = 1
  fib(n) = fib(n-1) + fib(n-2) for n > 1 
  
Example:   take 10 fibs = [0,1,1,2,3,5,8,13,21,34]

--------
Answer:

> fibs :: [Integer]
> fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

The definition starts with the two base cases of the
fibonacci sequence, 0 and 1. Then, it uses zipWith to
combine the list fibs with its tail: this way each
element from two places earlier is added to the element
from one place earlier, thus producing the next fibonacci
number. This process continues indefinitely, generating
the entire infinite list of fibonacci numbers.


--------------------------------
Question 5.3:
Define the infinite list abc, which is the infinite list of
all non-empty strings over the alphabet {'a','b','c'}. 
This list needs to be sorted based on the length of the 
strings. Moreover, strings of equal length should be sorted
lexicographically (dictionary order). 

Example: 
            take 15 abc 
          = ["a","b","c","aa","ab","ac",
             "ba","bb","bc","ca","cb",
             "cc","aaa","aab", "aac"]

--------
Answer:

> abc :: [String]
> abc = "a" : "b" : "c" : [s ++ [c] | s <- abc, c <- "abc"]

The definition starts with the three single-character
strings "a", "b", and "c". Then, it uses a list 
comprehension to generate all longer strings: for each 
string s already in the list abc, it appends each character 
c from the alphabet "abc" to s, in that order. This results
in new strings of length one greater than s, thus con-
tinuing the pattern of generating strings in order of in-
creasing length, and lexicographically within each length.


___________________________________________________________

6. ADT module
___________________________________________________________

The abstract data type (ADT) PQ tp implements a simple 
data type for the storage of elements of the type tp.
The name PQ stands for Priority Queue. Elements can be 
inserted in such a queue in arbitrary order. However, 
retrieving an element from a non-empty priority queue 
always yields the smallest element.

Implement a module PQ such that the concrete  
implementation of the type PQ is hidden from the user.

The following operations on the data type PQ must be 
implemented:
• empty: returns an empty priority queue.
• isEmpty: returns True for an empty priority queue, 
  otherwise False.
• insert: returns the queue that is the 
  result of inserting an element.
• getmin: returns the 'smallest' element of the queue.
• remove: returns the queue that is obtained by 
  removing the smallest element.

--------
Answer:

To turn the below code into a module, you would create
a file named PQ.hs and start it with the following
module declaration:

module PQ (
  PQ, empty, isEmpty, insert, 
  getmin, remove
) where

This line exports the abstract data type PQ and its
associated functions, but does not export the constructor
Pq of the data type, thus hiding the concrete 
implementation details.

> data PQ a = Pq [a] 
>   deriving Show

> -- returns an empty priority queue
> empty :: PQ a
> empty = Pq []

> -- checks if the priority queue is empty
> isEmpty :: PQ a -> Bool
> isEmpty (Pq []) = True
> isEmpty _       = False

> -- inserts an element into the priority queue
> insert :: Ord a => a -> PQ a -> PQ a
> insert x (Pq xs) = Pq (ins x xs)
>   where
>     ins y [] = [y]
>     ins y (z : zs)
>       | y <= z    = y : z : zs
>       | otherwise = z : ins y zs

> -- retrieves the minimum from the priority queue
> getmin :: Ord a => PQ a -> a
> getmin (Pq [])    = error "getmin: empty priority queue"
> getmin (Pq (x : _)) = x

> -- removes the minimum from the priority queue
> remove :: Ord a => PQ a -> PQ a
> remove (Pq [])     = error "remove: empty priority queue"
> remove (Pq (_ : xs)) = Pq xs


Example usage:

ghci> q = insert 5 (insert 3 (insert 7 empty))
ghci> q
Pq [3,5,7]
ghci> getmin q
3
ghci> q1 = remove q
ghci> q1
Pq [5,7]
ghci> getmin q1
5


___________________________________________________________

7. Proof on lists
___________________________________________________________

Given are the definitions of the functions take, and drop:

  take :: Int -> [a] -> [a]
  take 0 xs = []
  take n [] = []
  take n (x:xs) = x:take (n-1) xs

  drop :: Int -> [a] -> [a]
  drop 0 xs = xs
  drop n [] = []
  drop n (x:xs) = drop (n-1) xs

Prove the following property p: 

  p(xs):  take n xs ++ drop n xs = xs 
          for all finite lists xs and n ≥ 0

--------
Answer:

We prove the property p(xs) by structural induction on the
list xs.

----------------------------------------
Base case: prove p([])
----------------------------------------

    {LHS of p([])}
  take n [] ++ drop n []
=   {definition of take}
  [] ++ drop n []
=   {definition of drop}
  []
    {RHS of p([])}

----------------------------------------
Induction step: prove p(xs) => p((x:xs))
----------------------------------------

    Induction hypothesis:
      p(xs): take n xs ++ drop n xs = xs

    Since the definitions of take and drop
    distinguish the cases n = 0 and n > 0,
    in the case of a non-empty list, we
    need to do a case analysis on n.
    
  Case 1: n = 0

    {LHS of p((x:xs)) for n = 0}
  take 0 (x:xs) ++ drop 0 (x:xs)
=   {applying take}
  [] ++ drop 0 (x:xs)
=   {applying drop}
  [] ++ (x:xs)
=   {applying ++}
  (x:xs)
    {RHS of p((x:xs)) for n = 0}  

  Case 2: n > 0

    {LHS of p((x:xs)) for n > 0}
  take n (x:xs) ++ drop n (x:xs)
=   {applying take}
  (x : take (n-1) xs) ++ drop n (x:xs)
=   {applying drop}    
  (x : take (n-1) xs) ++ drop (n-1) xs
=   {applying ++}
  x : (take (n-1) xs ++ drop (n-1) xs)  
=   {induction hypothesis p(xs) for n-1 ≥ 0}
  (x : xs)
    {RHS of p((x:xs)) for n > 0}

□


___________________________________________________________

8. Proof on trees
___________________________________________________________

Given is the data type Tree:

  data BinTree a = Empty | Node a (BinTree a) (BinTree a)

  Given are the functions inorder and mirror:

  mirror :: BinTree a -> BinTree a
  mirror Empty = Empty
  mirror (Node x l r) = Node x (mirror r) (mirror l)

  inorder :: BinTree a -> [a]
  inorder Empty = []
  inorder (Node x l r) = inorder l ++ [x] ++ inorder r

Prove for all finite trees t: 
  
  p(t): reverse(inorder(mirror t)) = inorder t

[Note: If you need one or more lemmas to complete the 
 proof, then prove these lemmas separately. You may use 
 without proof that ++ is an associative operator, and 
 that xs ++ [] = xs. Definitions of reverse, and ++ are 
 given in the file functions.md]

--------
Answer:

We will prove property p by structural induction on t.

-----------------------------------------
1. Base case: prove p(Empty)
-----------------------------------------

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

-----------------------------------------
2. Inductive step: prove p(l) ∧ p(r) 
                         => p(Node x l r)
-----------------------------------------

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

□

----------------------------------------------------------
3. Lemma: 
      q(xs) : reverse(xs ++ ys) = reverse ys ++ reverse xs
----------------------------------------------------------

We will prove this lemma by structural induction on xs.

----------------------------------------------
3.1 Base case: prove q([])
----------------------------------------------

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

----------------------------------------------
3.2 Inductive step: prove q(xs) => q((x : xs))
----------------------------------------------

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