> import Prelude hiding (filter)

-----------------------------------------------------------
             RESIT FUNCTIONAL PROGRAMMING 2015               
-----------------------------------------------------------

You may use the standard arithmetic/Boolean operators and 
the standard Haskell functions given in the file 
functions.md throughout the entire exam.

___________________________________________________________

1. Types 
___________________________________________________________

Question 1.1:
What is the type of the following Haskell expression?

  [('4'=='4', 2)]

--------
Answer: 

The expression is a list with one element, which is a tuple
with two components, which are allowed to have different
types. The type of the first component is determined by the
equality operator (==) applied to two characters which 
yields a Boolean value. The type of the second component is
determined by the numeric literal 2, which is polymorphic 
and can be of any type that is an instance of the class 
Num. 
Therefore, the type of the expression is:

  Num a => [(Bool, a)]


--------------------------------
Question 1.2:
What is the most general type of the Haskell function f?

  f = map length

--------
Answer:

The function length takes a list [a] of any type a and 
returns the number of list elements as an Int, so its
type is: [a] -> Int
The function f partially applies map to the function 
length. This means that the first argument of map, which
is a binary function, is fixed to length :: [a] -> Int
Since the type signature of map is 
  map :: (c -> d) -> [c] -> [d]
partially applying it to length yields c = [a] and 
d = Int, resulting in the following signature for f:

  f :: [[a]] -> [Int]

That is, f is a unary function that takes a list of lists
and returns a list of Ints, where each Int represents the
length of the corresponding inner list in the input.


--------------------------------
Question 1.3:
What is the most general type of the Haskell function g?

  g = foldr (+) 0

--------
Answer: 

The function g partially applies foldr to the binary 
function (+) having type (+) :: Num c => c -> c -> c 
and an initial accumulator value of type Num d => d

The type signature of foldr is given by:
  foldr :: (a -> b -> b) -> b -> [a] -> b

Partially applying this function to the above inputs,
yields a = c, b = c, and d = c, resulting in the 
signature:

  g :: Num c => [c] -> c


--------------------------------
Question 1.4:
What is the type of the following Haskell expression?

  (\f -> (\g -> (\x -> f (g x))))

--------
Answer: 

This expression is a lambda function that takes three 
arguments f, g, and x. We see that g is applied to x,
so if x :: a, then g is a unary function g :: a -> b
To the result of this application g x, we then apply f.
So, f's input type must match g's output type, resulting
in:  f :: b -> c
The output type of the entire expression then is the 
output type of f, which is c. Therefore, the type of the 
expression is:

  (b -> c) -> (a -> b) -> a -> c


--------------------------------
Question 1.5:
What is the type of the following Haskell function h?

  h = head . tail . fst

--------
Answer: 

The function h is a composition of three functions: fst,
tail, and head. The function fst takes a tuple (a,b) and
returns its first component, so its type is:
  fst :: (a, b) -> a
In turn, the function tail takes a list [c] and returns 
its tail, and has type: 
  tail :: [c] -> [c]
Lastly, head takes a list [d] and returns its first
element, so its type is:
  head :: [d] -> d

To be able to compose these three functions, the output
type of each function must match the input type of the
next function on the right. Starting with fst, the 
output type is a, which must match the input type of
tail, which is [c]. So we have a = [c]. Next, the output
type of tail is [c], which must match the input type of
head, which is [d]. Therefore, c = d.

As we know, the output of a composition of functions is
a unary function, whose input type is the input type of
the inner function in the composition, and whose output
type is the output type of the outer function. 
Therefore, the type of h is:

  h :: ([d], b) -> d

That is, h takes a tuple whose first component is a list
and outputs the first element of its tail, i.e. its
second element.


___________________________________________________________

2. Programming in Haskell
___________________________________________________________

Write a Haskell function perms (including its type) that 
accepts as its argument a list, and outputs the list of 
lists that are the permutations of the input list. 
For example: 

    perms "abc" 
  = ["abc","acb","bac","bca","cab","cba"]

Note that the order of the elements in the output list 
is not important.

--------
Answer:

The question is not precise about whether the input list
can contain duplicate elements or not. The following
implementation works for lists with distinct elements.

> perms :: Eq a => [a] -> [[a]]
> perms [] = [[]]
> perms xs = [x : ps | x <- xs, 
>                      ps <- perms (filter (/= x) xs)]

The function perms generates all permutations of its input
list xs by peeling off each element x of xs in turn, and
recursively generating all permutations ps of the list that
remains after fitering out x from xs. The final result is
the list of all lists that are obtained by prepending x to
each of the permutations ps. The base case is the list
containing the empty list, which is the only permutation
of the empty list.

If the input list may contain duplicate elements, then the
above implementation does not work, since it filters out
all occurrences of x from xs, instead of just one. The
following implementation works for lists with duplicate
elements as well.

> perms' :: Eq a => [a] -> [[a]]
> perms' [] = [[]]
> perms' xs = [x : ps | x <- xs,
>                       ps <- perms' (delFirst x xs)]

> delFirst :: Eq a => a -> [a] -> [a]
> delFirst _ [] = []
> delFirst y (z:zs) | y == z    = zs
>                   | otherwise = z : delFirst y zs 

The helper function delFirst only removes the first
occurrence of its first argument from its input list. If we 
want to produce only unique permutations in the presence of 
duplicates, we can wrap the list comprehension in nub (from
Data.List) to remove duplicates from the final result, at 
the cost of an extra O(n^2) overhead due to duplicate
removal.

Finally, note that the time complexity of these 
implementations is in O(n!), where n is the length of the 
input list. This is unavoidable since there are n! 
permutations of a list of length n.


___________________________________________________________

3. List comprehensions
___________________________________________________________

Question 3.1:
Implement the function filter (including its type) as a
list comprehension.

--------
Answer:

> filter :: (a -> Bool) -> [a] -> [a]
> filter p xs = [x | x <- xs, p x]

The list comprehension extracts each element from the input
list xs and checks whether the element satisfies the predi-
cate p. If so, the element is included in the output list.


--------------------------------
Question 3.2:
A Pythagorean triad is a triple of positive integers 
(a, b, c) such that a^2 + b^2 = c^2. Write a Haskell 
function pytriads n that returns a list of all Pythagorean
triplets such that 0 < a ≤ b ≤ c ≤ n. This list must be 
ordered based on the value of c.

For example: 
              pytriads 18 
            = [(3,4,5),(6,8,10),(5,12,13),
               (9,12,15),(8,15,17)]

The implementation of pytriads must be a list 
comprehension.

--------
Answer:

> pytriads :: Integer -> [(Integer, Integer, Integer)]
> pytriads n = [(a,b,c) | c <- [1..n], b <- [1..c],
>                         a <- [1..b], a^2 + b^2 == c^2]

The list comprehension generates all triples (a,b,c) such
that 0 < a ≤ b ≤ c ≤ n, and then filters out those that do
not satisfy the Pythagorean condition a^2 + b^2 = c^2.
The order of the output list is determined by the order in
which the elements are generated in the list comprehension. 
Because the outermost generator is c, the resulting list is 
automatically ordered by increasing values of c. 


--------------------------------
Question 3.3:
An integer n is called a perfect number if it is equal to 
the sum of all its proper divisors (excluding n itself). 
For example, 6 is a perfect number, since 6 = 1+2+3. 
An integer n is called a semi-perfect number if it is equal
to the sum of a subset of its proper divisors (excluding n 
itself). An example of a semi-perfect number that is not a 
perfect number is 12, since the proper divisors of 12 are 
1, 2, 3, 4, and 6 which add up to 16, but 2+4+6 = 12.

Write a Haskell function spf n that computes the list of 
all semi-perfect numbers in the range [1..n]. The implemen-
tation must be a list comprehension (which may use helper 
function if needed). 

For example:  spf 50 = [6,12,18,20,24,28,30,36,40,42,48]

--------
Answer:

> -- computes the proper divisors of n
> divs :: Integer -> [Integer]
> divs n = [d | d <- [1..n `div` 2], n `mod` d == 0]

> -- computes all subsets of a list: the power set
> subs :: [a] -> [[a]]
> subs [] = [[]]
> subs (x : xs) = subs xs ++ [x : s | s <- subs xs]

> -- checks whether n is a semi-perfect number
> isSPF :: Integer -> Bool
> isSPF n = any ((== n) . sum) (subs (divs n))

> -- generates all semi-perfect numbers 
> -- in the range [1..n]
> spf :: Integer -> [Integer]
> spf n = [x | x <- [1..n], isSPF x]

The function spf generates all integers in the range
[1..n] and filters out those that are not semi-perfect,
using the helper function isSPF, which computes the
proper divisors of its input using the helper function
divs, generates all subsets of these divisors using the
helper function subs, and checks whether any of these
subsets sums up to the input number.


___________________________________________________________

4. Infinite lists
___________________________________________________________

Question 4.1:
Give a recursive definition of the infinite list tf of non-
empty lists of alternating boolean values that end with the
value True. 

Example:      take 4 tf 
            = [[True], [False, True], [True,False,True], 
               [False,True,False,True]]

--------
Answer:

> tf :: [[Bool]]
> tf = [True] : [(if head b then False else True) : b 
>                | b <- tf]

The first element of the list tf is [True]. Each subsequent 
element is generated by prepending either False or True to
an existing element b of tf, depending on whether the head
of b is True or False, respectively. This ensures that the
resulting list remains alternating and ends with True.


--------------------------------
Question 4.2:
Give a recursive definition of the list bits of all binary 
(character) strings that end with a zero, and do not 
contain two consecutive ones. The order of the strings may
be chosen arbitrarily.
For example, take 8 bits may yield 
["0","10","00","100","010","1010","000","1000"].

--------
Answer:

> bits :: [String]
> bits = "0" : [c : b | b <- bits, c <- ['0','1'], 
>               c == '0' || head b == '0']

The first element of the list bits is the string "0". To
build larger strings, we prepend either '0' or '1' to an
existing string b from bits. We can always prepend '0',
but we can only prepend '1' if the head of b is '0', to
avoid two consecutive ones. This ensures that all strings
in bits end with '0' and do not contain two consecutive
ones.


--------------------------------
Question 4.3:
Give a definition of the function multiples that takes as 
its argument a finite list of positive integers, and 
outputs the infinite list of all multiples of the input 
numbers. Note that the output list must be generated in 
increasing order, and should not contain any duplicates.

Example:    take 14 (multiples [3,5,7]) 
          = [0,3,5,6,7,9,10,12,14,15,18,20,21,24]

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

5. ADT module
___________________________________________________________

The abstract data type Bag tp implements a simple data type
for the storage of elements of the type tp. A bag is also 
known as a multiset, since elements may occur multiple 
times. For example, if we insert some element twice in a 
bag, and remove one of them afterwards, then one element 
still remains in the bag (in contrast with a standard set).

Implement a module Bag such that the concrete 
implementation of the type Bag is hidden from the user.

The following operations on the data type Bag must be 
implemented:
• empty: returns an empty bag.
• isEmpty: returns True for an empty bag, 
  otherwise False.
• insert: returns the bag that is the result 
  of inserting an element.
• del: returns the bag that is obtained 
  by removing an element.
• cardinality: returns the number of occurrences 
  of an element in the bag.
• union: returns the union of two bags.
• intersect: returns the intersection of two bags.

--------
Answer:

To turn the below code into a module, you would create
a file named Bag.hs and start it with the following
module declaration:

module Bag (
  Bag, empty, isEmpty, insert, del, 
  cardinality, union, intersect
) where

This line exports the abstract data type Bag and its 
associated functions, but does not export the constructor
Bag of the data type, thus hiding the concrete 
implementation details.

> data Bag a = Bag [a]

> -- Show instance for pretty printing of bags
> instance (Show a) => Show (Bag a) where
>   show (Bag xs) = "{" ++ showBag xs ++ "}"
>     where
>       showBag []     = " "
>       showBag [x]    = show x
>       showBag (x:xs) = show x ++ "," ++ showBag xs

> -- returns an empty bag
> empty :: Bag a
> empty = Bag []

> -- checks whether a bag is empty
> isEmpty :: Bag a -> Bool
> isEmpty (Bag []) = True
> isEmpty _        = False

> -- inserts an element into a bag
> insert :: a -> Bag a -> Bag a
> insert x (Bag xs) = Bag (x : xs)

> -- removes an element from a bag
> del :: Eq a => a -> Bag a -> Bag a
> del z (Bag xs) = Bag (delFirst z xs)
>   where
>     delFirst _ [] = []
>     delFirst y (x:xs) 
>       | y == x    = xs
>       | otherwise = x : delFirst y xs

> -- computes the number of occurrences 
> -- of an element in a bag
> cardinality :: Eq a => a -> Bag a -> Int
> cardinality y (Bag xs) = length (filter (== y) xs)

> -- computes the union of two bags
> union :: Bag a -> Bag a -> Bag a
> union (Bag xs) (Bag ys) = Bag (xs ++ ys)

> -- computes the intersection of two bags
> intersect :: Eq a => Bag a -> Bag a -> Bag a
> intersect (Bag []) _ = empty
> intersect (Bag (x:xs)) b@(Bag ys)
>   | x `elem` ys = insert x (intersect (Bag xs) (del x b))
>   | otherwise   = intersect (Bag xs) b


Example usage:

ghci> b1 = insert 3 (insert 2 (insert 3 (insert 1 empty)))
ghci> b2 = insert 3 (insert 4 (insert 1 (insert 5 empty)))
ghci> b1
{3,2,3,1}
ghci> b2
{3,4,1,5}
ghci> union b1 b2
{3,2,3,1,3,4,1,5}
ghci> intersect b1 b2
{3,1}
ghci> cardinality 3 b1
2


___________________________________________________________

6. Proof on lists
___________________________________________________________

The definitions of the functions filter, and (++) are given 
in the file functions.md of this exam folder.

Prove the following property q: 

  q(xs):  filter p (xs ++ ys) = filter p xs ++ filter p ys 
          for all finite lists xs and ys

[Note: refer to the file functions.md for the 
 definitions of filter and (++)]

--------
Answer:

We prove the property q by structural induction on the list
xs.

----------------------------------------
Base case: prove q([])
----------------------------------------

    {LHS of q([])}
  filter p ([] ++ ys)
=   {applying (++)}
  filter p ys
=   {unapplying (++)}
  [] ++ filter p ys
=   {unapplying filter}
  filter p [] ++ filter p ys
    {RHS of q([])}  

----------------------------------------
Induction step: prove q(xs) => q((x:xs))
----------------------------------------

  Induction hypothesis:
    q(xs): filter p (xs ++ ys) = filter p xs ++ filter p ys

    {LHS of q((x:xs))}
  filter p ((x:xs) ++ ys)
=   {applying (++)}
  filter p (x : (xs ++ ys))

  The recursive case of filter has two subcases, so in 
  order to continue the proof, we need to do a case
  distinction on whether p x is True or False, and prove
  both cases separately.

    Case 1: p x = True
    Case 2: p x = False

  Case 1: p x = True

    filter p (x : (xs ++ ys))
  =   {applying filter}
    x : filter p (xs ++ ys)
  =   {induction hypothesis q(xs)}
    x : (filter p xs ++ filter p ys)
  =   {unapplying (++)}
    (x : filter p xs) ++ filter p ys
  =   {unapplying filter}
    filter p (x : xs) ++ filter p ys
      {RHS of q((x:xs))}

  Case 2: p x = False
      
    filter p (x : (xs ++ ys))
  =   {applying filter}
    filter p (xs ++ ys)
  =   {induction hypothesis q(xs)}  
    filter p xs ++ filter p ys
  =   {unapplying filter for first term only}
    filter p (x : xs) ++ filter p ys
      {RHS of q((x:xs))}

□


___________________________________________________________

7. Proof on trees
___________________________________________________________

Given is the data type BinTree, and the functions inorder 
and maptree:

  data BinTree a = Empty | Node a (BinTree a) (BinTree a)

  inorder :: BinTree a -> [a]
  inorder Empty = []
  inorder (Node x l r) = inorder l ++ [x] ++ inorder r

  maptree :: (a -> b) -> BinTree a -> BinTree b
  maptree f Empty = Empty
  maptree f (Node x l r) = 
    Node (f x) (maptree f l) (maptree f r)

Prove for all finite trees t: 
  
  p(t): inorder(maptree f t) = map f (inorder t)

Associativity of (++) may be used without proof:

  (xs ++ ys) ++ zs = xs ++ (ys ++ zs)

[Note: refer to the file functions.md for
 definitions of map and (++)]

--------
Answer:

We prove the property p by structural induction on the
tree t.

--------------------------------------
Base case: prove p(Empty)
--------------------------------------

    {LHS of p(Empty)}
  inorder (mapTree f Empty)
=   {applying mapTree}
  inorder Empty
=   {applying inorder}
  []
=   {unapplying map}
  map f []
=   {unapplying inorder}
  map f (inorder Empty)
=   {RHS of p(Empty)}

--------------------------------------
Induction step: prove p(l) ∧ p(r)
                      => p(Node x l r)
--------------------------------------

    Induction hypotheses:
      p(l): inorder (mapTree f l) = map f (inorder l)
      p(r): inorder (mapTree f r) = map f (inorder r)

    {LHS of p(Node x l r)}
  inorder (mapTree f (Node x l r))
=   {applying mapTree}
  inorder (Node (f x) (mapTree f l) (mapTree f r))
=   {applying inorder}
  inorder (mapTree f l) ++ [f x] ++ inorder (mapTree f r)
=   {induction hypothesis p(l)}
  map f (inorder l) ++ [f x] ++ inorder (mapTree f r)
=   {induction hypothesis p(r)}
  map f (inorder l) ++ [f x] ++ map f (inorder r)
=   {unapplying map}
  map f (inorder l) ++ map f [x] ++ map f (inorder r)
=   {applying associativity of (++), without proof}
  map f (inorder l) ++ (map f [x] ++ map f (inorder r))
=   {applying lemma q}
  map f (inorder l) ++ map f ([x] ++ inorder r)
=   {applying lemma q once more}
  map f (inorder l ++ ([x] ++ inorder r))
=   {unapplying inorder, associativity of (++)}
  map f (inorder (Node x l r))
    {RHS of p(Node x l r)}

□

------------------------------------------------
Lemma q

  q(xs): map f (xs ++ ys) = map f xs ++ map f ys
------------------------------------------------

We prove the property q(xs) by structural  
induction on the list xs.

--------------------------------------
Base case: prove q([])
--------------------------------------

    {LHS of q([])}
  map f ([] ++ ys)
=   {applying (++)}
  map f ys
=   {unapplying (++)}
  [] ++ map f ys
=   {unapplying map}
  map f [] ++ map f ys
    {RHS of q([])}

----------------------------------------
Induction step: prove q(xs) => q((x:xs))
----------------------------------------

    Induction hypothesis:
      q(xs): map f (xs ++ ys) = map f xs ++ map f ys

    {LHS of q((x:xs))}
  map f ((x:xs) ++ ys)
=   {applying (++)}
  map f (x : (xs ++ ys))
=   {applying map}
  f x : map f (xs ++ ys)
=   {induction hypothesis}
  f x : (map f xs ++ map f ys)
=   {unapplying (++)}
  (f x : map f xs) ++ map f ys
=   {unapplying map}
  map f (x : xs) ++ map f ys
    {RHS of q((x:xs))}

□

___________________________________________________________