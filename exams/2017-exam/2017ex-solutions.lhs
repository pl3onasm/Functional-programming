> import Prelude hiding (repeat, concat, filter)

-----------------------------------------------------------
             EXAM FUNCTIONAL PROGRAMMING 2017               
-----------------------------------------------------------

You may use the standard arithmetic/Boolean operators and 
the standard Haskell functions given in the file 
functions.md throughout the entire exam.

___________________________________________________________

1. Types 
___________________________________________________________

Question 1.1:
What is the most general type of the following expression? 

  [(('A', "BC"), [True])]

--------
Answer: 

Tuples do not have to have their components of the same 
type. In this case, this means we can use a different type 
variable for each component of a tuple because each of them
has a different type. Since the list contains only one
element, all elements have the same type and no unification
is needed. The first component of the tuple is a pair of a
character and a string, and the second component is a list
of Booleans. Therefore, the most general type of the
expression is:

  [((Char, [Char]), [Bool])]

or:

  [((Char, String), [Bool])]


--------------------------------
Question 1.2:
What is the most general type of the following expression?

  [id, abs]

--------
Answer:

Both id and abs are unary functions of the type a -> a and
Num a => a -> a respectively. As a list requires its
elements to have the same type, the type checker will try to
unify these two types, which means that it will try to find
a common type for the type variable a. The only way to do
this is to restrict a to the type class Num. Therefore, the
most general type is:

  Num a => [a -> a]


--------------------------------
Question 1.3:
What is the most general type of the function f?

  f g (x,y) = g x y

--------
Answer: 

The function f takes as first argument a binary function g,
which is applied to the two components x and y of the
second argument, which is a tuple. As a tuple can contain
elements of different types, we need two different type
variables for x and y. The type of g must therefore be a
function that takes two arguments of different types.
The output of f is the result of applying g to x and y, 
meaning that the output type of f must match the output
type of g.
Therefore, the most general type of f is:

  f :: (a -> b -> c) -> (a, b) -> c


--------------------------------
Question 1.4:
What is the type of the function map?

--------
Answer: 

The function map takes as first argument a unary function f
and as second argument a list xs. Its output is a list 
containing the results of applying f to each element of xs.
Therefore, the type of map is:

  map :: (a -> b) -> [a] -> [b]


--------------------------------
Question 1.5:
What is the type of the following Haskell function h?

  h f g x y = f (g x y) x

--------
Answer: 

First, we see that g is a binary function that is applied 
to arguments x and y. The output of g is then used as
first argument of the binary function f, with x as second
argument. This means that the output type of g must match
the type of the first argument of f, and the type of x
must match the type of the second argument of f.
Therefore, the most general type of h is:

  h :: (a -> b -> c) -> (b -> d -> a) -> b -> d -> c


___________________________________________________________

2. Programming in Haskell
___________________________________________________________

The Luhn algorithm is a simple checksum formula used to 
validate a credit card number. It works as follows.
Let n be the number to be checked, e.g. n = 49927398716. 

The first step is to double every second digit, starting 
from the right (so, in the example we only consider the 
digits .9.2.3.8.1.). If doubling a digit results in a 
number greater than 9 then subtract 9 from the number. 
For this example, the result of this first step is the 
number x = 49947697726. 

In the second step of the algorithm, all digits of x are 
summed up. For the example, we get:
4 + 9 + 9 + 4 + 7 + 6 + 9 + 7 + 7 + 2 + 6 = 7
If this sum is evenly divisible by 10, then the number is 
a valid credit card number, otherwise it is invalid. The 
conclusion is that the example number is a valid credit 
card number.

Write a Haskell function isValidNumber :: Integer -> Bool 
that returns True if and only if its first argument is a 
valid credit card number. 

Example:    isValidNumber 49927398716 = True

--------
Answer:
 
> isValidNumber :: Integer -> Bool
> isValidNumber n = sum (altDbl(revDigits n)) `mod` 10 == 0
>   where 
>     altDbl xs = [if even i then x else luhnDbl x
>                  | (i, x) <- zip [0..] xs]
>     revDigits 0 = []
>     revDigits n = n `mod` 10 : revDigits (n `div` 10)
>     luhnDbl x = if x < 5 then 2*x else 2*x - 9

The function isValidNumber takes an integer n and returns 
True if n is a valid credit card number according to the 
Luhn algorithm. 
It first converts n into a reversed list of digits using 
revDigits, so that the rightmost digit of n becomes the 
first element of the list. The function altDbl then doubles
every second digit in this reversed list (i.e., digits at 
odd indices), using luhnDbl to subtract 9 whenever the 
doubling exceeds 9. 
Finally, the digits are summed and the sum is checked 
modulo 10; if the result is 0, the number is valid.


___________________________________________________________

3. Higher-order functions
___________________________________________________________

Question 3.1:
Using the higher-order functions foldr and map, define a 
function powersum (including its type) which takes two
non-negative integers n and e. It returns the sum of the 
first n positive integers raised to the power e.

So: powersum 4 3 = 1^3 + 2^3 + 3^3 + 4^3 
                 = 1 + 8 + 27 + 64 
                 = 100

--------
Answer:

> powersum :: Integer -> Integer -> Integer
> powersum n e = foldr (+) 0 (map (^e) [1..n])

The list [1..n] generates the first n positive integers.
The function map (^e) raises each of these integers to
the power e. Finally, foldr (+) 0 sums up all the elements
of the resulting list, starting from the initial value 0.


--------------------------------
Question 3.2:
Define the function filter (including its type) using the 
function foldr.

--------
Answer:

> filter :: (a -> Bool) -> [a] -> [a]
> filter p = 
>   foldr (\x acc -> if p x then x : acc else acc) []

The function filter takes a predicate p and a list, and
returns a new list containing only those elements that
satisfy p. It uses foldr to process the input list from 
right to left and applies a lambda function to each element
of the input list to decide whether to include it in the
resulting list or not. 


--------------------------------
Question 3.3:
Using function composition (.), foldr, map and the identity 
function id, write a function pipeline (including its type)
which given a list of functions, each of type a -> a, will 
form a pipeline function of type [a] -> [a].
In such a pipeline, each function in the original function 
list is applied in turn to each element of the input 
(assume the functions are applied from right to left in 
this case). 

For example:   pipeline [(+1),(*2)] [1,2,3] = [3,5,7]

--------
Answer:

> pipeline :: [a -> a] -> [a] -> [a]
> pipeline fs = map (foldr (.) id fs) 

The function pipeline takes a list of functions fs and
returns a new function that applies each function in fs to
each element of an input list. First, it uses foldr to 
compose the functions in fs from right to left, starting 
with the identity function id, into a single function. 
Then, it uses map to apply this composed function to each 
element of the input list.


___________________________________________________________

4. List comprehensions
___________________________________________________________

Question 4.1:
Give an implementation of the standard Haskell function 
concat (including its type) as a list comprehension. 
Recall that: 
  concat [[1,2],[],[3]] = [1,2,3] 
  concat ["hello", "world"] = "helloworld"

--------
Answer:

> concat :: [[a]] -> [a]
> concat xss = [x | xs <- xss, x <- xs]

The function concat takes a list of lists xss and flattens
it into a single list containing all the elements of the 
inner lists. This is achieved using a list comprehension
that iterates over each inner list xs in xss, and then
over each element x in xs, collecting all elements x into
the resulting list.


--------------------------------
Question 4.2:
Show how the single comprehension 
  [(x,y) | x <- [0..m], y <- [0..n]] 
  
with two generators can be re-expressed using two 
comprehensions with single generators. 
[Hint: make use of the library function concat.]

--------
Answer:

The single comprehension can be re-expressed as:

  concat [[(x,y) | y <- [0..n]] | x <- [0..m]]

The outer comprehension iterates over each x in the list
[0..m], and for each x, the inner comprehension generates
a list of pairs (x,y) for each y in the list [0..n].
Finally, concat is used to flatten the list of lists into
a single list of pairs.


--------------------------------
Question 4.3:
The dot product of two vectors 

  a = [a₀, a₁, ..., aₙ] 
  b = [b₀, b₁, ..., bₙ]

is defined as:

  a . b = ∑_{i=1}^n (aᵢ * bᵢ) =
          a₀*b₀ + a₁*b₁ + ... + aₙ*bₙ  

Implement the haskell function dotProduct using a list 
comprehension. It takes two lists as input, and returns
their dot product as output. You may assume that the 
input lists have the same length.

--------
Answer:

> dotProduct :: Num a => [a] -> [a] -> a
> dotProduct xs ys = sum [x * y | (x, y) <- zip xs ys]

The function dotProduct takes two lists xs and ys of the
same length and computes their dot product. It uses the
zip function to pair up corresponding elements from xs and
ys into tuples (x, y). Then, a list comprehension is used
to generate a list of products x * y for each pair (x, y).
Finally, the sum function computes the total sum of these
products, which is the dot product of the two vectors.  


___________________________________________________________

5. Infinite lists
___________________________________________________________

Question 5.1:
Give a recursive expression for the list pairs which is the
infinite list of pairs (n,n+1), where n ranges over the
natural numbers. For example:

  take 3 pairs = [(0,1),(1,2),(2,3)]

--------
Answer:

> pairs :: [(Integer, Integer)]
> pairs = (0, 1) : [(x+1, y+1) | (x, y) <- pairs]

The infinite list starts with the pair (0, 1). For every
existing pair (x, y) in pairs, it then generates (x+1, y+1)
and appends it to the list. This recursively continues 
indefinitely.

Alternatively, we can define the infinite list without a 
list comprehension as follows:

> pairs' = ps 0
>   where ps n = (n, n+1) : ps (n+1)

Here, the helper function ps takes a natural number n and
constructs the list of pairs starting from (n, n+1). It
then keeps recursively calling itself with n+1 to generate 
the next pairs.


--------------------------------
Question 5.2:
Give a definition of the function gendups which takes a 
(possibly infinite) list of positive Integers and generates 
the (posssibly infinite) list where each element x of the 
input list has been replaced by x copies of itself. 

For example:
  gendups [1,2,1,3] = [1,2,2,1,3,3,3]
  gendups [1..] = [1,2,2,3,3,3,4,4,4,4,....]

--------
Answer:

> gendups :: [Integer] -> [Integer]
> gendups xs = concat (map (\x -> rep x x) xs)
>   where
>     rep x 0 = []
>     rep x n = x : rep x (n-1)

The function gendups takes a list of positive integers xs
and uses map to replace each integer x in xs with a list
containing x copies of x.
This is done using the helper function rep, which takes an
integer x and a count n, and recursively constructs a list
with n copies of x. Finally, concat is used to flatten the
list of lists into a single list.

Note that we had to define a helper function rep that does 
the same as the standard function replicate, because the 
standard function takes an Int as first argument, while we
need an Integer here. 


--------------------------------
Question 5.3:
Define the function sums::[Integer] -> [Integer], that 
takes an infinite list of Integers and produces the 
corresponding infinite list of prefix sums.

For example: sums [0,2..] should produce the infinite list 
             [0, 0+2, 0+2+4, ...] = [0, 2, 6, ...]

--------
Answer:

> sums :: [Integer] -> [Integer]
> sums [] = []
> sums (x : xs) = x : map (+ x) (sums xs)

The function sums takes a list of integers and returns a
new list where each element is the sum of all previous
elements in the input list up to that point. It does this
recursively: the first element of the output list is just 
the first element of the input list, and for the rest of
the output list, we take the prefix sums of the tail xs
and add the first element x to each of them using map.


___________________________________________________________

6. ADT module
___________________________________________________________

The type Stack a is an Abstract Data Type (ADT) for stacks 
containing elements of the type a. Recall that a stack is 
a container that works according the LIFO (Last In First 
Out) principle. In other words, the element that was most
recently inserted by a push operation is returned by a top 
operation.

Implement a module Stack such that the concrete 
implementation of the type Stack is hidden to the user. 
You may choose yourself a suitable data representation for 
stacks.

The following operations on stacks need to be implemented:
• empty returns an empty stack.
• isEmpty returns True for an empty stack, 
  otherwise False.
• push: returns the stack that is obtained by 
  adding an element to the stack.
• pop: returns the stack that is obtained by 
  removing the top element from the stack.
• top: returns the element that was most 
  recently added to the stack.

--------
Answer:

To turn the below code into a module, you would create
a file named Stack.hs and start it with the following
module declaration:

module Stack (
  Stack, empty, isEmpty, 
  push, pop, top
) where

This line exports the abstract data type Stack and its 
associated functions, but does not export the constructor
St of the data type, thus hiding the concrete 
implementation details.

> data Stack a = St [a]
>   deriving Show

> -- returns an empty stack
> empty :: Stack a
> empty = St []

> -- checks if the stack is empty
> isEmpty :: Stack a -> Bool
> isEmpty (St []) = True
> isEmpty _       = False

> -- adds an element to the stack
> push :: a -> Stack a -> Stack a
> push x (St xs) = St (x : xs)

> -- removes the top element from the stack
> pop :: Stack a -> Stack a
> pop (St [])       = error "pop from empty stack"
> pop (St (_ : xs)) = St xs

> -- returns the top element of the stack
> top :: Stack a -> a
> top (St [])      = error "top from empty stack"
> top (St (x : _)) = x


Example usage:

ghci> s1 = push 'a' (push 'b' (push 'c' empty))
ghci> top s1
'a'
ghci> s2 = pop s1
ghci> top s2
'b'
ghci> isEmpty s2
False
ghci> isEmpty (pop (pop s2))
True


___________________________________________________________

7. Proof on lists
___________________________________________________________

Prove the following property p: 

  p(xs):  map (f.g) xs = (map f . map g) xs 
          for all finite lists xs

[Note: refer to the file functions.md for the 
 definitions of map and (.)]

--------
Answer:

We prove the property p(xs) by structural induction on the
list xs.

----------------------------------------
Base case: prove p([])
----------------------------------------

    {RHS of p([])}
  (map f . map g) []
=   {applying (.): f . g = \x -> f (g x)}
  map f (map g [])
=   {applying map}
  map f []
=   {applying map}
  []
=   {unapplying map}
  map (f.g) []
=   {LHS of p([])}

----------------------------------------
Induction step: prove p(xs) => p((x:xs))
----------------------------------------

    Induction hypothesis:
      p(xs): map (f.g) xs = (map f . map g) xs

    {RHS of p((x:xs))}
  (map f . map g) (x:xs)
=   {applying (.)}
  map f (map g (x:xs))
=   {applying map}
  map f (g x : map g xs)
=   {applying map}
  f (g x) : map f (map g xs)
=   {unapplying (.)}
  (f . g) x : (map f . map g) xs
=   {induction hypothesis}
  (f . g) x : map (f . g) xs
=   {unapplying map}
  map (f . g) (x:xs)
=   {LHS of p((x:xs))}

□


___________________________________________________________

8. Proof on trees
___________________________________________________________

Given is the data type Tree and the functions inorder, 
and mapTree:

  data Tree a = Empty | Node a (Tree a) (Tree a)

  inorder :: Tree a -> [a]
  inorder Empty = []
  inorder (Node x l r) = inorder l ++ [x] ++ inorder r

  mapTree :: (a -> b) -> Tree a -> Tree b
  mapTree f Empty = Empty
  mapTree f (Node x t1 t2) = 
      Node (f x) (mapTree f t1) (mapTree f t2)

Prove for all finite trees t: 

  p(t): inorder (mapTree f t) = map f (inorder t)

Associativity of (++) may be used without proof:

    (xs ++ ys) ++ zs = xs ++ (ys ++ zs)

[Note: refer to the file functions.md for
 definitions of map and (++)]

--------
Answer:

We prove the property p(t) by structural induction on the
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

--------------------------------------
Induction step: prove q(xs) => q(x:xs)
--------------------------------------

    Induction hypothesis:
      q(xs): map f (xs ++ ys) = map f xs ++ map f ys

    {LHS of q(x:xs)}
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
    {RHS of q(x:xs)}

□

___________________________________________________________