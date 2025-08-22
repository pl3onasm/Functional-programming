> import Prelude hiding (iterate, map) 

-----------------------------------------------------------
             EXAM FUNCTIONAL PROGRAMMING 2019               
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

'a' : 'b' : [] : []

--------
Answer: 

No, this is not a type correct expression. The cons 
operator (:) is right-associative (as you would expect from
a prepending operator), so the expression is parsed as:
  'a' : ('b' : ([] : [])).
Prepending the empty list to the empty list results in 
[[]], which is a list of lists. However, prepending
characters 'a' and 'b' to this list fails because the
types do not match: the operator (:) expects the elements
to be prepended to be of the same type as the elements of
the list it is prepending to. In this case, the first two
elements are characters, while the last element is a list
of lists. These types cannot be unified, leading to a type
error. The expression would be type correct if it were
written as: ('a' : 'b' : []) : [] to override the right-
associativity. Now we are prepending a list of characters
to a list of lists, and obtain the type [[Char]].


--------------------------------
Question 1.2:
Is the following expression type correct? 
If YES, then give the type of the expression.

('a' : 'b' : []) : []

--------
Answer:

Yes, this a type correct expression. The inner empty list
is prepended with the characters 'a' and 'b', which
results in a list of characters. So the inner list's type
is [Char]. This list is then prepended to another empty 
list [], which results in a list of lists of characters.
Thus, the type of the entire expression is: 

  [('a', 'b') : []] :: [[Char]]


--------------------------------
Question 1.3:
What is the most general type of the following function f?

f = (\x -> \y -> \z -> [x y, x (x z)])

--------
Answer: 

First, we can see that the function takes three arguments 
x, y, and z. The first argument x is a unary function that
is applied to y and z. So x :: a -> b, where a is the type
of y and z. 
Next, we see that the function x is also applied to itself,
meaning that the output type of x must be the same as its
input type. Therefore, we can conclude that b = a, and 
x :: a -> a.
Finally, the function f returns a list containing two
elements: the result of applying x to y and the result of
applying x to itself with z. Thus, the type of f is:

  f :: (a -> a) -> a -> a -> [a]


--------------------------------
Question 1.4:
What is the most general type of the following function g?

g = (. not)

--------
Answer: 

This is a section of the composition operator (.), which
results in a unary function taking a unary function as an
argment and returning a new unary function.
The operator (.) has the type:
  (b -> c) -> (a -> b) -> a -> c. 
The inner function not has the type Bool -> Bool, so we 
have a = Bool and b = Bool. Since the outer function
takes the output of not as its first argument, we have
that the type of the outer function is Bool -> a for some
type a. Thus, the most general type of g is:

  g :: (Bool -> a) -> Bool -> a


--------------------------------
Question 1.5:
What is the most general type of the following function h?

h = (not . )

--------
Answer:

This is another section of the composition operator (.)
where now the outer function is the Boolean negation
operator not, which again has the type Bool -> Bool.
This specifies the output of the inner function as Bool, as
its output must match the input of the outer function not,
so that for the inner function we have a -> Bool for some 
type a. Following the typing of the composition operator,
and fixing its first argument to not, we therefore have:

  h :: (a -> Bool) -> a -> Bool


___________________________________________________________

2. Programming in Haskell
___________________________________________________________

A well formed string of parentheses is deﬁned by the 
following recursive rules:

• The empty string is well formed.
• If s is a well formed string, 
  then (s) is a well formed string.
• If s and t are well formed strings, 
  then their concatenation st is a well formed string.

For example, "((()))" and "()()()" are well formed strings, 
while "(()", ")(()" and ")(" are not. 

Write a Haskell function isWFS :: String -> Bool such that
isWFS str returns True if the string str is well formed and
False otherwise.

--------
Answer:

> isWFS :: String -> Bool
> isWFS str = parse str 0
>   where
>     parse []         0 = True
>     parse []         _ = False
>     parse ('(' : cs) n = parse cs (n + 1)
>     parse (')' : cs) n = n > 0 && parse cs (n - 1)
>     parse _          _ = False   -- invalid characters

A helper function parse is used with an initial balance 
counter n = 0. This function consumes the string from left 
to right, and increments the counter for each opening
parenthesis and decrements it for each closing parenthesis.
If the counter is zero at the end of the string, the
string is well formed. If the counter is negative at any
point, or if it is not zero at the end, the string is not
well formed. The last guard clause ensures that only
parentheses are processed, and any other characters lead
to an immediate False return.


___________________________________________________________

3. Higher-order functions
___________________________________________________________

Question 3.1:
Without using recursion or a list comprehension, write a 
function selectiveMap which takes three arguments. Also, 
give the type of the function selectiveMap. 

The ﬁrst argument of the function is a predicate p, the 
second some function f, and the third a list xs. The 
function selectiveMap returns a list that is just like xs, 
but in which every element x that satisﬁes p is replaced 
by f applied to x.

For example, the call 

  selectiveMap even (*2) [1,2,3,4,5,6] 
  should return: [1,4,3,8,5,12]

--------
Answer:

> selectiveMap :: (a -> Bool) -> (a -> a) -> [a] -> [a]
> selectiveMap p f = 
>   foldr (\x acc -> (if p x then f x else x) : acc) []

Using foldr, we check each element x of the list against
the predicate p. If p x is True, we apply the function f
to x, otherwise we keep x unchanged. The result is
accumulated in the list, starting from an empty list. 

It is also possible to leave out the accumulator by turning
the lambda function into a partially applied (:) operator:

> selectiveMap' p f = 
>   foldr (\x -> (:) (if p x then f x else x)) []

Another possibility is to use map:

> selectiveMap'' p f xs = 
>   map (\x -> if p x then f x else x) xs


--------------------------------
Question 3.2:
Without using recursion or a list comprehension, write a 
function thresholdPairs which takes two arguments.
The ﬁrst is an Integer n, and the second is a list xs of 
Integer pairs. The output should be the list of pairs 
(a,b), in the same order as in the list xs, for which the
sum of a and b is greater than n.

For example, the function call 

  thresholdPairs 3 [(1,2),(2,2),(3,5),(0,3),(0,4)] 
  should return: [(2,2),(3,5),(0,4)]

--------
Answer:

> thresholdPairs :: Integer -> [(Integer, Integer)] 
>                   -> [(Integer, Integer)]
> thresholdPairs n = filter (\(a, b) -> a + b > n)

The lambda function checks the condition for each pair
(a, b) in the list. The filter function then collects all
pairs that satisfy this condition, returning a new list
with only those pairs.


--------------------------------
Question 3.3:
Implement the standard function map using the standard 
function foldr.

--------
Answer:

This can be done by using foldr to traverse the list and
applying the function f to each element, while accumulating
the results in a new list:

> map :: (a -> b) -> [a] -> [b]
> map f = foldr (\x acc -> f x : acc) []

We can also leave out the accumulator by turning the lambda
function into a section of the (:) operator:

> map' f = foldr (\x -> (f x :)) []

It is also possible to use the operator (.) to compose the
function f with the (:) operator, which results in an even
more concise definition:

> map'' f = foldr ((:).f) []


___________________________________________________________

4. List comprehensions
___________________________________________________________

Question 4.1:
Use a list comprehension to implement the function 
partition which takes two arguments. The first is some
element x, and the second a list xs. The function should 
return a pair of lists of which the first is the list of 
all elements of xs that are less than or equal to x, while 
the second is the list of all elements of xs that are 
greater than x. Also, give the most general type of the 
function partition.

--------
Answer:

> partition :: Ord a => a -> [a] -> ([a], [a])
> partition z xs = (smaller, larger)  
>   where
>     smaller = [x | x <- xs, x <= z]
>     larger  = [x | x <- xs, x >  z]

The function partition takes an element z and a list xs,
and uses two list comprehensions to create the two lists:
- smaller contains all elements x from xs 
  that are less than or equal to z
- larger contains all elements x from xs 
  that are greater than z
The function returns a tuple containing both lists. 
The type constraint Ord a => ensures that the elements
of the list xs can be ordered. This is the most general 
type, since any type that supports comparison can be
partitioned.


--------------------------------
Question 4.2:
Use an efficient list comprehension to implement the 
function tripletSum (including its type) that takes a 
positive Integer n, and returns the lexicographically 
ordered list of all triples (a,b,c) such that n equals 
a+b+c and 1 <= a <= b <= c. 

For example:

  tripletSum 6 = [(1,1,4),(1,2,3),(2,2,2)]

--------
Answer:

> tripletSum :: Integer -> [(Integer, Integer, Integer)]
> tripletSum n = [(a, b, n-a-b) | a <- [1 .. n `div` 3], 
>                           b <- [a .. (n - a) `div` 2]]

In order to generate the triples (a, b, c) efficiently, 
we use a list comprehension with two nested generators.
The outer generator iterates over a, which ranges from 1
up to n div 3, because a must be less than or equal to b
and c, and thus a cannot exceed n/3.
The inner generator iterates over b, which ranges from a
up to (n - a) div 2, because b must be less than or equal
to c. 
The value of c is then determined uniquely as n - a - b.

This ensures that all triples are unique and are generated 
in lexicographic order, as a is always less than or equal 
to b, and b is always less than or equal to c.


--------------------------------
Question 4.3:
The function adjacentTriples takes a list xs and outputs
the list of all triples of adjacent elements in the list 
xs. Give its type and an implementation using a list 
comprehension. 

For example:

  adjacentTriples "curry"
  = [('c','u','r'),('u','r','r'),('r','r','y')]

--------
Answer:

> adjTriples :: [a] -> [(a, a, a)]
> adjTriples xs = [(x, y, z) | (x, (y, z)) <- ts]
>   where ts = zip xs (zip (drop 1 xs) (drop 2 xs))

By using the zip function twice, we can create a list of
tuples where each tuple contains an element x from the
list xs and a pair (y, z) of the next two elements in the
list. Unpacking the tuples in the list comprehension
allows us to construct the desired triples (x, y, z).


___________________________________________________________

5. Infinite lists
___________________________________________________________

Question 5.1:
Give a recursive implementation of the function iterate 
(including its type) that takes two arguments. The ﬁrst is 
a function f and the second some value x. The call 
iterate f x returns an inﬁnite list of repeated 
applications of f to x. So: 

iterate f x = [x, f x, f(f x), f(f(f x)), f(f(f(f x))),...]

For example: 

  take 10 (iterate (*2) 1)
  = [1,2,4,8,16,32,64,128,256,512]

--------
Answer:

> iterate :: (a -> a) -> a -> [a]
> iterate f x = its
>   where its = x : map f [i | i <- its]

Here, The list is built using a helper list its, which is 
defined as the first element x followed by the results of 
mapping the function f over the infinite list itself, which
produces the repeated applications of f to x.

A version without using map or list comprehensions is 
also possible by applying f directly in the recursive call:

> iterate' :: (a -> a) -> a -> [a]
> iterate' f x = x : iterate' f (f x)

Here, the first element is again x, and the tail of the 
list is constructed by recursively calling iterate' with 
the next value f x.


--------------------------------
Question 5.2:
Give a deﬁnition of the infinite list tribonacci which is 
the ordered list of all tribonacci numbers. Recall that the
tribonacci numbers are deﬁned as: 

  T (n) = n for n < 3
  T (n) = T (n - 1) + T (n - 2) + T (n - 3) for n ≥ 3

So: take 10 tribonacci = [0,1,2,3,6,11,20,37,68,125] 

Your implementation must make (useful) use of the function 
zipWith, so map T [0..] is not accepted as a valid answer.

--------
Answer:

> tribonacci :: [Integer]
> tribonacci = t 
>   where t = 0 : 1 : 2 : zipWith (+) 
>             (zipWith (+) t (drop 1 t)) (drop 2 t)

The tribonacci list starts with the first three base cases
0, 1, and 2. The rest of the list is generated by summing
the previous three elements in two steps:

1. The inner zipWith sums the elements three and two 
   positions back (t and drop 1 t) relative to the element 
   being generated.
2. The outer zipWith then adds the element one position 
   back (drop 2 t) to the result of the inner zipWith,
   yielding the tribonacci number at the current position.


--------------------------------
Question 5.3:
Give a deﬁnition of the infinite list palindromes which is 
a list of lists of palindromic bit strings. The n-th list
contains all lexicographically sorted palindromes of length 
n (starting with n = 0). For example: 

  take 4 palindromes
  = [[""],["0","1"],["00","11"],["000","010","101","111"]]

--------
Answer:

> palindromes :: [[String]]
> palindromes = [""] : ["0","1"] : 
>   [[c : s ++ [c] | c <- "01", s <- p] | p <- palindromes]

The list palindromes is defined recursively. The first two
elements are the base cases: an empty string for length 
n = 0, and the strings "0" and "1" for length n = 1. 
For n ≥ 2, each palindrome is formed by surrounding a 
palindrome of length n-2 with the same character c,
which can be either '0' or '1', preserving the symmetry
of the palindrome. The list is generated in lexicographic
order by iterating over the characters '0' and '1', in that
order, and concatenating them with the palindromes of
length n-2, which are generated recursively from the
palindromes list itself.


___________________________________________________________

6. ADT module
___________________________________________________________

The type Polynomial is an Abstract Data Type (ADT) for real 
valued polynomials.

Implement a module Polynomial such that the implementation 
of the type Polynomial is hidden to the user.

The following operations need to be implemented:
• makePolynomial coeffs converts the coeffcients in the 
  list coeffs into a Polynomial. For example,
  makePolynomial [2.0,0.0,0.5] should produce the 
  Polynomial representation of 2x^2 + 0.5.
• eval pol x returns the evaluation of the polynomial pol 
  at x. For example, 2x^2 + 0.5 at x = 1.0 can be computed
  using eval (makePolynomial [2.0,0.0,0.5]) 1.0.
• add lhs rhs: returns the polynomial that is the addition
  of lhs and rhs. For example, (2x^2 + 0.5) + (x - 1) can
  be constructed using 
  add (makePolynomial [2.0,0.0,0.5]) 
      (makePolynomial [1.0,-1.0])
• scale a pol: multiplies the polynomial pol by the scalar 
  a. For example, 5(2x^2 + 0.5) can be constructed using
  scale 5.0 (makePolynomial [2.0,0.0,0.5])

--------

Answer:

To turn the below code into a module, you would create
a file named Polynomial.hs and start it with the following
module declaration:

module Polynomial (
  Polynomial,
  makePolynomial,
  eval,
  add,
  scale
) where

This line exports the abstract data type Polynomial and its
associated functions, but does not export the constructor
P, thus hiding the concrete implementation details.

> data Polynomial = P [Double]

> -- pretty prints a polynomial
> instance Show Polynomial where
>   show (P coeffs) =
>     let terms = reverse [(c, n) | 
>                     (c, n) <- zip coeffs [0..], c /= 0]
>         showTerm (c, 0) = "(" ++ show c ++ ")"
>         showTerm (c, 1) = "(" ++ show c ++ ")x"
>         showTerm (c, n) = "(" ++ show c ++ ")x^" ++ show n
>         join [] = ""
>         join [t] = t
>         join (t:ts) = t ++ " + " ++ join ts
>     in if terms == [] then "0" 
>        else join (map showTerm terms)

> -- constructs a polynomial from a list of coefficients
> -- which are given in descending order of degree
> makePolynomial :: [Double] -> Polynomial
> makePolynomial coeffs = P (reverse coeffs)

> -- evaluates a polynomial at a given point z
> eval :: Polynomial -> Double -> Double
> eval (P xs) z = sum [x * z^ex | (x, ex) <- zip xs [0..]]

> -- returns the sum of two polynomials
> add :: Polynomial -> Polynomial -> Polynomial
> add (P xs) (P ys) = P (add' xs ys)
>   where 
>     add' (x : xs) (y : ys) = (x + y) : add' xs ys
>     add' xs ys = xs ++ ys

> -- multiplies a polynomial by a scalar
> scale :: Double -> Polynomial -> Polynomial
> scale z (P xs) = P (map (* z) xs)

Internally, the coefficients of the polynomial are stored
as a list of Doubles in ascending order of degree (constant
term first) even though they are provided to makePolynomial
in descending order (highest degree first). This design 
choice makes operations such as addition and evaluation
easier to implement, because the lowest-degree terms are
always aligned at the head of the list, and the list index
corresponds directly to the term's degree.
A simple Show instance is included as a convenience to 
display polynomials in a user-friendly way.


Example usage:

ghci> p1 = makePolynomial [3.0, 1.0, 0.0, 2.0]
ghci> p2 = makePolynomial [2.0, -1.0, -0.5]
ghci> p1
(3.0)x^3 + (1.0)x^2 + (2.0)
ghci> p2
(2.0)x^2 + (-1.0)x + (-0.5)
ghci> eval p1 2.0
30.0
ghci> eval p2 3.0
14.5
ghci> add p1 p2
(3.0)x^3 + (3.0)x^2 + (-1.0)x + (1.5)
ghci> scale 4 p1
(12.0)x^3 + (4.0)x^2 + (8.0)


___________________________________________________________

7. Proof of equality
___________________________________________________________

Consider the following Haskell function:

    f 0 = 0
    f 1 = 1
    f n = 5*(f (n-1)) - 6*(f (n-2))

Prove the following property: 
  
    p(n): f n = 3^n - 2^n 
          for all non-negative integers n.

--------
Answer:

We will prove this property by natural strong induction
on n. Strong induction is used here because the definition
of f depends on the two previous values f(n-1) and f(n-2).

------------------------------------
Base case: prove p(0) and p(1)
------------------------------------

1. Case: p(0)

    {LHS of p(0)}
  f 0
=   {applying f}
  0
=   {arithmetic}  
  1 - 1
=   {arithmetic}
  3^0 - 2^0
    {RHS of p(0)}

2. Case: p(1)

    {LHS of p(1)}
  f 1
=   {applying f}
  1
=   {arithmetic}
  3 - 2
=   {arithmetic}
  3^1 - 2^1
    {RHS of p(1)}

------------------------------------
Inductive step: prove p(n) => p(n+1)
------------------------------------

    As we are using strong induction, we assume that the
    property holds for all  0 <= k <= n, where k ∈ ℕ.

    Induction hypothesis: p(n) and p(n-1)
      p(n):   f n       = 3^n - 2^n
      p(n-1): f (n - 1) = 3^(n - 1) - 2^(n - 1)

    {LHS of p(n+1)}
  f (n + 1)
=   {applying f}
  5 * (f n) - 6 * (f (n - 1))
=   {using induction hypothesis}
  5 * (3^n - 2^n) - 6 * (3^(n - 1) - 2^(n - 1))
=   {distributing and factoring}
  5 * 3^n - 5 * 2^n - 2 * 3 * 3^(n - 1) + 3 * 2 * 2^(n - 1)
=   {exponentiation rules}
  5 * 3^n - 5 * 2^n - 2 * 3^n + 3 * 2^n 
=   {algebra}
  3 * 3^n - 2 * 2^n
=   {exponentiation rules}
  3^(n + 1) - 2^(n + 1)
    {RHS of p(n+1)}

□


___________________________________________________________

8. Proof on trees
___________________________________________________________

Given is the data type Tree and the functions inorder, 
and flatten:

  data Tree a = Empty | Node a (Tree a) (Tree a)

  inorder :: Tree a -> [a]
  inorder Empty = []
  inorder (Node x l r) = inorder l ++ [x] ++ inorder r

  flatten :: Tree a -> [a] -> [a]
  flatten Empty ys = ys
  flatten (Node x l r) ys = flatten l (x:flatten r ys)

Prove for all ﬁnite trees t: 

  p(t): inorder t = flatten t []

[Note: If you need one or more lemmas to complete the 
proof, then prove these lemmas separately.
The defintion of ++ can be found in functions.md]

--------
Answer:

We will prove this property p by structural induction on 
the tree t.

--------------------------------------
Base case: prove p(Empty)
--------------------------------------

    {LHS of p(Empty)}
  inorder Empty
=   {applying inorder}
  []
=   {unapplying flatten}
  flatten Empty []
    {RHS of p(Empty)}

--------------------------------------
Inductive step: prove p(l) ∧ p(r)
                      => p(Node x l r)
--------------------------------------

    Induction hypothesis: p(l) and p(r)
      p(l): inorder l = flatten l []
      p(r): inorder r = flatten r []

    {LHS of p(Node x l r)}
  inorder (Node x l r)
=   {applying inorder}
  inorder l ++ [x] ++ inorder r
=   {using induction hypothesis}
  flatten l [] ++ [x] ++ flatten r []
=   {applying lemma: associativity of (++)}
  flatten l [] ++ ([x] ++ flatten r [])
=   {[x] ++ xs = x : xs by definition of (++)} 
  flatten l [] ++ (x : flatten r [])
=   {applying ++}
  flatten l (x : flatten r [])
=   {applying flatten}
  flatten (Node x l r) []
    {RHS of p(Node x l r)}
  
□

-------------------------------------------------
Lemma q(xs):  xs ++ (ys ++ zs) = (xs ++ ys) ++ zs
-------------------------------------------------

We will prove this lemma q by structural induction 
on the list xs.

----------------------------------------
Base case: prove q([])
----------------------------------------

    {LHS of q([])}
  [] ++ (ys ++ zs)
=   {applying ++}
  ys ++ zs
=   {unapplying ++}
  ([] ++ ys) ++ zs
    {RHS of q([])}

----------------------------------------
Inductive step: prove q(xs) => q((x:xs))
----------------------------------------

    Induction hypothesis: q(xs)
      xs ++ (ys ++ zs) = (xs ++ ys) ++ zs

    {LHS of q((x:xs))}
  (x:xs) ++ (ys ++ zs)
=   {applying ++}
  x : (xs ++ (ys ++ zs))
=   {using induction hypothesis}
  x : ((xs ++ ys) ++ zs)
=   {unapplying ++}
  (x : (xs ++ ys)) ++ zs
=   {unapplying ++}
  ((x:xs) ++ ys) ++ zs
    {RHS of q((x:xs))}

□

___________________________________________________________