-----------------------------------------------------------
             RESIT FUNCTIONAL PROGRAMMING 2019               
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

[True]:[]

--------
Answer: 

Yes, the expression is type correct. Prepending the list
[True] to the empty list [] results in a list of lists of
Booleans. Remember that the empty list [] is polymorphic,
its type being [a] for any type a. Thus, the type checker
can unify the types by substituting a with [Bool]. Hence,
the type of the expression is [[Bool]].


--------------------------------
Question 1.2:
Is the following expression type correct? 
If YES, then give the type of the expression.

[]:[True]

--------
Answer:

No, this expression is not type correct. We cannot prepend 
the empty list [] to a list of Booleans [True] because the
operator (:) expects a single element of type a and a list
of type [a], and not two lists of type [a]. The expression
is therefore ill-typed. The operator (:) cannot be used to 
prepend a list to another list. 


--------------------------------
Question 1.3:
What is the most general type of the following function f?

f = (\x -> \y -> \z -> [x (y z), y z])

--------
Answer: 

The function f takes three arguments, of which the first
two have to be unary functions, since they are each applied
to a single argument: y is applied to z, and x is applied 
to the result of y z. The third argument z is a value that
is passed to the function y. So, if we take z :: a, then 
y :: a -> b, and x :: b -> c for some types a, b, and c.

The function f returns a list of two elements, the first
being the result of applying x to the result of y z, and
the second being the result of applying y to z. Since a
list requires its elements to have the same type, we need
to unify the types of the two elements. This means that
both the output of x and the output of y must have the
same type, so we can conclude that c must be equal to b.

Thus, the most general type of f is:
f :: (b -> b) -> (a -> b) -> a -> [b]
where a, b, and c are type variables that can be 
instantiated to any types.


--------------------------------
Question 1.4:
What is the most general type of the following function g?

g = \x -> \y -> \z -> x.y.z

--------
Answer: 

The function g takes three arguments, x, y, and z, which 
are applied in sequence using the composition operator (.)
So, they all have to be unary functions.

As we know, the composition operator (.) has the type
(.) :: (b -> c) -> (a -> b) -> a -> c
where the the output type of the inner function must match
the input type of the outer function.
So, we can deduce the types of x, y, and z from left to
right as follows:
- x :: a -> b
- y :: c -> a
- z :: d -> c

Composing them yields a unary function that outputs a 
function of type d -> b. Thus, the most general type 
of g is:
g :: (a -> b) -> (c -> a) -> (d -> c) -> d -> b
where a, b, c, and d are type variables that can be
instantiated to any types.


--------------------------------
Question 1.5:
What is the type of the following function h?

h = foldr (&&)

--------
Answer: 

The function h is a partially applied version of the foldr
function with the operator (&&) as the first argument.

The type of foldr is:
foldr :: (a -> b -> b) -> b -> [a] -> b
and the type of (&&) is:
(&&) :: Bool -> Bool -> Bool

When we partially apply foldr to (&&), the type of (&&)
has to match the type of the first argument of foldr,
which is a function of type a -> b -> b. So we can deduce
that a = Bool and b = Bool. This leaves a function of type
b -> [a] -> b, so that the type of h is:
h :: Bool -> [Bool] -> Bool 


___________________________________________________________

2. Programming in Haskell
___________________________________________________________

We call an Integer n a trinumber if n can be expressed as  
a sum of distinct powers of three (i.e. no duplicates of 
powers of three are allowed). 

For example, the numbers 1, 3, 9, 12, and 118 are all 
trinumbers because:

    1 = 3^0
    3 = 3^1
    9 = 3^2
    12 = 3^1 + 3^2
    118 = 3^0 + 3^2 + 3^3 + 3^4

Note that the number 20 can be expressed as a sum of powers 
of three as follows: 20 = 3^0 + 3^0 + 3^2 + 3^2, however 20 
is not a trinumber because the powers of three are not 
distinct.

Give a implementation of isTriNumber n (including its type) 
which returns True if and only if n is a trinumber.

--------
Answer:

> isTriNumber :: Integer -> Bool
> isTriNumber n = check n 1
>   where
>     check rem power
>       | rem  <= 0 = rem == 0
>       | power > n = False
>       | otherwise = skip power || include power
>       where
>         skip p    = check  rem      (p * 3)
>         include p = check (rem - p) (p * 3)

This is a recursive approach that, at each step, either
includes the current power of three or skips it, thus 
exploring all combinations of distinct powers of three.

The recursive case is represented by the third guard. 
If we decide to include the current power, we subtract
it from the remaining value (rem) and continue checking
with the next power of three (by multiplying the current
power by 3). If we skip the current power, we leave the
remaining value unchanged and continue with the next 
power of three.

The first two guards handle the base cases:
- If the remaining value reaches zero, it means we have 
  successfully expressed the original number as a sum of
  distinct powers of three, and return True. 
- If the remaining value becomes negative or if the 
  current power exceeds the original number, we return 
  False. These conditions prune the recursion tree (search
  space) and prevent unnecessary exploration and stack
  overflow.


___________________________________________________________

3. Higher-order functions
___________________________________________________________

Question 3.1:
Give an implementation of the function length that makes 
use of foldr.

--------
Answer:

> len :: [a] -> Int
> len = foldr (\ _ acc -> acc + 1) 0

As we traverse the list, we ignore the actual elements
and simply count them by incrementing the accumulator
by 1 for each element. The initial accumulator value is
0, representing the length of an empty list.


--------------------------------
Question 3.2:
The function aligned accepts two lists, and returns the 
number of aligned elements in the two lists. For example,
aligned "abca" "abdae" should return 3. 

Give an implementation of the function aligned that does 
not make use of recursion or a list comprehension. 
What is the type of the function aligned?

--------
Answer:

> aligned :: Eq a => [a] -> [a] -> Int
> aligned xs ys = length . filter id $ zipWith (==) xs ys

First, zipWith (==) compares the two lists element by 
element, returning a list of Booleans where each element
indicating where the elements match.
Next, filter id keeps only the True values from this list.
Finally, length counts the number of True values, which
corresponds to the number of aligned elements. 

Note that we need to explicitly feed the two input lists
xs and ys to the function zipWith (==) to make the 
composition work correctly, as function composition (.) 
can only chain together functions where the output type of
one matches the input type of the next.


--------------------------------
Question 3.3:
The function concatMap is deﬁned as follows: 

  concatMap f xs = concat(map f xs)

Give an alternative implementation of concatMap using the
function foldr. What is the type of concatMap?

--------
Answer:

> concatMap :: (a -> [b]) -> [a] -> [b]
> concatMap f = foldr (\x acc -> f x ++ acc) []

Using foldr, we traverse the list xs and apply the function
f to each element x. The result of f x is a list [b], which 
we concatenate with the accumulator acc using (++), thus
gradually building up the final result as a single list.

The initial accumulator is an empty list [], which serves
as the base case for the foldr operation. The overall
result is a single list that contains all the elements
produced by applying f to each element of xs.


___________________________________________________________

4. List comprehensions
___________________________________________________________

Question 4.1:
What is the output of the expression 

  take 6 [(x,y) | x <- [1..], y <- [x+1..]]?

--------
Answer:

The output of the expression is:

  [(1,2),(1,3),(1,4),(1,5),(1,6),(1,7)]

The reason is that the combination of the two generators
works as a nested loop: for each value of x, y iterates
over the range starting from x+1. 


--------------------------------
Question 4.2:
The function evenLists is deﬁned as: 

  evenLists xss = map (filter even) xss

Give an alternative implementation of this function using a 
list comprehension.

--------
Answer:

> evenLists :: [[Int]] -> [[Int]]
> evenLists xss = [[x | x <- xs, even x] | xs <- xss]

The outer list comprehension iterates over each list xs 
in the input list of lists xss. For each xs, the inner
list comprehension collects only the even elements. This
produces a new list of lists containing only the even
elements from each original list.


--------------------------------
Question 4.3:
The function triples takes three finite lists and combines 
them as follows. 

Let: xs = [x0, x1, x2, .., xl], ys = [y0, y1, y2, .., ym], 
zs = [z0, z1, z2, .., zn], and q the minimum of l, m and n.

Then: triples xs ys zs = [(x0, y0, y0), (x1, y1, z1), 
                         (x2, y2, z2, ).., (xq , yq , zq )]

For example: 
              triples [0..3] [2..10] [3..20] =
              [(0,2,3),(1,3,4),(2,4,5),(3,5,6)]

Give the type of the function triples and an implementation 
using a list comprehension.

--------
Answer:

> triples :: [a] -> [b] -> [c] -> [(a, b, c)]
> triples xs ys zs = [(x, y, z) | 
>                     (x, (y, z)) <- zip xs (zip ys zs)]

The function triples takes three lists xs, ys, and zs,
and combines them into a list of tuples by zipping the
lists together. The outer zip pairs each element of xs
with a tuple containing the corresponding elements from
ys and zs. As zip truncates to the shortest list, this
ensures that the requirement of stopping at the minimum
length is satisfied.


___________________________________________________________

5. Infinite lists
___________________________________________________________

Question 5.1:
Deﬁne the infinite list fibs of Fibonacci numbers using a 
list comprehension. So:
  
  take 10 fibs 
  should return: [0,1,1,2,3,5,8,13,21,34]
  
Note that fibs = [fib n | n <- [0..]] is not considered a 
valid answer.

--------
Answer:

> fibs :: [Integer]
> fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

The first two elements of the Fibonacci sequence are 0 and
1. The rest of the sequence is generated by using
zipWith (+) to add the Fibonacci number from two positions
back (fibs) and the immediate previous number (tail fibs).
This effectively generates the Fibonacci sequence in an
infinite list.


--------------------------------
Question 5.2:
Without using a list comprehension, give a deﬁnition of 
the infinite list natlists = [[0],[0,1],[0,1,2],...].

--------
Answer:

> natlists :: [[Integer]]
> natlists = map (\n -> [0..n]) [0..]

Using map, we apply the function \n -> [0..n] to each 
natural number in the infinite list [0..]. This
has the effect of replacing each natural number n with 
the list [0..n], thus generating the desired infinite
list of lists. 

If we wanted to avoid using [0..] directly, we could 
define it recursively as follows:

> nats :: [Integer]
> nats = 0 : map (+1) nats


--------------------------------
Question 5.3:
Implement the function multiples that takes a finite list
of Integers and outputs the increasing infinite list of
positive integers that can be expressed as a multiple of 
one (or more) of the numbers in the input list.

For example:

  take 10 (multiples [5,2,8]) 
  should return: [2,4,5,6,8,10,12,14,15,16]

--------
Answer:

> multiples :: [Integer] -> [Integer]
> multiples xs = foldr merge [] [[x,2*x..] | x <- xs]

> merge :: Ord a => [a] -> [a] -> [a]
> merge [] ys = ys
> merge xs [] = xs
> merge (x : xs) (y : ys)
>   | x < y     = x : merge xs (y : ys)
>   | x > y     = y : merge (x : xs) ys
>   | otherwise = x : merge xs ys

The function multiples takes a list of integers xs and
generates an infinite list of multiples for each integer
in xs. The list comprehension generates a list of 
increasing lists of multiples for each integer x in xs,
starting from x and incrementing by x each time.
The foldr merge then takes these input streams of multiples
and merges them into a single sorted infinite list, whilst
removing duplicates. 


___________________________________________________________

6. ADT module
___________________________________________________________

The type Complex is an Abstract Data Type (ADT) for complex 
numbers.

Implement a module Complex such that the implementation of 
the type Complex is hidden to the user. Recall that the 
complex number a + ib (where i is the imaginary number for 
which i^2 = -1) can be represented as a pair (a, b) where a
and b are Doubles. 

The following operations need to be implemented:
• add: returns the complex addition of two complex numbers. 
  Recall that (a + ib) + (c + id) = (a + c) + i(b + d).
• sub: returns the complex subtraction. 
  Recall that (a + ib) - (c + id) = (a - c) + i(b - d).
• mul: returns the multiplication of two complex numbers. 
  Recall that (a + ib)(c + id) = (ac - bd) + i(ad + bc).

--------

Answer:

To turn the below code into a module, you would create
a file named Complex.hs and start it with the following
module declaration:

module Complex (
  Complex,
  make,
  add,
  sub,
  mul
) where

This line exports the abstract data type Complex and its 
associated functions, but does not export the constructor 
Cx, thus hiding the concrete implementation details.

> data Complex = Cx Double Double

> -- pretty printing of complex numbers
> instance Show Complex where
>   show (Cx re im) = 
>     "("++(show re)++") + ("++(show im)++")i"

> -- converts an input tuple into a complex number; 
> -- this function is needed because the module 
> -- does not export the constructor Cx 
> make :: (Double,Double) -> Complex
> make (a,b) = Cx a b

> -- returns the sum of two complex numbers
> add :: Complex -> Complex -> Complex
> add (Cx a b) (Cx c d) = Cx (a + c) (b + d)

> -- returns the subtraction of two complex numbers
> sub :: Complex -> Complex -> Complex
> sub (Cx a b) (Cx c d) = Cx (a - c) (b - d)

> -- returns the multiplication of two complex numbers
> mul :: Complex -> Complex -> Complex
> mul (Cx a b) (Cx c d) = Cx (a*c - b*d) (a*d - b*c)


Example usage:

ghci> x = make (1.0, 2.0)
ghci> y = make (3.5, -4.0)
ghci> x
(1.0) + (2.0)i
ghci> y
(3.5) + (-4.0)i
ghci> add x y
(4.5) + (-2.0)i
ghci> sub x y
(-2.5) + (6.0)i
ghci> mul x y
(11.5) + (-11.0)i


___________________________________________________________

7. Proof on lists
___________________________________________________________

Consider the following Haskell function rvl.

  rvl [] ys     = ys
  rvl (x:xs) ys = rvl xs (x:ys)

Prove the following property:

  p(xs): rvl (xs++ys) [] = rvl ys (rvl xs []) 
         for all finite lists xs and ys.

[Note: refer to functions.md for the definition of (++)]

--------
Answer:

Trying to prove the property for the specific case where
the third list in the property is empty, proves to be too 
difficult. It does not provide a strong enough induction 
hypothesis to work with. In order to get around this and 
strenghen the induction hypothesis, we will use the common 
technique of generalizing the property p.  
This means that we will first prove a lemma q where the
third argument of rvl can be any finite list zs. If this 
lemma holds, then so does the property pf.

Lemma q: 
          q(xs): rvl (xs++ys) zs = rvl ys (rvl xs zs) 
                 for all finite lists xs, ys, and zs.

We will prove this property q by structural induction on 
the list xs. 

--------------------------------------
Base case: prove q([])
--------------------------------------

    {LHS of q([])}
  rvl ([] ++ ys) zs
=   {unapplying ++}
  rvl ys zs
=   {unapplying rvl: first case}
  rvl ys (rvl [] zs)
    (RHS of q([]))

--------------------------------------
Inductive step: prove q(xs) => q(x:xs)
--------------------------------------

    Induction hypothesis:
      q(xs): rvl (xs ++ ys) zs = rvl ys (rvl xs zs)
  
    {LHS of q(x:xs)}
  rvl ((x:xs) ++ ys) zs
=   {applying ++}
  rvl (x:(xs ++ ys)) zs
=   {applying rvl}
  rvl (xs ++ ys) (x:zs)
=   {using induction hypothesis}
  rvl ys (rvl xs (x:zs))
=   {unapplying rvl: second case}
  rvl ys (rvl (x:xs) zs)
    (RHS of q(x:xs))

□

Thus, by structural induction, we have shown that the
property q holds for all finite lists xs, ys, and zs,
including the case where zs is empty.
By virtue of the lemma, we can now conclude that the 
property p holds for all finite lists xs and ys.

□

Note: The function rvl is in fact the reverse function
for lists in accumulator passing style.


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

  p(t): inorder (mirror t) = reverse (inorder t)

[Note: You may use without proof that the operator ++ is 
associative and that xs = xs ++ []. If you need any other 
lemmas to complete the proof, then prove these lemmas 
separately. For the definitions of reverse and ++ see the 
file functions.md]

--------
Answer:

We will prove the property p by structural induction on 
the tree t.

---------------------------------------
Base case: prove p(Empty)
---------------------------------------

    {LHS of p(Empty)}
  inorder (mirror Empty)
=   {applying mirror}
  inorder Empty
=   {applying inorder}
  []
=   {unapplying reverse}
  reverse []
=   {unapplying inorder}
  reverse (inorder Empty)
    {RHS of p(Empty)}

---------------------------------------
Inductive step: prove p(l) ∧ p (r) 
                      => p(Node x l r)
---------------------------------------

    Induction hypothesis:
      p(l): inorder (mirror l) = reverse (inorder l)
      p(r): inorder (mirror r) = reverse (inorder r)

    {LHS of p(Node x l r)}
  inorder (mirror (Node x l r))
=   {applying mirror}
  inorder (Node x (mirror r) (mirror l))
=   {applying inorder}
  inorder (mirror r) ++ [x] ++ inorder (mirror l)
=   {using induction hypothesis}
  reverse (inorder r) ++ [x] ++ reverse (inorder l)
=   {since x = reverse [x] by definition of reverse, 
     and in the last step, the definition of ++}
  reverse (inorder r) ++ reverse [x] 
  ++ reverse (inorder l)
=   {applying associativity of ++, given without proof}
  (reverse (inorder r) ++ reverse [x])
  ++ reverse (inorder l)
=   {applying lemma q}
  reverse ([x] ++ inorder r) ++ reverse (inorder l)
=   {applying lemma q again}
  reverse (inorder l ++ [x] ++ inorder r)
=   {unapplying inorder}
  reverse (inorder (Node x l r))
    {RHS of p(Node x l r)}

□

-------------------------------------------
Lemma q: 
        q(xs) : reverse(xs ++ ys) 
                = reverse ys ++ reverse xs
-------------------------------------------

We will prove this lemma by structural induction on xs.

-------------------------------------------
Base case: prove q([])
-------------------------------------------

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

--------------------------------------------
Inductive step: prove q(xs) => q((x : xs))
--------------------------------------------

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