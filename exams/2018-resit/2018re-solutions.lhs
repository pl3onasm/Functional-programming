> import Prelude hiding (filter, zipWith, gcd, scanl)

-----------------------------------------------------------
             RESIT FUNCTIONAL PROGRAMMING 2018               
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

  ("x", 'x', [True]) : []

--------
Answer: 

Yes, this is type correct. Tuples are allowed to contain
elements of different types, and the tuple in this case has
type (String, Char, [Bool]). It is prepended to the empty
list [], thus creating a list of tuples of the type: 
  [(String, Char, [Bool])]


--------------------------------
Question 1.2:
Is the following expression type correct? 
If YES, then give the type of the expression.

  (+1) . (0<)

--------
Answer:

No, this is not type correct. The composition operator (.)
expects two unary functions as arguments, which are 
provided by the sections (+1) and (0<). The inner section
(0<) is a unary function that takes a number and returns a
Boolean value indicating whether the number is greater 
than 0.
The outer section (+1) is a unary function that takes a
number and adds 1 to it. As a result, the type of the
expression (+1) . (0<) is not well-formed because the
output type of the inner function (0<) is a Boolean, while
the input type of the outer function (+1) is a number.
Thus, the types do not match, which means that the two
functions cannot be composed. Therefore, the expression is
not type correct.


--------------------------------
Question 1.3:
Is the following expression type correct? 
If YES, then give the type of the expression.

  (+1) . (0+)

--------
Answer: 

Yes, this is type correct. The composition operator (.)
expects two unary functions as arguments. These two
functions are (+1) and (0+), both sections of the binary
operator (+). The first section (+1) takes a number and  
adds 1 to it, while the second section (0+) takes a numbers
and adds 0 to it, which is effectively the identity 
function. Both sections have the type
  Num a => a -> a
Thus, the composition of these two functions results in a
function of the same type:

  (+1) . (0+) :: Num a => a -> a

This means that the resulting function takes a number as
input and returns a number as output, which is the input
number incremented by 1, since adding 0 does not change
the value.


--------------------------------
Question 1.4:
Is the following definition of f type correct?
If YES, then give the most general type of f.

  f = [] : [[xs] | xs <- f]

--------
Answer: 

No, this is not type correct. The cons operator (:) expects
a value of a certain type on its left side and a list of
the same type on its right side. In this case, the left
side is the empty list [] which has the type [a] for some
type a. The right side is a list comprehension and as we 
know its element type is [a] it must be a list of lists of
type [[a]]. That means that f must also have type [[a]].
However, f is defined recursively, and appears as a 
generator in the list comprehension. This means that the
element of xs that is extracted from f must have the type
[[a]], and so what is generated inside the comprehension is
a list of lists of lists, i.e. [[[a]]], leading to a type
of [[[[a]]]]. But this would mean that f is of type [[[a]]] 
and of type [[[[a]]]] at the same time, leading to a type 
mismatch.
Thus, the definition of f is not type correct because it
leads to a type mismatch between the left and right sides.


--------------------------------
Question 1.5:
What is the type of the following function g?

  (.).(.)

--------
Answer: 

The composition operator (.) expects two unary functions as
arguments and returns a unary function. Its type is:

  (.) :: (p -> q) -> (r -> p) -> r -> q

In this case, we are composing two unary functions, which
are themselves the result of a composition of two unary
functions. The key point is that the output of the inner
composition must match the input of the outer composition.

Let us first look at the inner composition (.) of type:

  (.) :: (x -> y) -> (z -> x) -> z -> y

This really means that the inner composition takes two
functions as arguments: the first argument has type 
(x -> y) and the second argument has type (z -> x). 
The output of the inner composition then is a function
of type (z -> y). Let us abstract away and treat these
functions as values a, b, and c, as we can do with
function types. So we have:

  a = x -> y
  b = z -> x
  c = z -> y

Thus, the inner (.) has type:  a -> b -> c

Now, the outer composition (.) takes the output c of the 
inner one and maps it to a new unary function of another 
type, let us call it d. So, the outer (.) has type: c -> d

Following the normal typing of the composition operator,
we can now deduce the type of the function g, where g takes
the outer (.) as its first argument and the inner (.) as
its second argument, plus the input (a -> b) of the inner 
(.) as its third argument. The output of g is then the 
same as the output of the outer (.), which is d. 
Thus, the type of g is:

  g :: (c -> d) -> (a -> b -> c) -> a -> b -> d


___________________________________________________________

2. Programming in Haskell
___________________________________________________________

We can represent a directed graph by a list (with the type 
[(Int,Int)]) of arcs.

For example, the graph depicted in the file graph.png 
included in this exam folder can be represented by the 
list [(1,2),(1,3),(3,2),(3,4),(4,3)].

Write a function 
  paths :: Int -> Int -> [(Int,Int)] -> [[Int]] 
such that paths a b arcs returns a list containing all the 
paths from node a to node b using steps taken from the list
arcs. Note that these paths may not use an arc more than 
once. 

For example: 

    paths 1 2 [(1,2),(1,3),(3,2),(3,4),(4,3)]
  = [[1,2],[1,3,2],[1,3,4,3,2]]

--------
Answer:

> paths :: Int -> Int -> [(Int,Int)] -> [[Int]]
> paths a b arcs
>   | a == b    = [[b]]                        
>   | otherwise =
>       [a : path | (x,y) <- arcs, x == a,               
>       path <- paths y b (filter (/= (x,y)) arcs)]
  
The first guard checks if we have reached the destination
node b. If so, we return a list containing just the
list [b].
The second guard is the recursive case. Here we look for
all arcs starting at a, and for each such arc (x,y), we
recursively find all paths from y to b, filtering out the
arc (x,y) from the list of arcs to avoid using it again.

The implementation uses a backtracking algorithm, which 
means that at each step we branch on all possible outgoing 
arcs from the current node, and then recursively explore 
each choice, keeping track of visited arcs to avoid cycles. 
If a branch leads to a dead end, recursion unwinds and 
backtracks to try the next option. When a branch succeeds 
in reaching the destination node b, that completed path is 
added to the result list.

Note that we cannot use a list of visited nodes to avoid
cycles, because the same node can appear in a single path
more than once, as shown in the example. Instead, we filter
out the arcs that have already been used in the current
path.


___________________________________________________________

3. Higher-order functions
___________________________________________________________

Question 3.1:
Give a Haskell implementation (including its type) of the 
function mapEach such that mapEach f xss returns a list of 
lists, containing the result of applying f to each element 
of each list in xss.

For example: 

    mapEach (+ 2) [[5, 4, 1], [7, 6], []] 
  = [[7, 6, 3], [9, 8], []]

--------
Answer:

> mapEach :: (a -> b) -> [[a]] -> [[b]]
> mapEach f = map (map f)

This implementation uses two nested calls to map:
• The outer map applies the function (map f) to each list 
  in the list of lists xss
• The inner map applies the function f to each element
  of each list in xss

Alternatively, we could write it as follows:

> mapEach' f xss = [map f xs | xs <- xss]

The list comprehension version is in fact syntactic sugar
for the previous implementation.


--------------------------------
Question 3.2:
Given is the function gcd that returns the greatest common 
divisor of its two arguments (i.e. gcd 36 42 returns 6):

> gcd :: Int -> Int -> Int
> gcd a b = if b == 0 then a else gcd b (a `mod` b)

Implement the function listgcd that takes a list of 
integers, and returns the greatest common divisor of all 
elements in the list. Your implementation must make use of 
the function foldr. 

For example:  listgcd [25,15,125,555] = 5

--------
Answer:

> listgcd :: [Int] -> Int
> listgcd = foldr gcd 0

We can set the initial accumulator value to 0 because of
how gcd is defined: gcd a 0 = a for any integer a.
Using foldr, we can apply the gcd function to each new
element taken from the input list and the current
intermediate accumulator value. This way, we reduce the
list to a single value, which is the greatest common
divisor of all elements in the list.


--------------------------------
Question 3.3:
Consider the following haskell definition of the function 
scanl:

  scanl f z xs = 
    [foldr f z (take len xs) | len <- [0..length xs]]

For example, scanl (+) 1 [1..10] returns 
[1,2,4,7,11,16,22,29,37,46,56]. The above implementation 
of scanl is quite inefficient (it has quadratic time 
complexity). Give an equivalent implementation (including
its type) that runs in linear time.

--------
Answer:

What this function actually does is collect the inter-
mediate accumulator values in a list when we run foldr
over the full list. So we can implement scanl in a more
efficient way by simply modifying the accumulator in the
definition of foldl, so that we can build the list of
intermediate results as we traverse the input list.

> scanl :: (b -> a -> b) -> b -> [a] -> [b]
> scanl f q []     = [q]
> scanl f q (x : xs) = q : scanl f (f q x) xs

When the input list is empty, we return the singleton list 
[q], which is the initial accumulator value.
In the recursive case we prepend the current accumulator q 
to the result list, and then recurse with the new 
accumulator f q x and the tail of the input list.

This method traverses the list only once, making it run in
linear time. The accumulator is updated at each step, and 
the intermediate results are collected in the result list 
as we go along, without the need to recompute them multiple
times as in the original implementation.


___________________________________________________________

4. List comprehensions
___________________________________________________________

Question 4.1:
Give an implementation of the standard Haskell function 
filter (including its type) using a list comprehension.

--------
Answer:

> filter :: (a -> Bool) -> [a] -> [a]
> filter p xs = [x | x <- xs, p x]

This implementation uses a list comprehension to iterate
over the input list xs and includes only those elements x
for which the predicate p returns True. The result is a new
list containing only those elements that satisfy the
predicate.


--------------------------------
Question 4.2:
Write a function sumdiv3not5 (including its type) that 
takes a list of Integers and returns the sum of the squares
of those numbers in the list that are divisible by 3 but 
not by 5. 

  For example: sumdiv3not5 [-6,15,2,3] = 45

You must use a list comprehension, and are not allowed 
to use recursion.

--------
Answer:

> sumdiv3not5 :: [Integer] -> Integer
> sumdiv3not5 xs = 
>   sum [x*x | x <- xs, x `mod` 3 == 0, x `mod` 5 /= 0]

Traversing the input list xs, we use a list comprehension
to filter out the elements that are divisible by 3 but not
by 5, and then we compute the square of each of these
elements. Finally, we sum up the squares of the filtered
elements using the standard sum function included in the
functions.md file.


--------------------------------
Question 4.3:
Give an implementation of the standard Haskell function 
zipWith (including its type) using a list comprehension.

--------
Answer:

> zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
> zipWith f xs ys = [f x y | (x, y) <- zip xs ys]

Using zip, we pair up the elements of the two input
lists xs and ys, stopping when the shorter list runs out
of elements. Then, we use a list comprehension to apply 
the function f to each pair (x, y) of elements from the
two lists, producing the resulting list of type [c].


___________________________________________________________

5. Infinite lists
___________________________________________________________

Question 5.1:
Give a definition of the infinite list inits (including its 
type) of which the n-th element is a list containing the 
numbers 0,1,2,..,n. 

Example:    take 5 inits 
          = [[0],[0,1],[0,1,2],[0,1,2,3],[0,1,2,3,4]]

--------
Answer:

> inits :: [[Integer]]
> inits = map (\x -> [0..x])  [0..]

This implementation uses the map function to apply a lambda
function to each element x in the infinite list [0..]. The
lambda function simply replaces each x with the list 
[0..x].

Alternatively, we can use a list comprehension:

> inits' = [[0..x] | x <- [0..]]

This version does the same thing: it repeatedly extracts a 
number x from the infinite list [0..] and constructs the 
list [0..x] for it, resulting in the same infinite list of 
lists.


--------------------------------
Question 5.2:
Give a definition of the function powerfunc (including its 
type) that accepts a function f on its input, and outputs
the infinite list of repeated applications of f. The first 
element (index 0) of this list should be the function f 
raised to the power zero, i.e. the identity function. 
The second element (index 1) is the function f raised to 
the power one, i.e. f itself. The third element (index 2) 
is the function f raised to the power two, i.e. f(f(x)) 
for all x, and so on.

For example:  

    head ((drop 5) (powerfunc (+2))) 0 = 10

--------
Answer:

The question asks for nothing more than a definition of the
function iterate. We would normally define it as follows:

> powerfunc :: (a -> a) -> a -> [a]
> powerfunc f x = x : map f (powerfunc f x)

However, the question makes it clear that the function 
powerfunc should not take an initial value x, but rather
should return a list of functions, where the n-th element
is the function f raised to the power n. Thus, we can
modify the previous version to return a list of functions
instead of a list of values:

> powerfunc' :: (a -> a) -> [a -> a]
> powerfunc' f = id : map (f .) (powerfunc' f)


--------------------------------
Question 5.3:
Consider the following Haskell code:

  f a b = a : b

  g a [] = a : []
  g a b = a : b

  list1 = foldr f [] [1..]
  list2 = foldr g [] [1..]

What will happen if we try to compute take 10 list1? 
And what happens if we try to compute take 10 list2?
Explain your answers.

--------
Answer:

Computing take 10 list1 results in the list:
[1,2,3,4,5,6,7,8,9,10]. This is because the function f
is defined to take two arguments a and b, and it constructs
a list by prepending a to b. Using foldr f [] [1..] builds
the list lazily. Haskell will evaluate just enough to 
satisfy the take 10 operation, meaning it only evaluates
the first 10 elements of the infinite list.

Computing take 10 list2 results in an infinite loop.
Just like f, the function g is defined to take two 
arguments a and b, creating a list by prepending a to b. 
However, it has a special case for the empty list [] as its
second argument, and this case comes first in the pattern
matching. Since pattern matching in Haskell checks patterns
top-down, this means that when g is called with an infinite 
list as its second argument, it will always try to match the 
first case, but it will never succeed. This is because it
needs to evaluate the entire second argument b to check if 
it is empty, and since b is an infinite list, it will never 
reach the end of it. As a result, the evaluation will never
terminate, leading to an infinite loop.

Note that if we swapped the order of the cases in g, then
foldr g [] [1..] would behave like foldr f [] [1..],
resulting in the same lazy evaluation and yielding the same
result as list1.


___________________________________________________________

6. ADT module
___________________________________________________________

The unary numeral system is the simplest numeral system to 
represent natural numbers. To represent the natural number 
N, an arbitrarily chosen symbol representing one is 
repeated N times. For example, the number 5 can be 
represented by the list [1, 1, 1, 1, 1] (here, the 
arbitrary chosen symbol is the digit 1). Hence, in this 
notation, the length of the list is the actual value it 
represents. We represent the value zero by the empty list.

The type NatNum is an Abstract Data Type (ADT) for 
implementing natural numbers. Its implementation uses the 
unary numeral system. Implement a module NatNum such that 
the concrete implementation of the type NatNum is hidden 
to the user.

The following operations on natural numbers need to be 
implemented:

• integerToNat n converts the Integer n into 
  the NatNum that represents n.
• natToInteger n converts the natural number n 
  into its decimal Integer value.
• isZero n returns True if and only if the 
  natural number n represents 0.
• isLessThan a b: returns True if and only if the
  natural number a is less than the natural number b.
• plus a b: returns the natural number that is 
  obtained by adding the natural numbers a and b.
• mul a b: returns the natural number that is 
  obtained by multiplying the natural numbers a and b.

--------
Answer:

To turn the below code into a module, you would create
a file named NatNum.hs and start it with the following
module declaration:

module NatNum (
  NatNum, integerToNat, natToInteger,
  isZero, isLessThan, plus, mul
) where

This line exports the abstract data type NatNum and its
associated functions, but does not export the constructor
Nat, thus hiding the concrete implementation details. It
does not export the helper function len either.

> -- we use () as the dummy symbol
> data NatNum = Nat [()]

> -- show instance for pretty printing
> instance Show NatNum where
>   show (Nat xs) = show (natToInteger (Nat xs))

> -- converts an Integer to a NatNum by creating a list
> -- of the dummy symbol () repeated n times
> integerToNat :: Integer -> NatNum
> integerToNat n = Nat [() | _ <- [1..n]]

> -- converts the NatNum to an Integer by returning
> -- the length of the list of symbols
> natToInteger :: NatNum -> Integer
> natToInteger (Nat xs) = len xs

> -- returns True if the NatNum represents zero
> isZero :: NatNum -> Bool
> isZero (Nat xs) = xs == [] 

> -- comparison is based on the length of the lists
> isLessThan :: NatNum -> NatNum -> Bool
> isLessThan (Nat xs) (Nat ys) = len xs < len ys

> -- addition is defined as concatenation of lists
> plus :: NatNum -> NatNum -> NatNum
> plus (Nat xs) (Nat ys) = Nat (xs ++ ys)

> -- multiplies two NatNums using repeated addition
> mul :: NatNum -> NatNum -> NatNum
> mul (Nat [])       _ = Nat []
> mul (Nat (x : xs)) z = plus z (mul (Nat xs) z) 

> -- helper function: computes the length of a list and
> -- returns an Integer. This is needed because the 
> -- standard function length returns an Int and we are
> -- not allowed to use toInteger  
> len :: [a] -> Integer
> len []       = 0
> len (_ : xs) = 1 + len xs


Example usage:

ghci> x = integerToNat 5
ghci> y = integerToNat 10
ghci> natToInteger x  
5
ghci> isZero x
False
ghci> plus x y
15
ghci> mul x y
50
ghci> isLessThan x y
True


___________________________________________________________

7. Proof on foldr and foldl
___________________________________________________________

Consider the following Haskell definitions of the functions
foldr and foldl:

  foldr f z [] = z
  foldr f z (x:xs) = f x (foldr f z xs)

  foldl f z [] = z
  foldl f z (x:xs) = foldl f (f z x) xs

Prove the following property p: 

  p(xs) : foldr (+) z xs = foldl (+) z xs 
          for all finite lists xs

--------
Answer:

We will prove the property p by structural induction on the
list xs.

----------------------------------------
Base case: prove p([])
----------------------------------------

    {LHS of p([])}
  foldr (+) z []
=   {applying foldr}
  z
=   {unapplying foldl}
  foldl (+) z [] 
    {RHS of p([])}

----------------------------------------
Inductive step: prove p(xs) => p((x:xs))
----------------------------------------

    Induction hypothesis:
      p(xs): foldr (+) z xs = foldl (+) z xs

    {LHS of p((x:xs))}
  foldr (+) z (x:xs)
=   {applying foldr}
  x + (foldr (+) z xs)
=   {using induction hypothesis}
  x + (foldl (+) z xs)

    {RHS of p((x:xs))}
  foldl (+) z (x:xs)
=   {applying foldl}
  foldl (+) (z + x) xs

In order to show that the two sides are equal, we
need to prove the following lemma q.

----------------------------------------
Lemma q
          q(xs): x + (foldl (+) z xs) 
                 = foldl (+) (z + x) xs
----------------------------------------

We will prove this lemma q by structural 
induction on the list xs.

----------------------------------------
Base case: prove q([])
----------------------------------------

    {LHS of q([])}
  x + (foldl (+) z [])
=   {applying foldl}
  x + z
=   {commutativity of (+)}
  z + x
=   {unapplying foldl}
  foldl (+) (z + x) []
    {RHS of q([])}

----------------------------------------
Inductive step: prove q(xs) => q((y:xs))
----------------------------------------

    Induction hypothesis:
      q(xs): x + (foldl (+) z xs) = foldl (+) (z + x) xs

    {LHS of q((y:xs))}
  x + (foldl (+) z (y:xs))
=   {applying foldl}
  x + (foldl (+) (z + y) xs)
=   {using induction hypothesis}
  x + foldl (+) ((z + y) + x) xs
=   {associativity and commutativity of (+)}
  foldl (+) ((x + z) + y) xs
=   {unapplying foldl}
  foldl (+) (x + z) (y:xs)  
    {RHS of q((y:xs))}

□

Since we have shown that the lemma q holds, we can now
conclude that the property p holds for all finite lists xs.

□


___________________________________________________________

8. Proof on trees
___________________________________________________________

Given is the data type Tree, and the functions mirror 
and inorder:

  data Tree a = Empty | Node a (Tree a) (Tree a)

  mirror :: Tree a -> Tree a
  mirror Empty = Empty
  mirror (Node x l r) = Node x (mirror r) (mirror l)

  inorder :: Tree a -> [a]
  inorder Empty = []
  inorder (Node x l r) = inorder l ++ [x] ++ inorder r

Prove for all finite trees t: 

  p(t): inorder(mirror t) = reverse(inorder t)

You may use, without proof, that the operator ++ is 
associative, i.e. xs++ys++zs = (xs++ys)++zs = xs++(ys++zs).
For the definition of reverse, refert to functions.md.

--------
Answer:

We will prove the property p by structural induction on the
tree t.

----------------------------------------
Base case: prove p(Empty)
----------------------------------------

    {LHS of p(Empty)}
  inorder(mirror Empty)
=   {applying mirror}
  inorder Empty
=   {applying inorder}
  []
=   {unapplying reverse}
  reverse []
=   {unapplying inorder}
  reverse(inorder Empty)
    {RHS of p(Empty)}

----------------------------------------
Inductive step: prove p(l) ∧ p(r) 
                      => p(Node x l r)
----------------------------------------

    Induction hypothesis:
      p(l): inorder(mirror l) = reverse(inorder l)
      p(r): inorder(mirror r) = reverse(inorder r)

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
Lemma q
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
=   {as noted, we may use without proof that 
     xs = xs ++ [], with xs = reverse ys}
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