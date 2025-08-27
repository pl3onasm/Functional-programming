> import Prelude hiding (foldl, repeat)

-----------------------------------------------------------
             EXAM FUNCTIONAL PROGRAMMING 2018               
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

  True : [] : []

--------
Answer: 

No, this is not type correct. The operator (:) is right-
associative, so the expression is parsed as: 
  True : ([] : [])

Prepending the empty list to the empty list results in 
[[a]] for some type a. To this the Boolean value True is 
prepended, but this is not compatible with the type of the 
list elements: [a] cannot be unified with Bool.
The expression would be type correct if it were written as:
(True : []) : []  to override the right-associativity. Now
we are prepending a list of Booleans to a list of lists, 
and obtain the type [[Bool]].


--------------------------------
Question 1.2:
Is the following expression type correct? 
If YES, then give the most general type of the expression.

  (True: []) : []

--------
Answer:

Yes, this expression is type correct. The operator (:) is
right-associative, but in this case that property is 
overridden by the parentheses. The expression is parsed as:
  (True : []) : [].

Here, (True : []) is a list of Booleans, and we are
prepending it to the empty list, which has type [a] for 
some type a. Unifying the types yields a = Bool. This 
produces a list of type [Bool], which is then prepended
to the empty list of type [b], for some type b. Unifying
the types now yields b = [Bool]. Thus, the most general
type of the expression is [[Bool]].


--------------------------------
Question 1.3:
Is the following expression type correct? 
If YES, then give the most general type of the expression.

  (True: []) : [] ++ [False]

--------
Answer: 

No, this is not type correct. The operator (:) is right-
associative, and (++) has the same precedence as (:), 
(you can check this in GHCi whith :i (:) and :i (++), 
and you will see that both have precedence level 5), so the
expression is parsed as:
  (True : []) : ([] ++ [False])

The right-hand side constructs a list of Booleans, having
the type [Bool]. The left-hand side also constructs a list
of Booleans by prepending True to the empty list. However,
the operator (:) in the middle expects an element of type
a to prepend to a list of type [a], but both sides of the
operator have type [Bool], leading to a type error.


--------------------------------
Question 1.4:
Is the following expression type correct? 
If YES, then give the most general type of the expression.

  (True : []) : [] ++ [[False]]

--------
Answer: 

Yes, this expression is type correct. By adjusting the 
previous list [False] to a list of lists [[False]], the
expression now prepends a list of Booleans to a list of
lists of Booleans. So that we obtain the list 
[[True], [False]], which has the type [[Bool]].


--------------------------------
Question 1.5:
What is the most general type of the following function f?

  f = map . filter

--------
Answer: 

Let us first look at the types of the functions involved: 
  map :: (a -> b) -> [a] -> [b]
  filter :: (a -> Bool) -> [a] -> [a]
  (.) :: (b -> c) -> (a -> b) -> a -> c

Since (.) only works on unary functions, the type of filter
is forced to be interpreted in its curried form as:
  filter :: (a -> Bool) -> ([a] -> [a])

That way, filter h (for some predicate h::(a -> Bool)) is a 
unary function of type [a] -> [a], which can be composed 
with map.

As we know the composition (.) f g can be expanded as
  f . g = \x -> f (g x) 
For the composition map . filter, this means:  
  map . filter = \h -> map (filter h)
where h is a predicate of type (a -> Bool).

So map takes the output of filter h, which is a unary
function of type [a] -> [a], as its first argument.
Once given its first argument, map becomes a unary function
of type:   map :: [[a]] -> [[a]]

Putting this all together, we have for the type of f,
also a unary function after the composition:
  f :: (a -> Bool) -> [[a]] -> [[a]]
which should be interpreted as:
  f :: (a -> Bool) -> ([[a]] -> [[a]])

That is, f is a function that takes a predicate h of type 
(a -> Bool) and returns a function of type ([[a]] -> [[a]])
that applies filter h to every inner list of a list of 
lists. So given a list of lists [[a]], it produces a list 
of lists of the same type [[a]], where each sublist has 
been filtered according to h.


___________________________________________________________

2. Programming in Haskell
___________________________________________________________

This problem is about pattern matching. A pattern is a 
String that specifies (describes) the strings that match 
the pattern. A pattern may only consist of lower case 
letters from the alphabet (i.e. a..z), asterisks (i.e. 
the * character), and question marks (i.e. the ? char). 
A question mark may only follow a letter and indicates zero
or one occurrence of the preceding character. For example, 
colou?r matches both color and colour. An asterisk may only 
follow a letter and indicates zero or more occurrences of 
the preceding character. 
For example, ab*c matches ac, abc, abbc, abbbc, and so on.

Write a Haskell function 

isMatch :: String -> String -> Bool 

such that isMatch pat str returns True if and only the 
string str can be produced by the pattern pat. 
For example: 

  isMatch "h?i?el*o?" "hello" = True
  isMatch "h?iel*" "ill" = False.

--------
Answer:

> isMatch :: String -> String -> Bool
> isMatch [] xs = xs == [] 

> isMatch (c : '?' : cs) xs = skipOrOnce c cs xs
> isMatch (c : '*' : cs) xs = skipOrMany c cs xs

> isMatch (c : cs) (x : xs) = c == x && isMatch cs xs
> isMatch _ _ = False

> -- helper: zero or one occurrence
> skipOrOnce :: Char -> String -> String -> Bool
> skipOrOnce c cs xs = isMatch cs xs || case xs of
>     (x : xs') | x == c -> isMatch cs xs'
>     _ -> False

> -- helper: zero or more occurrences
> skipOrMany :: Char -> String -> String -> Bool
> skipOrMany c cs xs = isMatch cs xs || case xs of
>     (x : xs') | x == c -> isMatch (c : '*' : cs) xs'
>     _ -> isMatch cs xs

The function isMatch is defined recursively and is defined
as follows:

• The first line of the function is the base case, which
checks if the pattern is empty. If it is, then the string
must also be empty to return True.
• The second line checks if the first character of the
pattern is followed by a question mark. If so, it calls
the helper function skipOrOnce. This helper either skips
the character c in the pattern or consumes it once if the
character matches the first character of the string.
• The third line checks if the first character of the
pattern is followed by an asterisk. If so, it calls the
helper function skipOrMany. This helper either skips the
character c in the pattern or consumes it one or more
times if the character matches the first character of the
string.
• The fourth line checks if the first character of the
pattern matches the first character of the string. If it
does, it recursively calls isMatch on the rest of the
pattern and the rest of the string.
• The last line is the catch-all case that returns False 
if the pattern does not match the string.

Note that the helper functions need to explore two cases:
the case where the character is skipped and the case where
the character is consumed. This is necessary for cases 
like:    isMatch "a?ab" "ab"
         isMatch "a*ab" "ab"
If we only explored the case where the character is
consumed because the first character matches, we would not
be able to match the string "ab" with the pattern "a?ab"
or "a*ab".


___________________________________________________________

3. Higher-order functions
___________________________________________________________

Question 3.1:
Use the higher-order function foldr to implement the 
function factorial (including its type) which takes a non-
negative integer n and return the factorial of n 
(i.e. n*(n-1)*(n-2)*...*1). 

Example:  factorial 5 = 120

--------
Answer:

> factorial :: Integer -> Integer
> factorial n = foldr (*) 1 [1..n]

The function foldr takes a binary function (here (*)), 
an initial accumulator value (here 1), and a list (in this 
case [1..n]). It applies the function to the elements of 
the list from right to left, combining them with the
accumulator value.

So for n = 5, we obtain: 
  foldr (*) 1 [1..5]
= 1 * (2 * (3 * (4 * (5 * 1))))
= 1 * (2 * (3 * (4 * 5)))
= 1 * (2 * (3 * 20))
= 1 * (2 * 60)
= 1 * (120)
= 1 * 120
= 120

Since multiplication is associative, the order in which the
elements are combined does not matter, and we can use
foldr to compute the factorial in this way.


--------------------------------
Question 3.2:
The higher order function foldr is used for reducing a list
as in the following example:

  foldr f 0 [1..5] = f 1 (f 2 (f 3 (f 4 (f 5 0))))

Implement the 'mirror' operation foldl (including its type)
such that foldl f 0 [1..5] = f (f (f (f (f 0 1) 2) 3) 4) 5

--------
Answer:

> foldl :: (a -> b -> a) -> a -> [b] -> a
> foldl f z [] = z
> foldl f z (x : xs) = foldl f (f z x) xs

The function foldl takes a binary function f, an initial
accumulator value z, and a list xs. It applies the function
f to the accumulator and the first element of the list,
then recursively calls itself with the updated accumulator
and the rest of the list. 

This produces a left-associative combination of the 
elements of the list with the accumulator, as follows:
  f (f (f (f (f z x₁) x₂) x₃) x₄) x₅

Note that type signature of foldl is slightly different 
from foldr, as it takes a function where the first argument 
is the accumulator and the second argument is the current 
element of the list, whereas foldr takes a function where 
the first argument is the current element of the list and 
the second argument is the accumulator. The difference in
the order of arguments is crucial for the operation of
foldl, and results in a left-associative operation,
whereas foldr is right-associative.


--------------------------------
Question 3.3:
Using function composition (.), foldr and the cons operator 
(:), implement the function folmap (including its type), 
which is your version of the standard function map. 

Example:   folmap (*2) [1,2,3,4] = [2,4,6,8]

--------
Answer:

> folmap :: (a -> b) -> [a] -> [b]
> folmap f xs = foldr ((:) . f) [] xs

In this implementation, foldr traverses the list xs from 
right to left, applying f to each element and prepending 
the result to the accumulator, which starts as [].
The expression ((:) . f) composes f with (:), creating a 
function that takes an element, applies f, and prepends 
the result.
This builds a new list in the same order as the input, 
reproducing the behavior of map.


___________________________________________________________

4. List comprehensions
___________________________________________________________

Question 4.1:
Implement the function pairs (including its type) using a 
list comprehension. 

Example:  pairs [1..3] ['a','b']
        = [(1,'a'),(1,'b'),(2,'a'),(2,'b'),(3,'a'),(3,'b')]

--------
Answer:

> pairs :: [a] -> [b] -> [(a, b)]
> pairs xs ys = [(x, y) | x <- xs, y <- ys]

The function pairs takes two lists xs and ys and produces 
all possible pairs (x, y) where x comes from xs and y from 
ys. The list comprehension iterates through every element 
of xs, and for each x, it pairs it with every element y in 
ys.


--------------------------------
Question 4.2:
Use a list comprehension to implement the function 
locations (including its type) that takes a value of some
type and a list of that type, and returns a list with 
locations (indexes starting from zero) where the value 
occurs in the list.

For example:    locations 1 [1,0,1,0,4,1] = [0,2,5]

--------
Answer:

> locations :: Eq a => a -> [a] -> [Int]
> locations z xs = [i | (x, i) <- zip xs [0..], x == z]

The function locations pairs each element of the list xs 
with its index using zip xs [0..]. The list comprehension 
then filters these pairs, keeping only those where x 
matches the target value z. The resulting list contains 
exactly the indices where z occurs in xs.


--------------------------------
Question 4.3:
The function sumProdPairs = zipWith (\x y -> (x+y,x*y)) 
is defined using the function zipWith.
Give an equivalent definition of sumProdPairs that uses 
a list comprehension instead.

--------
Answer:

> sumProdPairs :: Num a => [a] -> [a] -> [(a, a)]
> sumProdPairs xs ys = [(x+y,x*y) | (x, y) <- zip xs ys]

The function zip xs ys combines the two input lists element
by element into pairs (x, y). The list comprehension then 
iterates over these pairs and computes both their sum x + y
and product x * y, forming a tuple for each pair. 
This produces the same result as the original zipWith 
definition, but expressed using a list comprehension.


___________________________________________________________

5. Infinite lists
___________________________________________________________

Question 5.1:
Give a definition of the Haskell function repeat (including 
its type) that takes an argument and produces the list that
indefinitely repeats that argument. 

Example:    repeat 42 = [42,42,42,42,42,42,....]

--------
Answer:

> repeat :: a -> [a]
> repeat x = x : repeat x

The function repeat takes an argument x and constructs an
infinite list by prepending x to the result of calling
repeat x again. This creates a recursive definition that
produces an infinite list of the same element x.

For example, calling repeat 42 will yield:
  repeat 42 = 42 : repeat 42
            = 42 : (42 : repeat 42)
            = 42 : (42 : (42 : repeat 42))
            = [42, 42, 42, ...]


--------------------------------
Question 5.2:
Give a definition of the infinite list binaries which is 
the list of all non-empty lists containing zeros and ones.
The order of the elements of the list should be as in the 
following example: 

    take 14 binaries
    = [[0],[1],[0,0],[1,0],[0,1],[1,1],[0,0,0],
       [1,0,0],[0,1,0],[1,1,0],[0,0,1],[1,0,1],
       [0,1,1],[1,1,1]]

--------
Answer:

> binaries :: [[Integer]]
> binaries = [0] : [1] : [d : b | b <- binaries, d <- [0,1]]

The function binaries constructs an infinite list of lists
of zeros and ones. It starts with the base cases [0] and
[1], and then it generates all other lists by prepending
either 0 or 1 to each list in the existing binaries list.
This is done using a list comprehension that iterates over
the existing binaries and prepends each element with both
0 and 1, thus generating all possible combinations in the
correct order.


--------------------------------
Question 5.3:
Consider (x + 1)^n, for integer n ≥ 0. We can write this in
coefficient normal form, i.e. in the form:

      aₙx^n + aₙ₋₁x^{n-1} + .. + a₀
    
For example, (x + 1)^4 = x^4 + 4x^3 + 6x^2 + 4x + 1, yields 
the list of coefficients [1,4,6,4,1]. 
  
Give a definition of the infinite list coefficients of 
lists of coefficients, such that the n-th list corresponds 
with the coefficients of (x + 1)^n.

For example: 

    take 5 coefficients 
    = [[1],[1,1],[1,2,1],[1,3,3,1],[1,4,6,4,1]]

--------
Answer:

> coefficients :: [[Integer]]
> coefficients = [1] : map next coefficients
>   where
>     next xs = zipWith (+) (0 : xs) (xs ++ [0]) 

As we know, the coefficients of (x + 1)^n are given by the
binomial coefficients, which can be computed using the
Pascal triangle. In this triangle, each number is the sum 
of the two numbers directly above it. 
The function generates the coefficient lists by taking the 
previous list of coefficients and adding the elements 
pairwise, with a zero prepended to the left and a zero 
appended to the right, so that the pairs are added with
the correct offsets. The pairwise addition is done using 
the zipWith function.
We take the first three lists of coefficients to see how
this works:
  
  next [1] = zipWith (+) (0 : [1]) ([1] ++ [0])
           = zipWith (+) [0,1] [1,0]
           = [0+1, 1+0]
           = [1,1]
  
  next [1,1] = zipWith (+) (0 : [1,1]) ([1,1] ++ [0])
             = zipWith (+) [0,1,1] [1,1,0]
             = [0+1, 1+1, 1+0]
             = [1,2,1]
  
  next [1,2,1] = zipWith (+) (0 : [1,2,1]) ([1,2,1] ++ [0])
               = zipWith (+) [0,1,2,1] [1,2,1,0] 
               = [0+1, 1+2, 2+1, 1+0]
               = [1,3,3,1]


___________________________________________________________

6. ADT module
___________________________________________________________

The type Peano is an Abstract Data Type (ADT) for 
implementing natural numbers as follows:

  • Zero is a constructor that represents 
    the natural number 0.
  • Succ n, where n is of the type Peano, represents 
    the number that is 1 greater than the number 
    that n represents.

Implement a module Peano such that the concrete 
implementation of the type Peano is hidden to the user.

The following operations on Peano numbers need to be 
implemented:

• peanoToInteger n converts the Peano number n 
  into its decimal Integer value.
• isZero n returns True if and only if the 
  peano Number n represents 0.
• isLessThan a b: returns True if and only if 
  the Peano number a is less than the Peano number b.
• plus a b: returns the Peano representation of 
  adding the Peano numbers a and b.
• mul a b: returns the Peano representation of 
  multiplying the Peano numbers a and b.

--------
Answer:

To turn the below code into a module, you would create
a file named Peano.hs and start it with the following
module declaration:

module Peano (
    Peano,
    peanoToInteger,
    isZero,
    isLessThan,
    plus,
    mul
) where

This line exports the abstract data type Peano and its 
associated functions, but does not export the 
constructors of Peano, thus hiding the concrete 
implementation details.

> data Peano = Zero | Succ Peano

> -- instance for Peano to make it showable 
> instance Show Peano where
>   show n = show (peanoToInteger n)

> -- converts a Peano number to its Integer value
> peanoToInteger :: Peano -> Integer
> peanoToInteger (Zero)   = 0
> peanoToInteger (Succ x) = 1 + peanoToInteger x

> -- checks if the Peano number is Zero
> isZero :: Peano -> Bool
> isZero Zero = True
> isZero _    = False

> -- checks if the first Peano number is less than 
> -- the second Peano number by peeling off the first
> -- Succ and comparing the rest of the Peano numbers
> isLessThan :: Peano -> Peano -> Bool
> isLessThan  Zero    (Succ _) = True
> isLessThan (Succ x) (Succ y) = isLessThan x y
> isLessThan  _        _       = False 

> -- adds two Peano numbers by peeling off the first
> -- Succ and pushing it onto the second Peano number
> plus :: Peano -> Peano -> Peano
> plus  Zero    y = y
> plus (Succ x) y = plus x (Succ y)

> -- multiplies two Peano numbers by repeated addition
> mul :: Peano -> Peano -> Peano
> mul  Zero    _ = Zero
> mul (Succ x) y = plus y (mul x y)

> -- extra function, so we can easily make Peano numbers
> integerToPeano :: Integer -> Peano
> integerToPeano n
>   | n < 0     = error "Negative numbers not allowed"
>   | n == 0    = Zero
>   | otherwise = Succ (integerToPeano (n - 1))


Example usage:

ghci > x = integerToPeano 5
ghci > y = integerToPeano 3
ghci > peanoToInteger x
5
ghci > plus x y
8
ghci > mul x y
15
ghci > isLessThan x y
False
ghci > isZero x
False


___________________________________________________________

7. Proof of equality
___________________________________________________________

Consider the following Haskell functions.

    f 0     = 0
    f 1     = 1
    f n     = f (n-1) + f (n-2)
    
    g 0 a b = a
    g n a b = g (n-1) b (a+b)

Prove the following property p:

    p(n):   f n = g n 0 1 
            for all non-negative integers n

--------
Answer:
 
We will prove this property by natural strong induction
on n. Strong induction is used here because the definition
of f depends on the two previous values f(n-1) and f(n-2).

------------------------------------
Base cases: prove p(0) and p(1)
------------------------------------

Case 1: n = 0

    {LHS of p(0)}
  f 0
=   {applying f 0 = 0}
  0
=   {unapplying g 0 0 1 = 0}
  g 0 0 1
    {RHS of p(0)}

Case 2: n = 1

    {LHS of p(1)}
  f 1
=   {applying f 1 = 1}
  1
=   {unapplying g 1 0 1 = 1}
  g 1 0 1
    {RHS of p(1)}

------------------------------------
Induction step: prove p(n) => p(n+1)
------------------------------------

    As we are using strong induction, we assume that the
    property holds for all  0 <= k <= n, where k ∈ ℕ.

    Induction hypothesis: p(n) and p(n-1)
      p(n):   f n     = g n 0 1
      p(n-1): f (n-1) = g (n-1) 0 1

    {LHS of p(n+1)}
  f (n+1)
=   {applying f}
  f n + f (n-1)
=   {applying induction hypothesis for p(n)}
  g n 0 1 + f (n-1)
=   {applying induction hypothesis for p(n-1)}
  g n 0 1 + g (n-1) 0 1
=   {applying lemma q below}
  g (n+1) 0 1
    {RHS of p(n+1)}

□

For the proof above, we needed a lemma that allows us to
combine the two calls to g in the last step to obtain a
single call to g with the correct arguments. Proving this
lemma for the specific case where a = 0 and b = 1 proves
to be too difficult. It does not provide a strong enough
induction hypothesis to work with. In order to get around
this and strenghen the induction hypothesis, we will use
the common technique of generalizing the property we want
to prove. This means that we will first prove a lemma q
where the arguments a and b can be any finite natural
numbers. If this lemma holds, then it also holds for the
specific case where a = 0 and b = 1.

----------------------------------------------------
Lemma q

      q(n): g (n+2) a b = g (n+1) a b + g n a b
            for all non-negative integers n 
----------------------------------------------------

We will prove this lemma by natural induction on n. 

------------------------------------
Base case: prove q(0)
------------------------------------

    {LHS of q(0)}
  g 2 a b
=   {applying g}
  g 1 b (a+b)
=   {unapplying g}
  g 0 (a+b) (b + (a+b))
=   {applying g 0 a b = a}
  a + b
=   {unapplying g}
  g 0 a b + g 0 b (a+b)
=   {unapplying g}
  g 0 a b + g 1 a b
    {arithmetic}
= g 1 a b + g 0 a b
    {RHS of q(0)}

------------------------------------
Induction step: prove q(n) => q(n+1)
------------------------------------

    Induction hypothesis: 
      q(n):   g (n+2) a b = g (n+1) a b + g n a b

    {LHS of q(n+1)}
  g (n+3) a b
=   {applying g}
  g (n+2) b (a+b)
=   {applying induction hypothesis}
  g (n+1) b (a+b) + g n b (a+b)
=   {unapplying g}
  g (n+2) a b + g (n+1) a b
    {RHS of q(n+1)}

□


___________________________________________________________

8. Proof on trees
___________________________________________________________

Given is the data type Tree and the functions 
foldT, mapT, and inorder:

  data Tree a = Empty | Node a (Tree a) (Tree a)

  foldT :: (a -> a -> a) -> a -> Tree a -> a
  foldT f z Empty = z
  foldT f z (Node x l r) = 
      f (f (foldT f z l) x) (foldT f z r)

  inorder :: Tree a -> [a]
  inorder Empty = []
  inorder (Node x l r) = 
      inorder l ++ [x] ++ inorder r

Let f :: a -> a -> a be an associative function 
(i.e. f a (f b c) = f (f a b) c) with identity element z 
such that f x z = f z x = x.

Prove for all finite trees t: 

  p(t): foldT f z t = foldr f z (inorder t)

[Note: You may use that the operator ++ is associative 
 without giving a proof.
 For the definitions of foldr and ++, see functions.md.]

--------
Answer:

We will prove this property by structural induction on the
tree t.

--------------------------------------
Base case: prove p(Empty)
--------------------------------------

    {LHS of p(Empty)}
  foldT f z Empty
=   {applying foldT}
  z
=   {unapplying foldr}
  foldr f z []
=   {unapplying inorder}
  foldr f z (inorder Empty)
    {RHS of p(Empty)}

--------------------------------------
Induction step: prove p(l) ∧ p(r)
                      => p(Node x l r)
--------------------------------------

    Induction hypothesis:
      p(l): foldT f z l = foldr f z (inorder l)
      p(r): foldT f z r = foldr f z (inorder r)

    {LHS of p(Node x l r)}
  foldT f z (Node x l r)
=   {applying foldT}
  f (f (foldT f z l) x) (foldT f z r)


    {RHS of p(Node x l r)}
  foldr f z (inorder (Node x l r))
=   {applying inorder}
  foldr f z (inorder l ++ [x] ++ inorder r)
=   {applying associativity of (++) without proof}
  foldr f z ((inorder l ++ [x]) ++ inorder r)
=   {applying lemma q}
  f (foldr f z (inorder l ++ [x])) (foldr f z (inorder r))
=   {applying lemma q again}
  f (f (foldr f z (inorder l) (foldr f z [x]))) 
    (foldr f z (inorder r))
=   {applying both induction hypotheses}
  f (f (foldT f z l) (foldr f z [x])) (foldT f z r)
=  {unapplying foldr}
  f (f (foldT f z l) x) (foldT f z r)
    {RHS of p(Node x l r)}

□

--------------------------------------------------
Lemma q
         q(xs):  foldr f z (xs ++ ys)  
                 = f (foldr f z xs) (foldr f z ys)
--------------------------------------------------

We will prove this lemma by structural induction on the
list xs.

----------------------------------------
Base case: prove q([])
----------------------------------------

    {RHS of q([])}
  f (foldr f z []) (foldr f z ys)
=   {applying foldr}
  f z (foldr f z ys)
=   {z is the identity element of f}
  foldr f z ys
=   {unapplying ++}
  foldr f z ([] ++ ys)
    {LHS of q([])}

----------------------------------------
Induction step: prove q(xs) => q((x:xs))
----------------------------------------

    Induction hypothesis: 
      q(xs):  foldr f z (xs ++ ys)  
             = f (foldr f z xs) (foldr f z ys)

    {RHS of q((x:xs))}
  f (foldr f z (x:xs)) (foldr f z ys)
=   {applying foldr}
  f (f x (foldr f z xs)) (foldr f z ys)
=   {associativity of f}
  f x (f (foldr f z xs) (foldr f z ys))
=   {applying induction hypothesis}
  f x (foldr f z (xs ++ ys))
=   {unapplying foldr}
  foldr f z (x : (xs ++ ys))
=   {unapplying ++}
  foldr f z ((x:xs) ++ ys)
    {LHS of q((x:xs))}

□

___________________________________________________________