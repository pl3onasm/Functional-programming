-----------------------------------------------------------
             EXAM FUNCTIONAL PROGRAMMING 2025               
-----------------------------------------------------------

You may use the standard arithmetic/Boolean operators and 
the standard Haskell functions given in the file 
functions.md throughout the entire exam.

___________________________________________________________

1. Types 
___________________________________________________________

Question 1.1:
Is the following expression a valid Haskell expression?
If YES, then give the most general type of the expression.

([[]],[])

--------
Answer: 

Tuples do not require that the types of their components
are of the same type, so this is a valid expression.
The first component is a list containing the empty list.
Since the empty list has type [a] for any type a, the
first component has type [[a]] for some type a.
The second component is the empty list, which has type [b]
for some type b.
Thus, the most general type is: ([[a]], [b]) 


--------------------------------
Question 1.2:
Is the following expression a valid Haskell expression?
If YES, then give the most general type of the expression.

[[[]],[]]

--------
Answer:

Yes, this is a valid expression. The first element of the 
outer list is a list containing the emptpy list, so that 
the first element has type [[a]] for some type a.
The second element is the empty list, which has type [b]
for some type b. Since these two elements are in a list,
the type checking system will try to unify the two types,
and so it will choose b = [a].
Hence, the most general type of the outer list becomes: 
[[[a]]]


--------------------------------
Question 1.3:
Is the following expression a valid Haskell expression?
If YES, then give the most general type of the expression.

[[[]],[True]]

--------
Answer: 

No, this is not a valid expression. The first element of
the outer list is a list containing the empty list, which
has type [[a]] for some type a.
The second element is a list containing a single Boolean
value, so it has type [Bool].
These two types cannot be unified, since the variable a
is wrapped inside a list. So the type checking system
cannot find a common type for the two elements of the
outer list, and fails.


--------------------------------
Question 1.4:
Is the following expression a valid Haskell expression?
If YES, then give the most general type of the expression.

not.(&&)

--------
Answer: 

This is an invalid expression. The composition operator (.)
expects two unary functions as its arguments, matching its
signature
  (.) :: (b -> c) -> (a -> b) -> a -> c

In this case, the outer function is not :: Bool -> Bool,
and the inner function is (&&) :: Bool -> Bool -> Bool, 
which is equivalent to Bool -> (Bool -> Bool).

For composition to work, the output type of the inner 
function must match the input type of the outer one. 
In this case, that would require: Bool ≡ Bool -> Bool
This fails. The outer function's input type is rigid
(Bool), and cannot unify with Bool -> Bool.

If the operator not were hypothetically polymorphic, say 
a -> Bool, then the type variable a could unify with 
Bool -> Bool, and the composition would work.

Another way to make the expression valid is to partially
apply the operator (&&) to one of its arguments, so that
we obtain a unary function as an overall result of the 
composition. For example:

  not . (&& True) :: Bool -> Bool

Here, (&&) True :: Bool -> Bool is a unary function,
producing a Bool that the operator not can accept.


--------------------------------
Question 1.5:
Is the following expression a valid Haskell expression?
If YES, then give the most general type of the expression.

(&&).not

--------
Answer: 

Yes, this expression is valid. By swapping the order of
the two functions, we can now compose them successfully.
The inner function is now not :: Bool -> Bool, and the
outer function is (&&) :: Bool -> (Bool -> Bool).
While (&&) is a binary function, it can be partially
applied to produce a unary function. In the composition, 
it will be partially applied to whatever comes from the 
inner function.

For composition to work, the output type of the inner
function must match the input type of the outer one.
In this case, the inner function's output type is Bool,
which matches the input type of the outer function (&&).
Thus, the composition is valid, and the resulting function
has the type:

  (&&) . not :: Bool -> Bool -> Bool

That is, it takes a Bool, passes it through not, uses the 
result as the first argument to (&&), and returns a new 
function Bool -> Bool that waits for its second argument.


___________________________________________________________

2. Programming in Haskell
___________________________________________________________

As you know, merge sort is a divide-and-conquer sorting 
algorithm that sorts a list by splitting it into two 
halves, recursively sort each half, and then merge the 
sorted halves into a sorted list.

Give a Haskell implementation of the function 
mergeSort :: Ord a => [a] -> [a].

The call mergeSort [4,2,5,1,2] should return the sorted 
list [1,2,2,4,5]. You are not allowed to use the indexing 
operator (!!). You are allowed to imlement helper 
functions, but not your own implementation of the indexing
operator.

--------
Answer:

> -- takes two sorted lists and merges them  
> -- into a single sorted list (ascending order)
> merge :: Ord a => [a] -> [a] -> [a]
> merge [] ys = ys
> merge xs [] = xs
> merge (x : xs) (y : ys)
>   | x <= y    = x : merge xs (y : ys)
>   | otherwise = y : merge (x : xs) ys

> -- takes a list and sorts it in ascending order
> -- using a divide-and-conquer strategy
> mergeSort :: Ord a => [a] -> [a]
> mergeSort []  = []
> mergeSort [x] = [x]
> mergeSort xs  = merge (mergeSort left) (mergeSort right)
>   where
>     (left, right) = let n = length xs `div` 2
>                     in (take n xs, drop n xs)

The sort is in O(n log n) time complexity, where n is
the length of the input list. So the way we split the list
into two halves, is fine (as it does not affect the overall
complexity), even though it is not really the most 
efficient way to do it, as it traverses the list twice: 
once for length, and once for splitting.

A more efficient way to split the list in a single pass,
is to use two pointers which traverse the list at different 
speeds, so that when the faster pointer reaches the end,
the slower pointer is at the middle of the list. It would
look like this:

> halve :: [a] -> ([a], [a])
> halve zs = split zs zs []
>   where
>   split (x : xs) (_ : _ : ys) acc = split xs ys (x : acc)
>   split xs _ acc = (reverse acc, xs)


___________________________________________________________

3. List comprehensions
___________________________________________________________

Question 3.1:
The function pairDiff takes a list xs of integers, and 
returns the list of pairwise differences x-y of each pair
(x, y) where x appears before y in the list xs. 
The implementation must be a list comprehension (and is not
allowed to use recursion).

For example:
  
  pairDiff [6,5,4,3,2,1] 
  should return: [1,2,3,4,5,1,2,3,4,1,2,3,1,2,1]

--------
Answer:

> pairDiff :: [Int] -> [Int]
> pairDiff xs = [x - y | (x, i) <- zip xs [1..], 
>                         y <- drop i xs]

The comprehension pairs each element x of xs with its 
position i (counting from 1). For each x, it takes the 
sublist drop i xs, which skips x and all earlier elements,
ensuring that each y comes strictly after x in the 
original list. It then computes x - y for all such y. 
The result is a list of all pairwise differences x - y for 
each pair (x, y) where x appears before y in the original 
list xs. 


--------------------------------
Question 3.2:
Give an implementation of the function sublists xs 
(including its type) that returns the list of all 
contiguous sublists of xs. The implementation should 
make use of a list comprehension.

For example: 

  sublists [1,2,3] 
  should yield: [[1],[1,2],[1,2,3],[2],[2,3],[3]]


--------
Answer:

> sublists :: [a] -> [[a]]
> sublists xs = [take n (drop m xs) | m <- [0..len - 1],
>                 n <- [1..len - m]]
>   where len = length xs

This uses two zero-based indices:
- m is the starting position of the sublist.
- n is the length of the sublist

The take n (drop m xs) expression extracts the sublist
starting at position m with length n from the original list
xs. This way, we generate all contiguous sublists of xs.      


--------------------------------
Question 3.3:
Give a definition of the function 
zigzag :: [a] -> [a] -> [a] that produces a list that is an
alternation of the elements from two input lists. This 
alternation stops when the shortest input list is 
exhausted. The implementation of zigzag must be a list 
comprehension (without recursion).

For example: 
  
  zigzag [1..5] [10..20] 
  must produce: [1,10,2,11,3,12,4,13,5,14]

--------
Answer:

> zigzag :: [a] -> [a] -> [a]
> zigzag xs ys = concat [x : [y] | (x, y) <- zip xs ys]

This function uses the zip function to pair elements from
the two input lists xs and ys. For each pair (x, y), it
creates a list containing x and y. The resulting list of
lists is then flattened using concat. 

A slightly more concise way to achieve the same result
is to extract each element from the pairs directly:

> zigzag' xs ys = [el | (x, y) <- zip xs ys, el <- [x, y]]


___________________________________________________________

4. Higher-order functions
___________________________________________________________

Question 4.1:
Use foldr to implement the function glue, which acts the 
same as the ++ operator. So glue xs ys must return xs++ys.

--------
Answer:

> glue :: [a] -> [a] -> [a]
> glue xs ys = foldr (:) ys xs

This function uses ys as the initial accumulator and folds
over the list xs, prepending each element to the growing
accumulator. The result is the concatenation of xs and ys.


--------------------------------
Question 4.2:
Use foldr to implement the function mapf. The function mapf
must act the same as the standard map function, so 
mapf f xs must return the same result as map f xs.

--------
Answer:

> mapf :: (a -> b) -> [a] -> [b]
> mapf f xs = foldr (\x acc -> f x : acc) [] xs

The lambda function takes an element x from the list xs
and applies the function f to it, then prepends the result
to the accumulator acc. The initial accumulator for foldr 
is an empty list, so the final result is a new list where 
each element of xs has been transformed by the function f.


--------------------------------
Question 4.3:
Give the implementation and the type of the function doif
which takes a predicate, a function, and a list. Its output 
is the list that is obtained by applying the function to 
all elements of the list that satisfy the predicate, while 
all other elements remain unchanged. 

For example:
  doif (<=2) (*10) [1,2,3,4,5,1] = [10,20,3,4,5,10]

--------
Answer:

> doif :: (a -> Bool) -> (a -> a) -> [a] -> [a]
> doif p f = 
>   foldr (\x acc -> (if p x then f x else x) : acc) []

Using foldr, we check each element x of the list against
the predicate p. If p x is True, we apply the function f
to x, otherwise we keep x unchanged. The result is
accumulated in the list, starting from an empty list. 

It is also possible to leave out the accumulator from the 
lambda function because in foldr, a lambda function of the 
form \x -> ... automatically produces a function that 
prepends to the accumulator. So we can write:

> doif' p f = foldr (\x -> if p x then f x else x) []


___________________________________________________________

5. Inﬁnite lists and lazy evaluation
___________________________________________________________

Question 5.1:
Given is the infinite list of prime numbers, defined as 
follows:

> primes :: [Integer] 
> primes = sieve [2..]
>   where
>   sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p /= 0]
  
Give the definition of the function isComposite n which 
returns True if and only if n is a composite 
(i.e. non-prime) number. You are not allowed to use the 
boolean operator not in your solution.

--------
Answer:

> isCompos :: Integer -> Bool
> isCompos n =  n > 1 && n /= head (dropWhile (< n) primes)

This function traverses the list of primes until it finds 
the first prime number greater than or equal to n.
If n > 1 and is different from that prime, then n is 
composite, and the function returns True. Otherwise n is 
either prime or not greater than 1, and the function 
returns False.


--------------------------------
Question 5.2:
The numbers 

  t(n) = ∑_{i=1}^n i = n(n+1)/2

(for positive integers n) are called triangle numbers. 
The expression Σ_{i=1}^n i is the sum of the first n
positive integers which is equal to n(n+1)/2. So, the 
n-th triangle number equals the sum of the first n 
positive integers.

Implement the ordered infinite list trinums of triangle 
numbers. 

So, take 8 trinums should return [1,3,6,10,15,21,28,36].

The implementation must have the form 

  trinums = ?? : [x + y | (x,y) <- ??]. 
  
You are required to substitute proper expressions for the
question marks.

--------
Answer:

> trinums :: [Integer]
> trinums = 1 : [x + y | (x, y) <- zip trinums [2..]] 

The first triangle number is 1, and the rest of the 
triangle numbers are generated by adding the next
integer to the previous triangle number. 
The list [2..] provides the sequence of integers to add, 
and zip trinums [2..] pairs each triangle number with the 
next integer. The comprehension then adds these pairs to 
produce the infinite ordered list. 
This creates the sequence of triangle numbers as follows:

  T₁ = 1
  T₂ = 1 + 2 = 3
  T₃ = 3 + 3 = 6
  T₄ = 6 + 4 = 10
  ...


--------------------------------
Question 5.3:
Consider the Cantor snake from the figure included in this
exam folder (file: CantorSnake.png).
We represent the rational number a b as the pair (a, b). 
Give a Haskell definition of the inifinite list cantor that 
produces all pairs in the order that is given in the 
figure. Example:

  take 10 cantor = [(1,1),(1,2),(2,1),(3,1),(2,2),
                    (1,3),(1,4),(2,3),(3,2),(4,1)]

--------
Answer:

> cantor :: [(Integer, Integer)]
> cantor = [if even s then (s - p, p) else (p, s - p) 
>           | s <- [2..], p <- [1..s-1]]

The key observation to capture the pattern in the Cantor
snake is that the sum s of the numerator and denominator 
over each diagonal is constant, starting at 2, and 
increasing by 1 for each subsequent diagonal. 
Following the line of the snake in the figure, we see that
the numerator and denominator follow an alternating 
pattern along each diagonal based on whether the sum s is 
even or odd. If s is even, the numerator decreases while 
the denominator increases, and if s is odd, the numerator 
increases while the denominator decreases. The range for 
this pattern is from 1 to s-1, i.e. the diagonal length. 

The cantor list generates all pairs (a, b) in this snake
pattern, by iterating over the sums s and the positions p 
within each diagonal. Based on the parity of s, either 
the numerator or the denominator is derived from the
position p, while the other is derived from the sum s.  


___________________________________________________________

6. ADT module
___________________________________________________________

The abstract data type (ADT) KVstore ktp vtp implements a 
simple key-value store, where ktp is the data type of the 
keys, and vtp is the data type of the values. 

Implement a module that implements the ADT hiding its 
internal structure from the user.

The following operations on the ADT must be implemented:
• empty :: Eq ktp => KVstore ktp vtp
  This creates an empty key-value store.
• insert :: Eq ktp => ktp -> vtp -> KVstore ktp vtp 
            -> KVstore ktp vtp 
  This inserts a key-value pair into the store (or over-
  writes an already existing pair with the given key).
• find :: Eq ktp => ktp -> KVstore ktp vtp -> Maybe vtp
  This looks up a value by its key.
• delete :: Eq ktp => ktp -> KVstore ktp vtp 
            -> KVstore ktp vtp
  This removes a key-value pair from the store.
• size :: Eq ktp => KVstore ktp vtp -> Int 
  This returns the number of key-value pairs in the store.

--------

Answer:

To turn the below code into a module, you would create
a file named KVstore.hs and start it with the following
module declaration:

module KVstore (
  KVstore, empty, insert, 
  find, delete, size
) where

This line exports the abstract data type KVS and the 
functions, but does not export the constructor FO, thus
hiding the concrete implementation details.

> data KVstore ktp vtp = KVS [(ktp, vtp)]

> empty :: Eq ktp => KVstore ktp vtp
> empty = KVS []

> insert :: Eq ktp => ktp -> vtp -> KVstore ktp vtp
>          -> KVstore ktp vtp
> insert k v (KVS kvs) = 
>   KVS ((k, v) :  filter (\(k', _) -> k' /= k) kvs)

> find :: Eq ktp => ktp -> KVstore ktp vtp
>          -> Maybe vtp
> find k (KVS kvs) = check kvs
>   where
>   check [] = Nothing
>   check ((k', v) : kvs)
>     | k == k'   = Just v
>     | otherwise = check kvs

> delete :: Eq ktp => ktp -> KVstore ktp vtp
>             -> KVstore ktp vtp
> delete k (KVS kvs) = 
>   KVS (filter (\(k', _) -> k' /= k) kvs)

> size :: Eq ktp => KVstore ktp vtp -> Int
> size (KVS kvs) = length kvs


___________________________________________________________

7. Proof on lists
___________________________________________________________

The function add is defined as follows:

  add :: Integer -> [Integer] -> [Integer]
  add a [] = []
  add a (x : xs) = (a + x) : add a xs

Prove the following property:

  p(xs) : add a (add b xs) = add (a+b) xs 
  
  for all finite lists xs and for all values of a and b

--------
Answer:

We will prove property p by structural induction on the 
list xs. 

---------------------------------
Base case: prove p([])
---------------------------------

    {LHS of p([])}
  add a (add b [])
=   {applying add}
  add a []
=   {applying add}
  []
=   {unapplying add}
  add (a+b) []
    {RHS of p([])}

---------------------------------
Inductive step: prove p((x : xs))
---------------------------------

    Induction hypothesis:
      p(xs) : add a (add b xs) = add (a+b) xs

    {LHS of p((x : xs))}
  add a (add b (x : xs))
=   {applying add}
  add a ((b+x) : add b xs)
=   {applying add}
  (a+b+x) : add a (add b xs)
=   {induction hypothesis}
  (a+b+x) : add (a+b) xs
=   {associativity of +}
  ((a+b)+x) : add (a+b) xs
=   {unapplying add}
  add (a+b) (x : xs)
    {RHS of p((x : xs))}

□


___________________________________________________________

8. Proof of foldr property
___________________________________________________________

Prove for all finite lists xs::[a] and ys::[a], any 
value z::a, and any function f :: a -> a -> a that:

  p(xs): foldr f z (xs ++ ys) = foldr f (foldr f z ys) xs

[Note: find the definitions of foldr and ++ in the file
       functions.md included in this exam folder]

--------
Answer:

We will prove this property p by structural induction on 
the list xs. 

---------------------------------
Base case: prove ([])
---------------------------------

    {RHS of p([])}
  foldr f (foldr f z ys) []
=   {applying foldr}
  foldr f z ys
=   {unapplying ++}
  foldr f z ([] ++ ys)

---------------------------------
Inductive step: prove p((x : xs))
---------------------------------

    Induction hypothesis:
      p(xs): foldr f z (xs ++ ys) 
      = foldr f (foldr f z ys) xs

    {RHS of p((x : xs))}
  foldr f (foldr f z ys) (x : xs)
=   {applying foldr}
  f x (foldr f (foldr f z ys) xs)
=   {induction hypothesis}
  f x (foldr f z (xs ++ ys))
=   {unapplying foldr}
  foldr f z (x : xs ++ ys)
=   {unapplying ++}
  foldr f z ((x : xs) ++ ys)
    {LHS of p((x : xs))}

□

___________________________________________________________