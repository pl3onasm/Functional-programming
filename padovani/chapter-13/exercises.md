# Written Exercises for Chapter 13

## Exercise 13.4

Determine which of the expressions are well-typed, and if they are, what their most general type is.

------------------------------

### 13.4.1

```haskell
(+) 1 2 3
```

This expression is not well-typed in any meaningful or executable sense. The operator (+) is a binary function of type: `Num a => a -> a -> a`. It expects two arguments, but here it is given three. The subexpression ((+) 1 2) evaluates to numeric value, and the result is then applied to 3. However, applying a number to an argument does not make sense.

Still, due to Haskell’s highly polymorphic type system, GHCi accepts the expression and infers a type: `(Num t1, Num (t1 -> t2)) => t2`, which is technically correct, but semantically meaningless. It relies on the invalid assumption that a function type like `(t1 -> t2)` could be an instance of `Num`, which is not the case. Therefore, the expression is ill-typed.

------------------------------

### 13.4.2

```haskell
(-) 1
```

This expression is well-typed. The binary operator `(-)` can be partially applied in Haskell, just like any other curried function. The expression `(-) 1` fixes the first argument to `1`, resulting in a function that takes one argument `x` and returns `1 - x`.The most general type of the expression is: `Num a => a -> a`.

------------------------------

### 13.4.3

```haskell
(**) (if True then sin else cos)
```

The expression is ill-typed. The operator (**) has the type `Floating a => a -> a -> a`. It expects two numeric arguments of type `a`. However, in the given expression, the first argument is a conditional that always evaluates to `sin`, which is a function, not a numeric value. As a result, the expression fails to type-check.

Note that, again, GHCi accepts the expression and infers a type: `(Floating a, Floating (a -> a)) => (a -> a) -> a -> a`. This is syntactically valid but semantically meaningless, since it relies on the invalid assumption that a function type such as `a -> a` could be an instance of `Floating`. Therefore, the expression should be considered ill-typed.

------------------------------

### 13.4.4

```haskell
(**) (if True then 1 else 2)
```

This expression is well-typed. The conditional expression `if True then 1 else 2` evaluates to `1`, which is a numeric value of type `Num a => a`. Applying (**) to a single numeric argument results in a partially applied function of type `Floating a => a -> a`. This function takes another numeric argument and raises `1` to the popwer of that argument. The most general type of the expression is thus `Floating a => a -> a`.

------------------------------

### 13.4.5

```haskell
(+) fromIntegral 1
```

This expression is ill-typed. The operator `(+)` has the type `Num a => a -> a -> a`, which means it expects two numeric arguments. However, `fromIntegral` is a function that converts an integral number to a more general numeric type, and does not match the expected type of the first argument of `(+)`. As a result, the expression fails to type-check.

Still, GHCi accepts the expression and infers the type: `(Integral a, Num b, Num (a -> b)) => a -> b`. This type relies on the invalid assumption that a function like fromIntegral could be an instance of `Num`, which is why the constraint `Num (a -> b)` was added. However, a function cannot be an instance of `Num`, so the expression is ill-typed.

------------------------------

### 13.4.6

```haskell
(+) (fromIntegral 1)
```

Thanks to the added brackets, the previous expression is now well-typed. The expression `fromIntegral 1` evaluates to the numeric value `1`, which has type `Num a => a`. The operator `(+)` is then be partially applied to this value, resulting in a function that takes one numeric argument and adds it to `1`. The most general type of the expression is: `Num a => a -> a`.

------------------------------

### 13.4.7

```haskell
(+) 2
```

This expression is well-typed. The expression `(+) 2` fixes the first argument of the binary operator `(+)` to `2`, resulting in a partially applied function that takes one numeric argument `x` and returns `2 + x`. The most general type of the expression is: `Num a => a -> a`.

------------------------------

### 13.4.8

```haskell
if True then (+) else (/) 1
```

The expression is ill-typed, because the conditional `if True then (+) else (/) 1` has two branches with *different* types. The first branch, `(+)`, has the type `Num a => a -> a -> a`, while the second branch, `(/) 1`, has the type `Fractional b => b -> b`. Since the two branches have incompatible types (the first is a binary function and the second is unary), the overall expression cannot be well-typed.

------------------------------

### 13.4.9

```haskell
(if True then (+) else (/)) 1
```

The added brackets make the previous expression well-typed. The conditional expression `if True then (+) else (/)` now evaluates to a function of consistent type, because both operators `(+)` and `(/)` are binary functions, whose types can be unified to the shared type `Fractional a => a -> a -> a`. Since `True` is the condition, the result is always the binary operator `(+)`, which is then partially applied to `1`. This yields a function that takes one numeric argument and adds `1` to it. The most general type of the expression is: `Fractional a => a -> a`.

------------------------------

### 13.4.10

```haskell
if (&&) True True then (++) else (\s -> \t -> s ++ ","++ t)
```

This expression is well-typed. The conditional `if (&&) True True` evaluates to `True`, so the first branch `(++)` is selected. The operator `(++)` has the type `[a] -> [a] -> [a]`. It takes two lists of the same type and returns their concatenation. The second branch, `\s -> \t -> s ++ "," ++ t`, performs a similar operation, but specifically on strings (lists of characters), and inserts a comma between them. As the types of both branches can be unified to `[Char] -> [Char] -> [Char]`, the conditional expression is well-typed. The most general type of the entire expression is: `[Char] -> [Char] -> [Char]`.

------------------------------

## Exercise 13.5

Let `curry` and `uncurry` be defined as follows:

```haskell
curry :: ((a, b) -> c) -> a -> b -> c
curry f = \x -> \y -> f (x, y)

uncurry :: (a -> b -> c) -> (a, b) -> c
uncurry f = \(x, y) -> f x y
```

Determine the most general type and the behavior of the given functions.

------------------------------

### 13.5.1

```haskell
curry (uncurry divides . \(x, y) -> (y, x))
```

The anonymous function `\(x, y) -> (y, x)` swaps the elements of an input tuple and has the type:

```haskell
\(x, y) -> (y, x) :: (a, b) -> (b, a)
```

However, since the anonymous function's output serves as the input for `uncurry divides` through composition, both `a` and `b` must be of type `Int`, as `divides` is defined to take two `Int` arguments.

From the notes, we know that `divides` is defined as:

```haskell
divides :: Int -> Int -> Bool
divides m n = m /= 0 && n `mod` m == 0
```

Therefore, `uncurry divides` has the type:

```haskell
uncurry divides :: (Int, Int) -> Bool
```

Finally, applying `curry` to the entire expresson yields a function that takes two `Int` arguments and returns a `Bool`, checking whether the second argument divides the first one.

Hence, the most general type of the entire expression is:

```haskell
curry (uncurry divides . \(x, y) -> (y, x)) :: Int -> Int -> Bool
```

------------------------------

### 13.5.2

```haskell
curry (uncurry (/) . \(x, y) -> (y, x))
```

The anonymous function `\(x, y) -> (y, x)` again swaps the elements of an input tuple. It is now composed with `uncurry (/)`, which takes a pair of `Fractional` numbers and returns their quotient. The type of `uncurry (/)` is:

```haskell
uncurry (/) :: (Fractional a) => (a, a) -> a
```

Since the output of the swap function is used as input for `uncurry (/)`, its type must be:

```haskell
\(x, y) -> (y, x) :: (Fractional a) => (a, a) -> (a, a)
```

Thus, the entire composition has the same type as `uncurry (/)`.

Applying `curry` to this composition, results in a function that takes two `Fractional` numbers `x` and `y` and returns `y / x`, that is, the quotient of the second argument divided by the first one.

The most general type of the entire expression is:

```haskell
curry (uncurry (/) . \(x, y) -> (y, x)) :: Fractional a => a -> a -> a
```

------------------------------

### 13.5.3

```haskell
curry fst
```

This expression applies `curry` to the function `fst`, which has the type:

```haskell
fst :: (a, b) -> a
```

Applying `curry` transforms a function on a pair into a curried function that takes two arguments separately. So `curry fst` becomes a function that takes two arguments and returns the first one.

The most general type of the expression is:

```haskell
curry fst :: a -> b -> a
```

------------------------------

### 13.5.4

```haskell
uncurry (*) . (\x -> (x, x))
```

The anonymous function `(\x -> (x, x))` takes a single argument and returns a tuple in which both components are equal. It has the type:

```haskell
(\x -> (x, x)) :: a -> (a, a)
```

This function is then composed with `uncurry (*)`, which has the type:

```haskell
uncurry (*) :: Num a => (a, a) -> a
```

It takes a pair of numbers and returns their product. Since the output of the anonymous function is passed as input to `uncurry (*)`, the type variable `a` in the typing of its definition must be an instance of the Num type class.

Thus, the entire expresson results in a function that takes a single numeric argument `x`, creates a tuple `(x, x)`, and then computes the product of the two elements in the tuple, which is the square of `x`.

The most general type of the entire expression is:

```haskell
uncurry (*) . (\x -> (x, x)) :: Num a => a -> a
```

------------------------------

### 13.5.5

```haskell
(==) 0 . uncurry mod . (\x -> (x, 2))
```

The anonymous function `(\x -> (x, 2))` takes a single argument and returns a tuple where the first element is `x` and the second element is the fixed value `2`. Its type is:

```haskell
(\x -> (x, 2)) :: a -> (a, Num)
```

The fucntion `uncurry mod` has the type:

```haskell
uncurry mod :: Integral a => (a, a) -> a
```

The composition of the two functions has the same type as `uncurry mod`, and takes a single argument `x`, creates a tuple `(x, 2)`, and returns the remainder of `x` divided by `2`.

Finally, this result is passed to the specialized equality operator `(==) 0`, which checks if the result is equal to `0`. In other words, the entire expression stands for a function that checks if the input number `x` is even.

The most general type of the entire expression is:

```haskell
(==) 0 . uncurry mod . (\x -> (x, 2)) :: Integral a => a -> Bool
```

------------------------------
