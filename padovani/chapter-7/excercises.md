# Written Exercises for Chapter 7

## Exercise 7.1

Infer the most general type of the given functions.

### 7.1.1

```haskell
f x y z = z
```

The most general type of the function `f` is:

```haskell
f :: a -> b -> c -> c
```

### 7.1.2

```haskell
f x y z = if x then y else z
```

The most general type of the function `f` is:

```haskell
f :: Bool -> a -> a -> a
```

### 7.1.3

```haskell
f x y z = if x == y then z else x
```

The most general type of the function `f` is:

```haskell
f :: Eq a => a -> a -> a -> a
```

Note that z must be of the same type as x because both branches of the `if` expression need to return a value of the same type. Also, x and y must be of the same type because they are compared using `==`, which requires both operands to be of the same type instance of the `Eq` typeclass.

### 7.1.4

```haskell
f x y u v = x == y || u == v
```

The most general type of the function `f` is:

```haskell
f :: (Eq a, Eq b) => a -> a -> b -> b -> Bool
```

Note that `x` and `y` must be of the same type, and `u` and `v` must also be of the same type, because both pairs are compared using the equality operator `(==)`. The function returns a `Bool`, indicating whether either pair is equal.

## Exercise 7.2

Find the most general type in each of the following cases.

### 7.2.1

```haskell
a -> a -> b, a -> b -> b, a -> b -> c
```

The third function has the most general signature, since it can take two different types for the first two arguments and a third unrelated type for the result.

### 7.2.2

```haskell
Int -> Int -> Bool, a -> b -> c, Int -> a -> b
```

The second function has the most general type, as it can take any types for its two arguments and return a third unrelated type.

### 7.2.3

```haskell
a -> a, a -> b, Int -> Int
```

The second function has the most general type, as it can take any type for its first argument and return a different type for its result.

### 7.2.4

```haskell
(Integral a) => a -> a -> a, Int -> Int -> Int, (Eq a) => a -> a -> a
```

The second function is the most specific, as it is constrained to the concrete type `Int`. The first function is more general because it can take any integral type, including `Int` and `Integer` (and others that are instances of the `Integral` typeclass), but the third function is the most general because it only requires the type to be an instance of the `Eq` typeclass, allowing for a wider range of types since `Eq` is a more general constraint than `Integral`. (All `Integral` types are also `Eq`, but not all `Eq` types are `Integral`.)

### 7.2.5

```haskell
Int, (Integral a) => a, (Ord a) => a
```

The third type is the most general, as it can take any type that is an instance of the `Ord` typeclass, which includes all integral types and many others. The second type is more specific than the third because it only allows integral types, while the first type is the most specific since it only allows the concrete type `Int`.

## Exercise 7.3

For each type in the previous exercise, define a function that has that type.

### 7.3.1

```haskell
f1 :: a -> b -> c
f1 _ _ = error "This function just ignores both its arguments"
```

### 7.3.2

Same as 7.3.1.

### 7.3.3

```haskell
f3 :: a -> b
f3 _ = error "This function ignores its first argument"
```

### 7.3.4

```haskell
-- Picks first argument if both are equal, otherwise
-- picks the second argument.
f4 :: (Eq a) => a -> a -> a
f4 x y = if x == y then x else y
```

### 7.3.5

It's not possible to define a function with the signature `(Ord a) => a` because it does not take any arguments from which a type could be inferred. There is no way to construct such a polymorphic value in general, as it would require a value of type `a` without any input to determine what `a` is. Haskell has no way to resolve what `a` should be.

It would work with a concrete type, like `Int`, but not with a polymorphic type. For example:

```haskell
answerToLife :: Int
answerToLife = 42
```
