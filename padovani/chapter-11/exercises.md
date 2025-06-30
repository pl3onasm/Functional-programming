# Written Exercises for Chapter 11

## Exercise 11.1

Infer the most general type for the following functions.

### 11.1.1

```haskell
f :: Eq a => a -> a -> a -> Bool
f x y z = x == y && y == z
```

This is the most general type for the function f. All three parameters `x`, `y`, and `z` must be of the same polymorphic type `a`, since they are compared for equality. That type `a` must be an instance of the `Eq` typeclass, so that the equality operator `(==)` is defined for it.

### 11.1.2

```haskell
f :: Eq a => a -> b -> a -> Bool
f x _ y = x == y
```

This function only compares the first and third parameters for equality, so the first and third parameters must be of the same type `a`, which must be an instance of the `Eq` typeclass. The second parameter is not used in the body of the function, so it can be of any type `b`. The function returns `True` if the first and third parameters are equal, and `False` otherwise.

### 11.1.3

```haskell
f :: Ord a => a -> a -> a -> a
f x y _ | x < y = x
f _ y z | y < z = y
```

This function is partially defined, meaning its guards do not cover all possible cases. The first pattern checks if `x` is less than `y`, and if so, returns `x`. The second pattern checks if `y` is less than `z`, and if so, returns `y`. However, if neither condition holds, the function falls through without a match, resulting in a runtime error due to non-exhaustive patterns.

All three arguments `x`, `y`, and `z` must be of the same type `a`, which must be an instance of the `Ord` typeclass, so that the less-than operator `(<)` can be used for comparison.

### 11.1.4

```haskell
f :: [a] -> [b] -> Int
f [] _ = 0
f _ [] = 0
f (_ : xs) (_ : ys) = 1 + f xs ys
```

The lists can be of any types `a` and `b`, since the actual elements are ignored (only the structure of the lists matters). The function returns the length of the shortest list, as it counts pairs until one of the lists runs out of elements. No type constraints are necessary for the elements of the lists, because the function does not perform any operations on the elements themselves.

### 11.1.5

```haskell
f :: Num a => [(a, a)] -> a
f [] = 0
f [(_, y)] = y
f ((x, _) : _) = x
```

This function takes a list of tuples, where each tuple contains two elements of the same type `a`. They must be of the same type, because, in the case of a non-empty list, the function returns either the first element of the list's head or the second element if the list has only one tuple. In the case of an empty list, `f` returns `0` as a default value. Therefore, the type `a` must be an instance of the `Num` typeclass.

### 11.1.6

```haskell
f :: [(a, b)] -> [a]
f [] = []
f (p : ps) = fst p : f ps
```

The second rule features the function `fst`, which extracts the first element of a tuple, implying that the elements of the input list must be tuples. No specific operations are performed on the components, so there are no typeclass constraints required and the types can be any. The function returns a list of the first elements of each tuple in the input list in the same order.

### 11.1.7

```haskell
f :: String -> Bool
f "" = True
f _ = False
```

This function checks if the input string is empty. The type of the input is `String`, which is an alias for `[Char]` in Haskell. The function returns `True` if the string is empty and `False` otherwise. No typeclass constraints are required, since the function performs only pattern matching and does not rely on any operations specific to the elements of the string.

### 11.1.8

```haskell
f :: [Char] -> Bool
f [] = True
f (c : _) | Data.Char.isSpace c = False
f (_ : cs) = f cs
```

The use of `isSpace` requires the input list to consist of `Char` elements. Therefore, the type is constrained to `[Char]`. The function takes a string and returns `False` as soon as it encounters a whitespace character, or `True` otherwise.

## Exercise 11.2

Determine which of the definitions are well-formed and well-typed. For those that are, say which are exhaustive en provide their most general type.

### 11.2.1

```haskell
f :: a -> b -> (a, b)
f x y = (x, y)
```

This function is well-formed and well-typed. It takes two parameters of arbibrary types `a` and `b`, respectively, and wraps them in a tuple. The function is exhaustive, meaning it handles all possible inputs of the appropriate types.

### 11.2.2

```haskell
f :: a -> b -> (a, a)
f x y = (x, x)
```

This function is also well-formed and well-typed. It takes two parameters of arbitrary types `a` and `b`. The second parameter `y` is ignored, and the function returns a tuple where both elements are the first parameter `x`. The function is exhaustive, as it can handle any inputs of types `a` and `b`.

### 11.2.3

```haskell
f [] = []
f x@(y : ys) = x : y
```

This function is not well-typed due to a type mismatch in the second equation. In the second clause, `x` is matched as a non-empty list (with head `y` and tail `ys`), but `y` is not a list, so the expression `x : y` is invalid. The left operand `x` is a list, while the right operand `y` is an element, leading to a type error, because the left operand to (:) must be an element, and the right operand must be a list, not the other way around.

### 11.2.4

```haskell
f :: [a] -> [a]
f [] = []
f x@(y : ys) = x ++ ys
```

This function is well-formed and well-typed. It takes a list containing elements of arbitrary type `a`. The first clause handles the empty list by returning `[]`. The second clause matches `x` with the entire input list, and matches `ys` with its tail, returning the `x ++ ys`, which is fine since both `x` and `ys` are lists of the same type `a`. The function is exhaustive, as it covers both cases of the input list (empty and non-empty).

### 11.2.5

```haskell
f :: Bool -> a -> a -> a
f False x _ = x
f True _ x = x
```

This function is well-formed and well-typed. It takes a `Bool` flag as its first argument and two additional parameters which must be of the same arbitrary type `a`, since the function may return either of these last two, based on the value of the `Bool`. The function is exhaustive, as it covers both possible values of the `Bool` (i.e., `True` and `False`).

### 11.2.6

```haskell
f (x, y) = x : y
f [x, y] = x : y
```

This function is not well-typed, because the two equations have incompatible input patterns. The first clause takes a tuple `(x, y)` and attempts to construct a list with `x` as the head and `y` as the tail. However, the second clause expects a list of two elements `[x, y]`, which is not compatible with the first clause's expected input type. The first clause expects a tuple, while the second expects a list, leading to a type error.

### 11.2.7

```haskell
f (x, y) = x : y
f (x : y) = x : y
```

This function is not well-typed due to the same reason as the previous one. The first clause expects a tuple `(x, y)`, while the second clause expects a list pattern `(x : y)`. This produces a type error.

### 11.2.8

```haskell
f ((_, _, x) : _) = x
f [_] = 0
```

This function is well-typed, since we can infer a meaningful type for the function:

```haskell
f :: Num a => [(b, c, a)] -> a
```

However, it is ill-formed because the two clauses overlap and are not exhaustive. The first clause matches a non-empty list. The second clause matches a list with exactly one element, but this case is already covered by the first clause, which can also match a singleton list. The patterns are also non-exhaustive, as they do not handle the case of an empty list, meaning the function will raise a runtime error if called with an empty list.
