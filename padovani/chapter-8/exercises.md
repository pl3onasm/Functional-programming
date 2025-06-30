# Written Exercises for Chapter 8

## Exercise 8.1

Determine the most general type of the given expressions.

### 8.1.1

```haskell
(1, 2, 3.5)
```

Type: `(Num a, Num b, Fractional c) => (a, b, c)`

This expression is a tuple containing three elements. The first two elements are numeric literals, each polymorphic over potentially different numeric types, so they are assigned separate type variables `a` and `b` constrained by `Num`. The third element is a fractional literal, which requires a different type variable `c`, since it it is still polymorphic and can still be instantiated with various `Fractional` types (e.g., `Double`, `Float`). Haskell does not assume that all numeric literals in a tuple must share the same type unless the context enforces it.

### 8.1.2

```haskell
((==), 1)
```

Type: `(Eq a, Num b) => (a -> a -> Bool, b)`

This tuple has the equality operator `(==)` as its first element, which is a function that takes two arguments of the same type `a` and returns a `Bool`. The second element is a numeric literal, which is polymorphic over any type `b` that is an instance of the `Num` type class. Since the two elements are independent, their type variables are not unified, allowing the first element to be of any type that supports equality, while the second element can be any numeric type.

### 8.1.3

```haskell
((==) 1, 1)
```

Type: `(Num a, Eq a, Num b) => (a -> Bool, b)`

Note that we must include both `Num` and `Eq` constraints for the first element of the tuple, even though `Num a` implies that `a` is an instance of `Eq`. Haskell's type inference does not automatically deduce that `a` is an instance of `Eq` from `Num a`. It requires explicit mention of both constraints if funcionality from both classes is used.

### 8.1.4

```haskell
((==) (1,1))
```

Type: `(Eq a, Num a, Eq b, Num b) => ((a, b) -> Bool)`

The tuple contains one element, which is a function that takes a tuple of two numbers, compares it with `(1, 1)`, and returns a `Bool`. The type constraints indicate that the elements of the tuple must be instances of both `Eq` and `Num`. We use different type variables `a` and `b` to represent the two elements of the input tuple, as they can be of different numeric types.

### 8.1.5

```haskell
((1, 1.5), (2, 1.5))
```

Type: `(Num a, Fractional b, Num c, Fractional d) => ((a, b), (c, d))`

Note that even though both tuples apparently have the same second element `1.5`, Haskell does not infer that they must be the same type. This is because each literal is treated as polymorphic independently, unless the context forces unification.

### 8.1.6

```haskell
((+), 1)
```

Type: `(Num a, Num b) => (a -> a -> a, b)`

This expression uses the addition operator `(+)`, which is a function that takes two arguments of the same numeric type `a` and returns a value of the same type. The second element of the tuple is an unrelated numeric literal that does not have to match the type of the first element, but it is still constrained by the `Num` type class.

### 8.1.7

```haskell
((+) 1, 1)
```

Type: `(Num a, Num b) => (a -> a, b)`

This expression is similar to the previous one, but here we partially apply the addition operator `(+)` to the numeric literal `1`. The first element of the tuple is now a function that takes one numeric argument of type `a` and returns `a + 1`. The second element is a numeric literal of a potentially different numeric type `b`.

### 8.1.8

```haskell
(id, (+) 1, (/) 2)
```

Type: `(Num b, Fractional c) => (a -> a, b -> b, c -> c)`

This expression is a tuple containing three elements. The first element is the identity function, which has the type `a -> a`, which returns its input unchanged. The second element is the partially applied addition operator `(+) 1`, which takes a numeric argument of type `b` and returns `b + 1`. The third element is the partially applied division operator `(/) 2`, which takes an argument of fractional type `c` and returns `c / 2`. As tuples can contain elements of different types, the type variables `a`, `b`, and `c` are independent of each other. The constraints `Num b` and `Fractional c` reflect the type class requirements for addition and division, respectively.

### 8.1.9

```haskell
(($), id)
```

Type: `((a -> b) -> a -> b, c -> c)`

The first element is the function application operator `($)`, which takes a function and an argument and applies the function to the argument (so `f $ x` is equivalent to `f(x)` in Haskell), yielding a result of the same type as the function's return type `b`. The type of `($)` is polymorphic, allowing it to work with any function type `a -> b`. The second element is the identity function `id`, which returns its input unchanged and has the polymorphic signature `c -> c`.

### 8.1.10

```haskell
((\x y -> (x, y)), (\x y -> (y, x)))
```

Type: `((a -> b -> (a, b)), (c -> d -> (d, c)))`

This expression is a tuple of two anonymous (lambda) functions. We use lambda expressions `(\x y -> ...)` here because named function definitions like fun x y = ... are not valid inside expressions such as tuples. Named functions must be declared separately at the top level. Both functions take two arguments and return a tuple containing those arguments, but the first function returns them in the order `(x, y)`, while the second function returns them in reverse order `(y, x)`. Though the two functions have similar structures, they are not required to have the same type, so we use different type variables for their inputs and outputs.

## Exercise 8.2

Determine the simplest pattern to bind the number `14` to the variable `x`.

### 8.2.1

```haskell
(x, _) = (14, 1)
```

This pattern matches a tuple where the first element is `14` and binds it to `x`, while the second element is ignored using the wildcard `_`.

### 8.2.2

```haskell
((_, (_, x)), _, _) = ((True, (1, 14)), False, 1.5)
```

This pattern matches a nested tuple structure. The first element is a tuple where the first element is `True` (ignored) and the second element is another tuple `(1, 14)`, from which we extract `14` and bind it to `x`. The other elements of the outer tuple are ignored.

### 8.2.3

```haskell
(_, (_, x)) = ((False, 1.5), (True, 14))
```

This pattern matches a tuple where the first element is `(False, 1.5)` (ignored) and the second element is `(True, 14)`, from which we extract `14` and bind it to `x`.

### 8.2.4

```haskell
(_, _, _, 14) = (False, 1.5, True, 14)
```

This pattern matches a tuple with four elements, where the first three elements are ignored using `_`, and the fourth element is `14`, which is bound to `x`.

### 8.2.5

```haskell
(_, x) = ((False, 1.5, True), 14)
```

This pattern matches a tuple where the first element is a three-element tuple `(False, 1.5, True)` (ignored) and the second element is `14`, which is bound to `x`.
