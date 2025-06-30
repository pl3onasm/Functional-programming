# Written Exercises for Chapter 5

## Exercise 5.1

Determine whether the function definitions are well-typed, and if so, what their types are.

### 5.1.1

```haskell
f x = sin (fromIntegral x)
```

This function definition is well-typed. Its type signature is `f :: (Integral a, Floating b) => a -> b`. The function takes an argument `x` of an integral type and returns a value of a floating type after converting `x` to a floating type using `fromIntegral`.

### 5.1.2

```haskell
f x = x ** 1
```

This function definition is well-typed. Its type signature is `f :: (Floating a) => a -> a`. The function takes an argument `x` of a floating type and returns the same type after raising `x` to the power of `1`, which is effectively `x`. 

### 5.1.3

```haskell
f b = if b then sin else False
```

This definition is ill-typed. The `if` expression expects both branches to return values of the same type. Here, `sin` is a function of type `(Floating a) => a -> a`, while `False` is of type `Bool`. Therefore, the types do not match, and this definition will result in a type error.

### 5.1.4

```haskell
f x = sin sin x
```

If we ask GHCi to infer the type of this function, it will come up with the signature `f :: (Floating a, Floating (a -> a)) => a`. This assumes that there is a Floating instance for function types such as `(a -> a)`, which is not the case in the standard Prelude. Therefore, though the function is well-typed in terms of syntax, it cannot be evaluated at runtime. In other words, the defintion is not meaningful, even though it typechecks.

### 5.1.5

```haskell
f x = (sin sin) x
```

This definition is equivalent to the previous one, since function application is left-associative in Haskell. The placement of parentheses does not change the meaning of the expression. So, we come to the same conclusion: the function is not meaningful, even though it typechecks.

### 5.1.6

```haskell
f x = sin (sin x)
```

This function definition is well-typed. Its type signature is `f :: (Floating a) => a -> a`. The function takes an argument `x` of a floating type, applies `sin` to it, and then applies `sin` again to the result. Both applications of `sin` are valid for floating types, and the function returns a value of the same type as its input.

### 5.1.7

```haskell
f x y = x + y + 1.5
```

The plus operator `(+)` requires both operands to be of the same type, which must be a numeric type. Since `1.5` is a Fractional number, the type signature of this function becomes `f :: (fractional a) => a -> a -> a`. The function takes two arguments `x` and `y`, both of a fractional type, and returns their sum plus `1.5`, which is also of the same fractional type.

### 5.1.8

```haskell
f x y z = if x then y else z
```

The function definition is well-typed. Its type signature is `f :: Bool -> a -> a -> a`. In other words, the first argument must be of type Bool, whereas the second and third arguments can be of any type, provided they are of the same type. 

### 5.1.9

```haskell
f x y = if x then y + 1 else True
```

This definition is ill-typed. The `if` expression expects both branches to return values of the same type. Here, `y + 1` is a numeric expression (assuming `y` is a number), while `True` is of type `Bool`. Therefore, the types do not match, and this definition will result in a type error.

### 5.1.10

```haskell
f x y u v = v + (if x < y then 0 else u)
```

This function definition is well-typed. Its type signature is `f :: (Ord a, Num b) => a -> a -> b -> b -> b`. The first two arguments `x` and `y` must be of an orderable type (so that we can compare them), while the last two arguments `u` and `v` must be of the same numeric type, since they are involved in an addition operation. The function returns a value of the same numeric type as `v` and `u`.
