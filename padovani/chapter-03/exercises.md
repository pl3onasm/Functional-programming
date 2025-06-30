# Written Exercises for Chapter 3

## Exercise 3.1

Infer the type of the given expressions.

-----------------------------
### 3.1.1

```haskell
1 + 2 * 3
```

Type: `Num a => a`.

The expression contains numeric literals that can be of any numeric type. The operators `+` and `*` are defined for any type that is an instance of the `Num` typeclass, hence the type is polymorphic and can be instantiated with any numeric type that is an instance of `Num`, such as `Int`, `Integer`, `Float`, or `Double`.

-----------------------------
### 3.1.2

```haskell
1 + 2 ** 3
```

Type: `Floating a => a`.

The expression involves the exponentiation operator `**`, which is defined for types that are instances of the `Floating` typeclass, so the expression's type is constrained to `Floating a`.

-----------------------------
### 3.1.3

```haskell
1 + 2 ^ 3
```

Type: `Num a => a`.

The expression uses the exponentiation operator `^`, which has the type signature `(Integral b, Num a) => a -> b -> a`. The exponent must be of an `Integral` type. However, the final type of the expression is entirely determined by the base type `a`. Thus, the expression is polymorphic and can be of any numeric type that is an instance of `Num`.

-----------------------------
### 3.1.4

```haskell
1 + div 2 3
```

Type: `Integral a => a`.

The `div` function is defined for integral types, so the expression's type is constrained to `Integral a`.

-----------------------------
### 3.1.5

```haskell
1 + (/) 2 3
```

Type: `Fractional a => a`.

The division operator `( / )` is defined for types that are instances of the `Fractional` typeclass, so the expression's overall type is constrained to `Fractional a`, since addition requires both operands to be of the same type.

-----------------------------
### 3.1.6

```haskell
1 + sin pi
```

Type: `Floating a => a`.

The `sin` function is defined for types that are instances of the `Floating` typeclass, and `1` is a numeric literal that can be of any numeric type. This means that the expression's overall type is constrained to `Floating a`, since addition requires both operands to be of the same type.

-----------------------------
### 3.1.7

```haskell
pi / 2
```

Type: `Floating a => a`.

The division operator `( / )` is defined for types that are instances of the `Fractional` typeclass, and `pi` is a constant defined in the `Prelude` as a floating-point constant (as part of the subclass `Floating` of `Fractional`). Therefore, the expression's type is constrained to `Floating a`.

-----------------------------
### 3.1.8

```haskell
if pi > 3 then 1 else (pi / 2)
```

Type: `Floating a => a`.

The expression uses the `if` construct, which requires both branches to have the same type. The first branch is `1`, which can be of any numeric type, and the second branch is `pi / 2`, which is of type `Floating a`. Therefore, the overall type is constrained to `Floating a`.

-----------------------------
### 3.1.9

```haskell
if pi > 3 then 1 else (2:: Int)
```

Type: `Int`.

The first branch contains `1`, which can be of any numeric type, but the second branch is explicitly typed as `Int` by means of the annotation `(2 :: Int)`. Since both branches of the `if` expression must have the same type, the first branch can be coerced to `Int`, the overall type of the expression is `Int`. Note that this is a concrete type, not polymorphic, so the typing does not involve a type variable like `a`.

-----------------------------
### 3.1.10

```haskell
negate (if True then 1 else 2.0)
```

Type: `Fractional a => a`.

The `if` expression has two branches: `1`, which can be of any numeric type, and `2.0`, which is a literal of the `Fractional` typeclass. Since both branches must have the same type, Haskell will unify them to a common type, meaning that `1` is implicitly treated as `1.0` (i.e. it is coerced to the same `Fractional` type to match). The result of the conditional is thus op type `Fractional a => a`. The `negate` function is defined in the superclass `Num`, so it can be applied to a `Frational` type, and the overall type of the expression becomes `Fractional a => a`.

-----------------------------
## Exercise 3.2

The difference between the two operators `^` and `**` is that `^` is used for integral exponents, while `**` is used for fractional exponents. This is reflected in their type signatures:

- `^` has the type signature `(Integral b, Num a) => a -> b -> a`, meaning it takes a numeric type `a` for the base and an integral type `b` for the exponent and returns a numeric type `a`.

- `**` has the type signature `(Floating a) => a -> a -> a`, meaning it takes a floating-point type `a` for both the base and the exponent and returns a floating-point type `a`.

-----------------------------
## Exercise 3.3

Are the given expressions well-typed?

-----------------------------
### 3.3.1

```haskell
div 2.0 1.0
```

No, this expression is not well-typed. The `div` function is defined for `Integral` types, and `2.0` and `1.0` are floating-point literals of type `Fractional`. Therefore, applying `div` to these input values will result in a type error.

-----------------------------
### 3.3.2

```haskell
(/) pi 3
```

This expression is well-typed. The division operator `( / )` is defined for types that are instances of the `Fractional` typeclass, and `pi` is a floating-point constant of the `Floating` subclass. That means that the whole expression becomes of the type `Floating a => a`. The numeric literal `3` is implicitly coerced to the same type as `pi`, so that the expression is well-typed and has the type `Floating a => a`.

-----------------------------
### 3.3.3

```haskell
if (2 < 3) then pi + 1 else False
```

No, this expression is not well-typed. The `if` expression requires both branches to have the same type. The first branch `pi + 1` is of type `Floating a => a`, while the second branch `False` is of type `Bool`. Since these types are incompatible, the expression is not well-typed.

-----------------------------
### 3.3.4

```haskell
(/) 2 3 4
```

This expression is ill-typed. The division operator `( / )` is defined for types that are instances of the `Fractional` typeclass, and  takes exactly two arguments. However, in this case, it is applied to three arguments: `2`, `3`, and `4`.

In Haskell, functions are curried, so `( / )` 2 is a function of type `Fractional a => a -> a`, obtained by partially applying the division operator to `2`. This function can then be applied to `3`, yielding a value of type `Fractional a => a`.

However, the next application attempts to apply this value (which is a number, not a function) to `4`. This is invalid because a numeric value cannot be used as a function.

GHCi, however, will infer a type like: `(Fractional (t1 -> t2), Num t1) => t2`. This odd type arises because the compiler tries to interpret the numeric value `( / ) 2 3` as a function so that it can be applied to `4`. But there is no instance of `Fractional` for function types, so the type checker will reject this expression as ill-typed.

-----------------------------
### 3.3.5

```haskell
negate pi
```

This expression is well-typed. The `negate` function is defined for types that are instances of the generic `Num` typeclass, and `pi` is a floating-point constant (of type `Floating`). Since `Floating` is a subclass of `Num`, `pi` can be passed to negate without any problem.Therefore, the expression is well-typed and has the type `Floating a => a`, since it is still polymorphic over any `Floating` type.

-----------------------------
### 3.3.6

```haskell
abs (2 - 3)
```

This expression is well-typed. The subtraction operator `(-)` is defined for generic numeric types, and `2` and `3` are numeric literals that can be interpreted as any numeric type. So the expression `2 - 3` results in a numeric type, and the `abs` function is defined for types that are instances of the `Num` typeclass. Therefore, the overall expression is well-typed and has the type `Num a => a`.

-----------------------------
### 3.3.7

```haskell
abs 2 - 3
```

This expression is well-typed. Recall that in Haskell, function application has higher precedence than any other operator, so `abs 2` is evaluated first. The `abs` function is defined for types that are instances of the `Num` typeclass, and `2` is a numeric literal. Thus the result of `abs 2` is of the polymorphic type `Num a => a`. The subtraction operator `(-)` is also defined for numeric types, and can be applied to the result of `abs 2` and the numeric literal `3`. Therefore, the overall expression is well-typed and remains polymorphic, with the type `Num a => a`.

-----------------------------
### 3.3.8

```haskell
negate (-) 2 3
```

Since function application has higher precedence than any operator, this expression is parsed as `(negate (-)) 2 3`, which means that `negate` is applied to `(-)`, the subtraction operator. However, `negate` requires a numeric argument, and so the expression is ill-typed.

GHCi, however, will infer something like: `(Num t, Num (t -> t -> t)) => t`. This indicates the type checker is attempting to treat `(-)` as a numeric value (of type `t -> t -> t`), which is not valid because functions do not have `Num` instances. The expression is ill-typed and will be rejected by the compiler at compile time.

-----------------------------
### 3.3.9

```haskell
True :: Float
```

This expression is ill-typed. The literal `True` is of type `Bool`, and it cannot be coerced into a `Float`. Haskell's type system does not allow for such a conversion, so this expression will result in a type error.

-----------------------------
### 3.3.10

```haskell
(1 :: Float) + (2 :: Int)
```

This expression is ill-typed. The numeric literal `1` is explicitly typed as `Float`, and `2` is explicitly typed as `Int`. However, the addition operator `(+)` requires both operands to be of the same type. Since `Float` and `Int` are different types, and there is no automatic conversion between them, this expression will result in a type error.

-----------------------------
## Exercise 3.4

Add appropriate conversions so that the expressions are well-typed.

-----------------------------
### 3.4.1

```haskell
mod 2 pi
```

To make this expression well-typed, we need to convert `pi` to an integral type, since the `mod` function is defined for integral types. We can use the `round` or `truncate` functions to convert `pi` to an `Int`:

```haskell
mod 2 (round pi)
```

-----------------------------
### 3.4.2

```haskell
sin (2 :: Int)
```

To make this expression well-typed, we need to convert the `Int` to a `Floating` type, since the `sin` function is defined for floating-point types. We can use the `fromIntegral` function to convert the `Int` to a `Float`:

```haskell
sin (fromIntegral (2 :: Int))
```

-----------------------------
### 3.4.3

```haskell
(2.5 :: Float) + (mod 3 2)
```

To make this expression well-typed, we need to convert the result of `mod 3 2` to a `Float`, since `2.5` is a `Float`. We can use the `fromIntegral` function to convert the result of `mod`:

```haskell
(2.5 :: Float) + fromIntegral (mod 3 2)
```

-----------------------------
### 3.4.4

```haskell
2 ^ pi
```

To make this expression well-typed, we need to convert `pi` to an integral type, since the `^` operator is defined for numeric types where the exponent is an integral type. We can use the `round` or `truncate` functions to convert `pi` to an `Int`:

```haskell
2 ^ round pi
```
