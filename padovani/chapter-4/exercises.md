# Written Exercises for Chapter 4

## Exercise 4.1

Which names can be used in definitions?

### 4.1.1: x

This is a valid name for a definition. It is a single lowercase letter, which is allowed in Haskell.

### 4.1.2: 3x

This is not a valid name for a definition. Names cannot start with a digit.

### 4.1.3: A

This is not a valid name for a definition. Names must start with a lowercase letter or an underscore, and 'A' is an uppercase letter.

### 4.1.4: x'y

This is a valid name for a definition. It contains lowercase letters and an apostrophe, which is allowed in Haskell.

### 4.1.5: x'

This is a valid name for a definition. It is a single lowercase letter followed by an apostrophe, which is allowed in Haskell.

### 4.1.6: 'x

This is not a valid name for a definition. Names cannot start with an apostrophe.

### 4.1.7: x3'

This is a valid name for a definition. It contains lowercase letters, a digit, and an apostrophe, which is allowed in Haskell.

### 4.1.8: xA

This is a valid name for a definition. It contains a lowercase letter followed by an uppercase letter, which is allowed in Haskell.

### 4.1.9: x_3

This is a valid name for a definition. It contains a lowercase letter, a digit, and an underscore, which is allowed in Haskell.

### 4.1.10: _x

This is a valid name for a definition. It starts with an underscore, which is allowed in Haskell, followed by a lowercase letter.

## Exercise 4.2

Determine the value of the expressions.

### 4.2.1

```haskell
let x = pi / 2 in sin x ** 2
```

The value of the expression is `1.0`. This is because `sin (pi / 2)` equals `1`, and `1 ** 2` is also `1`.

### 4.2.2

```haskell
let x = 1 in if x > 0 then x else (negate x)
```

The value of the expression is `1`. Since `x` is `1`, which is greater than `0`, so that the `if` condition evaluates to `True`, and the expression returns `x`, which is `1`.

### 4.2.3

```haskell
if True then let x = 2 in x ^ 4 else 0
```

The value of the expression is `16`. Since the condition is `True`, so that the `let` expression is evaluated, and `x` is bound to `2`, making the whole expression resolve to `2 ^ 4`, which is `16`.

### 4.2.4

```haskell
negate (let { x = 1; y = 2 } in x + y)
```

The value of the expression is `-3`. The `let` expression binds `x` to `1` and `y` to `2`, so `x + y` evaluates to `3`. The `negate` function then negates this value, resulting in `-3`.

### 4.2.5

```haskell
let x = 1 in let y = 2 in x + y
```

The value of the expression is `3`. The outer `let` binds `x` to `1`, and the inner `let` binds `y` to `2`. The expression evaluates to `x + y`, which is `1 + 2`, resulting in `3`.

### 4.2.6

```haskell
let x = 1 in let x = 2 in x
```

The value of the expression is `2`. In this case, the inner `let` shadows the outer `x`, so the inner `x` is used, which is bound to `2`. Therefore, the expression evaluates to `2`.

### 4.2.7

```haskell
let x = 1 in let y = 2 in let x = 0 in x + y
```

The value of the expression is `2`. The outer `let` binds `x` to `1`, but the inner `let` binds a new `x` to `0`, which shadows the outer `x`. The `y` is still bound to `2`, so the expression evaluates to `0 + 2`, resulting in `2`.

### 4.2.8

```haskell
if let x = True in x then let x = 1 in x else let x = 2 in x
```

The value of the expression is `1`. The `let` expression binds `x` to `True`, so the condition evaluates to `True`. Therefore, the first branch is taken, where `x` is bound to `1`, making the whole expression evaluate to `1`.

## Exercise 4.3

Write an expression that reduces to $2^{16}$ using only the literal `2`, no more than 4 occurrences of the operator `*`, and no other literal or operator or function.

This can be achieved by using exponentiation by repeated squaring:

```haskell
let x = 2 * 2 in 
let y = x * x in 
let z = y * y in 
  z * z
```
