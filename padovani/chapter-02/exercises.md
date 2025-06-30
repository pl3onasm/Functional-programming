# Written Exercises for Chapter 2

## Exercise 2.1

Evaluate manually the given Haskell expressions.

### 2.1.1

```haskell
1 / 2 + 3
```

To evaluate the expression `1 / 2 + 3`, we first compute `1 / 2`, which results in `0.5`. Then we add `3` to `0.5`, yielding `3.5`.

### 2.1.2

```haskell
(1 + 2) + 3
```

The parentheses enforce that we first compute `1 + 2`, which results in `3`. Then we add `3` to `3`, yielding `6`.

### 2.1.3

```haskell
1 / (2 + 3)
```

The parentheses enforce that we first compute `2 + 3`, which results in `5`. Then we compute `1 / 5`, yielding `0.2`.

### 2.1.4

```haskell
sin pi ** 2
```

To evaluate the expression `sin pi ** 2`, we first compute `sin pi`, which equals `0`. Then we raise `0` to the power of `2`, resulting in `0`.

### 2.1.5

```haskell
False || False && True
```

The `&&` operator has higher precedence than `||`, so we first evaluate `False && True`, which results in `False`. Then we compute `False || False`, which results in `False`.

To see that the `&&` operator has indeed higher precedence than `||`, check this expression:

```haskell
True || False && False
```

This evaluates to `True`, because `False && False` is evaluated first, resulting in `True || False`, which is `True`.

### 2.1.6

```haskell
not False && False
```

To evaluate the expression `not False && False`, we first compute `not False`, which results in `True`. Then we evaluate `True && False`, which results in `False`. In Haskell, the `not` operator has higher precedence than `&&`, so it is applied first.

### 2.1.7

```haskell
not (if True then False else True)
```

The parentheses ensure that the `if` expression is evaluated first. Since the condition is `True`, the expression evaluates to its first branch, which is `False`. Then we apply `not` to `False`, resulting in `True`.

### 2.1.8

```haskell
if if False then True else False then False else True
```

The condition of the outer `if` is evaluated first in order to determine which branch to take. That condition is itself formulated as an `if` expression, which evaluates to `False`, since its own condition is `False` and its `else` branch evaluates to `False`. Therefore, condition of the outer `if` evaluates to `False`, and we take its `else` branch, which yields the final value `True`.

### 2.1.9

```haskell
if pi < pi / 2 then 1 else 2 + 1
```

The condition `pi < pi / 2` evaluates to `False`, since `pi` is greater than `pi / 2`. Therefore, we take the `else` branch, which evaluates to `2 + 1`, resulting in `3`.

### 2.1.10

```haskell
pi ** 2 < 3 || pi ** 2 >= 3
```

First, we compute `pi ** 2`, which is approximately `9.8696`. Then we evaluate the two comparisons: `9.8696 < 3` evaluates to `False`, and `9.8696 >= 3` evaluates to `True`. Since the `||` operator returns `True` if at least one of its operands is `True`, the overall expression evaluates to `True`.

## Exercise 2.2

Write Haskell expressions corresponding to the given mathematical formulas.

### 2.2.1

Expression: $\sqrt{2^2 + 3^2}$

```haskell
sqrt (2^2 + 3^2)
```

### 2.2.2

Expression: $\sin^2(\pi / 2)$

```haskell
sin (pi / 2) ** 2
```

### 2.2.3

Expression: $\mid -100 + 2^{2 + 3} \mid$

```haskell
abs (-100 + 2^(2 + 3))
```

### 2.2.4

Expression: $\sqrt{2 \pi 10} \; \lparen \frac{10}{e} \rparen ^{10}$

```haskell
sqrt (2 * pi * 10) * (10 / exp 1) ^ 10
```

### 2.2.5

Expression: $\frac{\frac{\pi}{2} + \frac{3}{4}}{\sqrt 2}$

```haskell
(pi / 2 + 3 / 4) / sqrt 2
```

## Exercise 2.3

Try to understand the error messages resulting from the evaluation of the given Haskell expressions.

### 2.3.1

```haskell
True == True == True
```

Error message:

*Precedence parsing error cannot mix `==` [infix 4] and `==` [infix 4] in the same infix expression*

Eplanation:

The `(==)` operator has fixity declared as infix level 4, left-associative. The parser disallows mixing the same operator twice without explicit parentheses, to avoid ambiguity.

So writing `True == True == True` without parentheses causes a parse error even before type checking. The parser cannot decide if this means `(True == True) == True` or `True == (True == True)`.

### 2.3.2

```haskell
4 `div` -2
```

Error message:

*Precedence parsing error cannot mix ‘div’ [infixl 7] and prefix '-' [infixl 6] in the same infix expression*

Explanation:

Without parentheses around `-2`, the parser cannot unambiguously deterime how to parse the expression: it could mean either ``4 `div` (-2)`` or ``(4  `div` -) 2``. Even if they had the same precedence, the parser would still reject mixing a prefix operator (-) immediately following an infix operator (div) in the same expression without parentheses.

### 2.3.3

```haskell
abs True
```

Error message:

*No instance for ‘Num Bool’ arising from a use of ‘abs’*

Explanation:

The `abs` function expects a numeric argument, but `True` is a boolean value, not a number. Haskell does not have a `Num` instance for `Bool`, so it cannot apply `abs` to `True`.

### 2.3.4

```haskell
if 3 then 1 else 2
```

Error message:

*Could not deduce ‘Num Bool’ arising from the literal ‘3’*

Explanation:

The `if` expression expects a boolean condition, but `3` is a numeric literal. So GHCi tries to interpret `3` as a boolean. But numeric literals like `3` use the Num typeclass, so GHC tries to find an instance of Num Bool (to treat 3 as a Bool). But there is no such instance, so it raises an error.

### 2.3.5

```haskell
if True then 1 else False
```

Error message:

*No instance for ‘Num Bool’ arising from the literal ‘1’*

Explanation:

The `if` expression expects both branches to have the same type. The `then` branch evaluates to `1`, which is a numeric value, whereas the `else` branch evaluates to `False`, which is a boolean value. Haskell tries to unify the types of both branches, but `Bool` is not an instance of the `Num` typeclass. The error message arises because GHC attempts to interpret Bool as a numeric type to match the then branch but fails.

### 2.3.6

```haskell
3 < True
```

Error message:

*No instance for ‘Num Bool’ arising from the literal ‘3’*

Explanation:

The comparison operator `<` has the type signature `Ord a => a -> a -> Bool`, meaning it expects two values of the same type `a` with an `Ord` constraint. To make the types match, GHC tries to unify `a` in both places. In order to do that, it tries to make `Bool` fit into the `Num` typeclass constraint imposed by the numeric literal `3`. So it looks for an instance of `Num Bool`, but there is no such instance, leading to the error. 

## Exercise 2.4

Add parentheses tot the given expressions so that they evaluate correctly.

### 2.4.1

Given expression: `sin sin 3`

This will not evaluate because the expression is parsed as `sin (sin) 3`, which is not valid since `sin` expects a numeric argument, not another function. To fix it, we can add parentheses to indicate that we want to apply `sin` to `3` first:

```haskell
sin (sin 3)
```

### 2.4.2

Given expression: `True && 2 < 3`

This will not evaluate because the expression is parsed as `(True && 2) < 3`, which is not valid since `&&` expects both operands to be of type `Bool`, whereas `2` is numeric. To fix it, we can add parentheses to ensure that the comparison is evaluated first, resulting in:

```haskell
True && (2 < 3)
```

### 2.4.3

Given expression: `div 8 div 4 2`

This will not evaluate, since `div` expects two numeric arguments, but here it is being applied to `8` and the function `div`, which is not valid. To fix it, we can add parentheses to ensure that `div 4 2` is evaluated first, resulting in:

```haskell
div 8 (div 4 2)
```

### 2.4.4

Given expression: `(+) negate 5 2`

This will not evaluate because the `(+)` operator expects two numeric arguments, but here its first argument `negate` is a function, not a numeric value. To fix it, we can add parentheses to ensure that `negate 5` is evaluated first, resulting in:

```haskell
(+) (negate 5) 2
```

### 2.4.5

Given expression: `mod (+) 1 (*) 3 4 div 16 4`

This will not evaluate because `mod` expects two numeric arguments, but here it is being applied to the function `(+)`, which is not valid. To fix it, we can add parentheses to ensure that the addition and division operations are evaluated first:

```haskell
mod ((+) 1 (* 3 4)) (div 16 4)
```
