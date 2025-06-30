# Written Exercises for Chapter 9

## Exercise 9.2

Determine which of the following expressions are well-typed, and if so, what their most general types are.

### 9.2.1

```haskell
[1, 2, 3.5]
```

Type: `(Fractional a) => [a]`

This expression is well-typed. It is a list containing different polymorphic numeric literals. This is allowed as long as they can be unified under a common type class constraint, which in this case is `Fractional`.

### 9.2.2

```haskell
[1, 2, round 3.5]
```

Type: `(Integral a) => [a]`

This expression is well-typed. The first two elements are numeric literals of type `Num`, and `round 3.5` returns an integral type, which can be unified with the first two elements under the `Integral` type class constraint.

### 9.2.3

```haskell
[(==) 1, id]
```

This is ill-typed. The first element is a partially applied equality function that expects an argument of the same type as `1`, which is `Num a => a`. The second element is the identity function `id`, which has the type `b -> b`. Since these two types cannot be unified, this expression is ill-typed.

### 9.2.4

```haskell
[(==) 1, (==) 2.5]
```

Type: `(Fractional a, Eq a) => a -> Bool`

This expression is well-typed. Both elements are partially applied equality functions. The first element is a partially applied equality function that expects an argument of the same type as `1`, which is `Num a => a`. The second element is a partially applied equality function that expects an argument of the same type as `2.5`, which is `Fractional a => a`. Since both numbers can be unified under the common type class `Fractional`, the function types can be unified as well, resulting in a type that accepts any `Fractional` type and returns a `Bool`. This means that the list does in fact contain elements of the same type and is well-typed.

### 9.2.5

```haskell
[(1, 1.5), (2, 1.5)]
```

Type: `[(Num a, Fractional b) => (a, b)]`

This expression is well-typed. It is a list of tuples, where the first element of each tuple is a numeric literal (which can be unified under `Num`), and the second element is a fractional literal (which can be unified under `Fractional`). Tuples are allowed to have different types for their elements, unlike lists. However, since both tuples have the same common type signature, the list is well-typed.

### 9.2.6

```haskell
[(+), (*)]
```

Type: `(Num a) => [a -> a -> a]`

This expression is well-typed. It is a list of functions that take two arguments of the same numeric type and return a value of the same type. The `+` and `*` operators are both binary functions that operate on numeric types, so they can be unified.

### 9.2.7

```haskell
[id, (+) 1; (/) 2]
```

This expression is ill-typed. The first element is the identity function `id`, which has the type `a -> a`. The second element is a partially applied addition function `(+) 1`, which has the type `b -> b` for some numeric type `b`. The third element is a partially applied division function `(/) 2`, which has the type `c -> c` for some fractional type `c`. Since these types cannot be unified, this expression is ill-typed.

### 9.2.8

```haskell
[fst, (+)]
```

This expression is ill-typed. The first element `fst` has the type `(a, b) -> a`, which is that of a function that takes a tuple and returns its first element. The second element `(+)` has the type `Num a => a -> a -> a`, which is that of a function that takes two numeric arguments and returns their sum. Since these two types cannot be unified, this expression is ill-typed.

### 9.2.9

```haskell
[$, id]
```

This expression is ill-typed. The first element `($)` is the function application operator with the type `((a -> b) -> a -> b)`, which takes a function and an argument and applies the function to the argument. The second element `id` has the type `c -> c`, which is the identity function. Since these two types cannot be unified, this expression is ill-typed.

## Exercise 9.6

What is the advantage in defining ++ as a right-associative operator?

The advantage of defining `++` as a right-associative operator is that it improves the efficiency of chained list concatenations by avoiding repeated traversal of intermediate results.

Right-associativity means that when multiple `++` operators are used in an expression, they are grouped from the right. For example:

```haskell
[1, 2] ++ [3, 4, 5] ++ [6, 7, 8, 9]
```

is interpreted as:

```haskell
[1, 2] ++ ([3, 4, 5] ++ [6, 7, 8, 9])
```

Had ++ been left-associative, it would instead be grouped as:

```haskell
([1, 2] ++ [3, 4, 5]) ++ [6, 7, 8, 9]
```

Although both groupings produce the same result, they differ in performance. This is because the implementation of ++ must traverse the entire first list before appending the second.

In our example, this means that in the right-associative version, the first concatenation requires traversing the second list only once, and the second concatenation then requires traversing the first list a single time to append the result of the first concatenation to the first list. This results in a total number of operations that is linear in the size of the final list.

However, in the left-associative version, the first concatenation requires traversing the first list to append the second list, and then the result of this concatenation is traversed again to append the third list. This results in the first list being traversed once during the first concatenation, and again as part of the longer list during the second, effectively traversing its elements twice, which is inefficient.

This inefficiency becomes more pronounced as the number of lists being concatenated increases. Chaining n lists in a left-associative manner can lead to a total time complexity of O(n²), since each new list is appended to a result that grows progressively larger. In contrast, right-associative grouping ensures that each list is only traversed once, resulting in a total linear time complexity.

Thus, defining ++ as right-associative ensures better performance for chained list concatenations and avoids unnecessary traversals.
