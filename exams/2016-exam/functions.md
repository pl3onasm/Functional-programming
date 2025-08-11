# Allowed Functions and Operators

The following standard arithmetic/Boolean operators and standard Haskell functions may be used throughout the exam.

```haskell
[] ++ ys = ys
(x:xs) ++ ys = x : (xs++ys)

map f xs = [f x | x <- xs]

filter p xs = [x | x <- xs, p x]

foldr f z [] = z
foldr f z (x:xs) = f x (foldr f z xs)

sum [] = 0
sum (x:xs) = x + sum xs

reverse [] = []
reverse (x:xs) = reverse xs ++ [x]

head (x:xs) = x

tail (x:xs) = xs

length [] = 0
length (x:xs) = 1 + length xs

replicate n x = [x | i <- [1..n]]

f . g = \x -> f (g x)

zip (x:xs) (y:ys) = (x,y) : zip xs ys
zip xs ys = []

zipwith f xs ys = [f x y | (x,y) <- zip xs ys]

-- Lemma associativity of ++ (may be used without proof):
-- (xs ++ ys) ++ zs = xs ++ (ys ++ zs) = xs ++ ys ++ zs

-- Lemma concatenation with [] (may be used without proof):
-- xs ++ [] = xs

```
