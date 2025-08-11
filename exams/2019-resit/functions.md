# Allowed Functions and Operators

The following standard arithmetic/Boolean operators and standard Haskell functions may be used throughout the exam.

```haskell
[] ++ ys = ys
(x:xs) ++ ys = x : (xs++ys)

concat xss = [x | xs <- xss, x <- xs]

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

take 0 xs = []
take _ [] = []

take n (x:xs) = x:take (n-1) xs

drop 0 xs = xs
drop _ [] = []
drop n (x:xs) = drop (n-1) xs

length [] = 0
length (x:xs) = 1 + length xs

replicate n x = [x | i <- [1..n]]

(f . g) x = f (g x)

zip (x:xs) (y:ys) = (x,y) : zip xs ys
zip _ _ = []

zipWith f (x:xs) (y:ys) = f x y : zipWith f xs ys
zipWith _ _ _ = []

```
