# Allowed Functions and Operators

The following standard arithmetic/Boolean operators and standard Haskell functions may be used throughout the exam.

```haskell
[] ++ ys = ys
(x:xs) ++ ys = x : (xs++ys)

even x = x `mod` 2 == 0

odd x = x `mod` 2 == 1

head (x: _) = x

foldr f z [] = z
foldr f z (x:xs) = f x (foldr f z xs)

and xs = foldr (&&) True xs

or xs = foldr (||) False xs

sum xs = foldr (+) 0 xs

product xs = foldr (*) 1 xs

all p xs = and [ p x | x <- xs]

any p xs = or [ p x | x <- xs]

elem a xs = or [ a==x | x <- xs]

concat xss = [x | xs <- xss, x <- xs]

map f xs = [f x | x <- xs]

filter p xs = [x | x <- xs, p x]

reverse [] = []
reverse (x:xs) = reverse xs ++ [x]

take _ [] = []
take n (x:xs) = if n == 0 then [] else x:take (n-1) xs

drop _ [] = []
drop n (x:xs) = if n == 0 then (x:xs) else drop (n-1) xs

dropWhile p [] = []
dropWhile p (x:xs) = if (p x) then dropWhile p xs else (x:xs)

takeWhile p [] = []
takeWhile p (x:xs) = if (p x) then (x:takeWhile p xs) else []

length [] = 0
length (x:xs) = 1 + length xs

replicate 0 x = []
replicate n x = x:replicate (n-1) x

(f . g) x = f (g x)

zip (x:xs) (y:ys) = (x,y) : zip xs ys
zip _ _ = []

zipWith f (x:xs) (y:ys) = f x y : zipWith f xs ys
zipWith _ _ _ = []

```
