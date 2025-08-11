# Allowed Functions and Operators

The following standard arithmetic/Boolean operators and standard Haskell functions may be used throughout the exam.

```haskell
fst (a,b) = a

snd (a,b) = b

id x = x

[] ++ ys = ys
(x:xs) ++ ys = x : (xs++ys)

concat [] = []
concat (xs:xss) = xs ++ concat xss

map f [] = []
map f (x:xs) = f x : map f xs

filter p [] = []
filter p (x:xs)
  | p x = x:filter p xs
  | otherwise = filter p xs

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

(f . g) x = f (g x)

zip (x:xs) (y:ys) = (x,y) : zip xs ys
zip _ _ = []

zipWith f (x:xs) (y:ys) = f x y : zipWith f xs ys
zipWith _ _ _ = []

```
