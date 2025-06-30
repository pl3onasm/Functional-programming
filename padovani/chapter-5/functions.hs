------------------------------------------------
-- Exercise 5.2

-- Returns the sign of a number
sign :: Int -> Int
sign n
  | n < 0     = -1
  | n > 0     = 1
  | otherwise = 0

-------------------------------------------------
-- Exercise 5.3

-- Converts int to bool
intToBool :: Int -> Bool
intToBool 0 = False
intToBool _ = True

-- Converts bool to int
boolToInt :: Bool -> Int
boolToInt False = 0
boolToInt _ = 1

-------------------------------------------------
-- Exercise 5.4

-- Rounds a number to the nearest integer
roundToInt :: RealFrac a => a -> Int
roundToInt n = truncate (n + 0.5 * signum n)

-------------------------------------------------
-- Exercise 5.5

-- Returns the fractional part of a number
fractionalPart :: RealFrac a => a -> a
fractionalPart n = n - fromIntegral (truncate n)

-------------------------------------------------
-- Exercise 5.6

-- Converts degrees to radians
degToRad :: Floating a => a -> a
degToRad degrees = degrees * pi / 180

-- Converts radians to degrees
radToDeg :: Floating a => a -> a
radToDeg radians = radians * 180 / pi

-- A few variants of trig functions taking 
-- degrees as input
sinDeg :: Floating a => a -> a
sinDeg degrees = sin (degToRad degrees)

cosDeg :: Floating a => a -> a
cosDeg degrees = cos (degToRad degrees)

tanDeg :: Floating a => a -> a
tanDeg degrees = tan (degToRad degrees)

-------------------------------------------------
-- Exercise 5.7

-- Determines if a year is a leap year
isLeap :: Int -> Bool
isLeap year = year `mod` 4 == 0 
  && (year `mod` 100 /= 0 || year `mod` 400 == 0)

-------------------------------------------------
-- Exercise 5.8

-- Implements f(a,b,x) = ax + b
f1 :: Num a => a -> a -> a -> a
f1 a b x = a * x + b

-- Implements f(a,b,c,x) = ax^2 + bx + c
f2 :: Num a => a -> a -> a -> a -> a
f2 a b c x = a * x^2 + b * x + c

-- Implements f(n,x) = sin^n(x) + cos^n(x)
f3 :: Floating a => Int -> a -> a
f3 n x = sin x ^ n + cos x ^ n

-- implements f(r,s) = ((π^2)(r+s)(r-s)^2) / 4
f4 :: Floating a => a -> a -> a
f4 r s = (pi^2 * (r + s) * (r - s)^2) / 4

-- Implements f(x,n) = x^(1/n)
f5 :: Floating a => a -> Int -> a
f5 x n = x ** (1 / fromIntegral n)

-------------------------------------------------
-- Exercise 5.9

-- Implements a function that computes the 
-- modulus of a complex number 
modulus :: Floating a => a -> a -> a
modulus re im = sqrt (re^2 + im^2)

-------------------------------------------------
