import Numeric (showFFloat)

-----------------------------------------------------------
-- Exercise 15.1

data Cx = Cx Double Double
  deriving (Eq)

-- Takes a Double and formats it to a string with two 
-- decimal places
format :: Double -> String
format x = showFFloat (Just 2) x ""

-- Defines its own instance of Show for Cx
-- to display complex numbers in the form "a + bi" 
instance Show Cx where
  show (Cx a b) = format a ++ " "  
                  ++ (if b >= 0 then "+ " else "") 
                  ++ format b ++ "i"

-- Takes two complex numbers and returns their sum
add :: Cx -> Cx -> Cx
add (Cx a b) (Cx c d) = Cx (a + c) (b + d)

-- Takes two complex numbers and returns their difference
sub :: Cx -> Cx -> Cx
sub (Cx a b) (Cx c d) = Cx (a - c) (b - d)  

-- Takes two complex numbers and returns their product
mul :: Cx -> Cx -> Cx
mul (Cx a b) (Cx c d) = Cx (a * c - b * d) (a * d + b * c)

-- Takes two complex numbers and returns their quotient
div' :: Cx -> Cx -> Cx
div' (Cx a b) (Cx c d) = 
  let denom = c * c + d * d
  in Cx ((a * c + b * d) / denom) ((b * c - a * d) / denom)

-- Takes a complex number and returns its modulus
modulus :: Cx -> Double
modulus (Cx a b) = sqrt (a * a + b * b)

-- Takes a complex number and returns its conjugate
conjugate :: Cx -> Cx
conjugate (Cx a b) = Cx a (-b)

-- Takes a complex number and returns its real part
re :: Cx -> Double
re (Cx a _) = a

-- Takes a complex number and returns its imaginary part
im :: Cx -> Double
im (Cx _ b) = b

-----------------------------------------------------------
-- Exercise 15.2

-- Takes a function and a list, and returns a new list
-- with the function applied to each element of the input
-- where the elements are of the maybe type.
-- If the function returns Nothing, the element is
-- not included in the output list
maybeMap :: (a -> Maybe b) -> [a] -> [b]
maybeMap _ [] = []
maybeMap f (x : xs) =
  case f x of
    Just y  -> y : maybeMap f xs
    Nothing -> maybeMap f xs

-----------------------------------------------------------
-- Exercise 15.3

-- Takes a list of Maybe values and returns a list
-- of the Just values, ignoring the Nothing values.
stripJust :: [Maybe a] -> [a]
stripJust [] = []
stripJust (Nothing : xs) = stripJust xs
stripJust (Just  x : xs) = x : stripJust xs

-- Non-recursive version of stripJust
stripJust' :: [Maybe a] -> [a]
stripJust' xs = [x | Just x <- xs]

-- Point-free version of stripJust
stripJust'' :: [Maybe a] -> [a]
stripJust'' = maybeMap id

-----------------------------------------------------------
-- Exercise 15.4

-- Defines maybeMap as a combination of map and stripJust
maybeMap' :: (a -> Maybe b) -> [a] -> [b]
maybeMap' f = stripJust . map f

{- First we apply the function f to each element
   of the list using map, which gives us a list of Maybe
   values. Then we use stripJust to remove the Nothing
   values and keep only the Just values.
-}

-----------------------------------------------------------
-- Exercise 15.5

-- Splits an input list of Either values into a pair of 
-- lists holding the parameters of Left and Right resp.
splitEither :: [Either a b] -> ([a], [b])
splitEither = foldr (\x (ys, zs) -> 
                      case x of
                        Left a  -> (a : ys, zs) 
                        Right b -> (ys, b : zs))
                    ([], [])

-- Alternatively, we can define it recursively, using
-- an accumulator
splitEither' :: [Either a b] -> ([a], [b])
splitEither' xs = split xs ([], [])
  where 
    split [] acc = acc
    split (Left  x : xs) (ys, zs) = split xs (x : ys, zs)
    split (Right x : xs) (ys, zs) = split xs (ys, x : zs) 

-----------------------------------------------------------
-- Exercise 15.6

data Number = I Int | F Float
              deriving (Eq, Ord)

-- Converts a number to a string
showNumber :: Number -> String
showNumber (I n) = show n
showNumber (F n) = show n

-- Converts a string to a number
readNumber :: String -> Number
readNumber s
  | '.' `elem` s = F (read s)
  | otherwise    = I (read s)

-- Better practice would be to use a Maybe number type:
readNum :: String -> Maybe Number
readNum s
  | '.' `elem` s = case reads s of
                     [(x, "")] -> Just (F x)
                     _         -> Nothing
  | otherwise    = case reads s of
                     [(x, "")] -> Just (I x)
                     _         -> Nothing

-----------------------------------------------------------
-- Exercise 15.7

-- Defines a type alias for Range
type Range = (Maybe Int, Maybe Int) 

-- Intersection of two ranges
intersect :: Range -> Range -> Range
intersect (l1, u1) (l2, u2) =
      let l = mx l1 l2
          u = mn u1 u2
      in if validRange l u then (l, u) else emptyRange
  where 
    mx Nothing y = y
    mx x Nothing = x
    mx (Just a) (Just b) = Just (max a b)
    mn Nothing _ = Nothing
    mn _ Nothing = Nothing
    mn (Just a) (Just b) = Just (min a b)

-- Union of two ranges
union :: Range -> Range -> Range
union (l1, u1) (l2, u2) = (mn l1 l2, mx u1 u2)
  where 
    mn Nothing y = y
    mn x Nothing = x
    mn (Just a) (Just b) = Just (min a b)
    mx Nothing _ = Nothing
    mx _ Nothing = Nothing
    mx (Just a) (Just b) = Just (max a b)

emptyRange :: Range
emptyRange = (Just 1, Just 0) 

validRange :: Maybe Int -> Maybe Int -> Bool
validRange (Just a) (Just b) = a <= b
validRange _ _ = True

{- Example usage in GHCi:

  r1 = (Just 1, Just 5)
  r2 = (Just 2, Nothing)

  intersect r1 r2
  Result: (Just 2, Just 5)
  
  union r1 r2
  Result: (Just 1, Nothing)
-}

{- The exercise asks to use a type alias for Range, but
   better practice would be to use a complex data type
   to represent the range, as shown below. This allows
   for more elegant handling of empty ranges and
   provides better type safety
-}

-- Complex data type for Range
data Ran = Ran (Maybe Int) (Maybe Int) | Empty
  deriving (Eq, Show)

-- Intersection
intersct :: Ran -> Ran -> Ran
intersct Empty _ = Empty
intersct _ Empty = Empty
intersct (Ran l1 u1) (Ran l2 u2) =
    let l = mx l1 l2
        u = mn u1 u2
    in if validBounds l u then Ran l u else Empty
  where 
    mx Nothing y = y
    mx x Nothing = x
    mx (Just a) (Just b) = Just (max a b)
    mn Nothing _ = Nothing
    mn _ Nothing = Nothing
    mn (Just a) (Just b) = Just (min a b)

-- Union
unite :: Ran -> Ran -> Ran
unite Empty r = r
unite r Empty = r
unite (Ran l1 u1) (Ran l2 u2) = Ran (mn l1 l2) (mx u1 u2)
  where 
    mn Nothing y = y
    mn x Nothing = x
    mn (Just a) (Just b) = Just (min a b)
    mx Nothing _ = Nothing
    mx _ Nothing = Nothing
    mx (Just a) (Just b) = Just (max a b)

-- Validity check for raw bounds (used in intersct)
validBounds :: Maybe Int -> Maybe Int -> Bool
validBounds (Just a) (Just b) = a <= b
validBounds _ _ = True

{-- Example usage:

  r1 = Ran (Just 1) (Just 5)
  r2 = Ran (Just 3) (Just 10)
  r3 = Ran (Just 7) (Just 8)

  intersct r1 r3
  Result: Empty
  
  unite r1 r2
  Result: Ran (Just 1) (Just 10)
  
  intersct r1 Empty
  Result: Empty

  unite r1 r3
  Result: Ran (Just 1) (Just 8)  
    -- Note that this is correct according 
    -- to the definition of union that was given
-}

-----------------------------------------------------------





