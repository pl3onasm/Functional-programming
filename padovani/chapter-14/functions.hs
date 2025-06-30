-----------------------------------------------------------
-- Exercise 14.1

data Day = Monday | Tuesday  | Wednesday | Thursday |
           Friday | Saturday | Sunday
  deriving (Show, Eq, Ord, Enum)

-- Takes a day as input and returns the next day 
tomorrow :: Day -> Day
tomorrow x 
   | x == Monday = Tuesday
   | x == Tuesday = Wednesday
   | x == Wednesday = Thursday
   | x == Thursday = Friday
   | x == Friday = Saturday
   | x == Saturday = Sunday
   | otherwise = Monday

-----------------------------------------------------------
-- Exercise 14.2

-- Takes a day as input and returns the previous day
yesterday :: Day -> Day
yesterday x 
   | x == Monday = Sunday
   | x == Tuesday = Monday
   | x == Wednesday = Tuesday
   | x == Thursday = Wednesday
   | x == Friday = Thursday
   | x == Saturday = Friday
   | otherwise = Saturday

-- Another, more concise way, to implement tomorrow and 
-- yesterday is to derive the Day type from Enum, which
-- has built-in functions like succ and pred, but also
-- fromEnum and toEnum, which allow us to convert
-- between the Day type and an Int type, so we can use
-- arithmetic operations on the Int values.
tomorrow' :: Day -> Day
tomorrow' x = toEnum ((fromEnum x + 1) `mod` 7)

yesterday' :: Day -> Day
yesterday' x = toEnum ((fromEnum x - 1) `mod` 7)

-- Defined in point-free style (i.e. without explicitly
-- mentioning its arguments), these functions can be 
-- written as:
tmw :: Day -> Day
tmw = toEnum . (`mod` 7) . (+) 1 . fromEnum

ysd :: Day -> Day
ysd = toEnum . (`mod` 7) . subtract 1 . fromEnum

-----------------------------------------------------------
-- Exercise 14.3

-- Takes a day and an integer as input and returns the day
-- that is the given number of days after the input day.
after :: Day -> Int -> Day
after x n = toEnum ((fromEnum x + n) `mod` 7)

-- And we might as well define a function before:
before :: Day -> Int -> Day
before x n = toEnum ((fromEnum x - n) `mod` 7)

-----------------------------------------------------------
-- Exercise 14.4

data QuasiBool = No | Yes | Unknown
  deriving (Show, Eq, Ord)

-- Takes two QuasiBool values and returns a QuasiBool
-- that represents the logical AND operation.
qand :: QuasiBool -> QuasiBool -> QuasiBool
qand No _ = No
qand _ No = No
qand Yes Yes = Yes
qand _ _ = Unknown

{- If either argument is No, the result is No. If both
   arguments are Yes, the result is Yes. If one of the
   arguments is Unknown, the result is Unknown.
-}

-- Takes two QuasiBool values and returns a QuasiBool
-- that represents the logical OR operation.
qor :: QuasiBool -> QuasiBool -> QuasiBool
qor Yes _ = Yes
qor _ Yes = Yes
qor No No = No
qor _ _ = Unknown

{- If either argument is Yes, the result is Yes. If both
   arguments are No, the result is No. If one of the
   arguments is Unknown, the result is Unknown.
-}

-- Takes a QuasiBool value and returns its negation.
qnot :: QuasiBool -> QuasiBool
qnot No = Yes
qnot Yes = No
qnot Unknown = Unknown

-- Takes a list of QuasiBool values and computes the 
-- result of applying qand to all the values in the list.
qandList :: [QuasiBool] -> QuasiBool
qandList = foldr qand Yes

{- We fold the list using qand starting with the identity 
   element Yes because Yes is the neutral element for AND 
   (Yes && x = x).
-}

-- Takes a list of QuasiBool values and computes the
-- result of applying qor to all the values in the list.
qorList :: [QuasiBool] -> QuasiBool
qorList = foldr qor No

{- We fold the list using qor starting with the identity 
   element No because No is the neutral element for OR 
   (No || x = x).
-}

-- Takes a QuasiBool value and converts it to a Bool.
qtoBool :: QuasiBool -> Bool
qtoBool No = False
qtoBool Yes = True
qtoBool Unknown = error "Cannot convert Unknown to Bool"

-- Takes a Bool value and converts it to a QuasiBool.
btoQ :: Bool -> QuasiBool
btoQ True = Yes
btoQ False = No 