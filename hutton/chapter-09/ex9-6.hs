import Data.List (sortOn)

-----------------------------------------------------------
-- Exercise 9.6

-- arithmetic operations used in expressions
data Op = Add | Sub | Mul | Div | Exp

-- show instance for pretty printing operations
instance Show Op where
  show Add = "+"
  show Sub = "-"
  show Mul = "*"
  show Div = "/"
  show Exp = "^"

-- expressions can be either a value or an 
-- application of an operation
data Expr = Val Int | App Op Expr Expr

-- show instance for pretty printing expressions
instance Show Expr where
  show (Val n) = show n
  show (App o l r) = brak l ++ show o ++ brak r
    where
      brak (Val n) = show n
      brak e = "(" ++ show e ++ ")"

-- Result type is a pair of an expression and its value
type Result = (Expr,Int)

-- validity check for operations
valid :: Op -> Int -> Int -> Bool
valid Add x y = x <= y
valid Sub x y = x > y
valid Mul x y = x /= 1 && y /= 1 && x <= y
valid Div x y = y > 1 && x `mod` y == 0
valid Exp x y = x > 1 && y > 1 

-- performs the operation assuming it is valid
apply :: Op -> Int -> Int -> Int
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x `div` y
apply Exp x y = x ^ y

-- extracts all values from an expression
values :: Expr -> [Int]
values (Val n)     = [n]
values (App _ l r) = values l ++ values r

-- evaluates an expression to its result, if valid
-- returns an empty list if evaluation fails
eval :: Expr -> [Int]
eval (Val n)     = [n | n > 0]
eval (App o l r) = 
  [apply o x y | x <- eval l, y <- eval r, valid o x y]

-- returns all subsequences of a list (i.e. all
-- in ordered combinations of included elements)
subs :: [a] -> [[a]]
subs [] = [[]]
subs (x : xs) = yss ++ map (x :) yss
  where yss = subs xs

-- inserts an element into all possible positions in a list
interl :: a -> [a] -> [[a]]
interl x []       = [[x]]
interl x (y : ys) = (x : y : ys) : map (y :) (interl x ys)

-- generates all permutations of a list
-- i.e. all possible arrangements of its elements
perms :: [a] -> [[a]]
perms []       = [[]]
perms (x : xs) = concat (map (interl x) (perms xs))

-- returns all permutation of all subsequences 
-- of a list (i.e. all combinations of elements
-- in all possible orders)
choices :: [a] -> [[a]]
choices = concat . map perms . subs

-- splits a list into all non-empty pairs of left 
-- and right sublists
split :: [a] -> [([a],[a])]
split []  = []
split [_] = []
split (x : xs) = 
  ([x],xs) : [(x : ls,rs) | (ls,rs) <- split xs]

-- generates a list of all valid expressions paired
-- with their values from a list of natural numbers
results :: [Int] -> [Result]
results [] = []
results [n] = [(Val n,n) | n > 0]
results ns = [res | (ls,rs) <- split ns, lx <- results ls,
                    ry <- results rs, res <- combine lx ry]

-- combines two results with each arithmetic 
-- operation to create new results
combine :: Result -> Result -> [Result]
combine (l,x) (r,y) =
  [(App o l r, apply o x y) | o <- ops, valid o x y]

-- List of all arithmetic operations
-- used in expressions
ops :: [Op]
ops = [Add,Sub,Mul,Div,Exp]

-- Generates all valid expressions over a list of numbers
-- Returns a list of pairs (expression, value)
allResults :: [Int] -> [Result]
allResults ns =
  [(e,m) | ns' <- choices ns, (e,m) <- results ns']

-- Finds all valid expressions over a list of numbers
-- that evaluate exactly to a given target number
exactSols :: [Int] -> Int -> [Expr]
exactSols ns n = [e | (e, v) <- allResults ns, v == n]

-- Finds all expressions that evaluate to a target value
-- returning the best possible results, including exact
-- solutions if they exist
nearestSols :: [Int] -> Int -> [Expr]
nearestSols ns target = bestExprs
  where
    (minDiff, bestExprs) = 
      foldr select (maxBound, []) (allResults ns)

    select (e, v) (dmin, acc)
      | diff  < dmin = (diff, [e])
      | diff == dmin = (dmin, e : acc)
      | otherwise    = (dmin, acc)
      where diff = abs (v - target)

-- Finds all expressions that evaluate to a target value
-- returning the best possible results, sorted by
-- simplicity (number of operations)
simplestSols :: [Int] -> Int -> [Expr]
simplestSols ns target = 
  sortOn simplicity (nearestSols ns target)

-- Counts the number of operations in an expression
-- (i.e. the number of App constructors)
-- This is used to determine an expression's simplicity,
-- where fewer operations mean a simpler expression
simplicity :: Expr -> Int
simplicity (Val _)     = 0
simplicity (App _ l r) = 1 + simplicity l + simplicity r

-----------------------------------------------------------

{-
  a) In order to allow the use of exponentiation in 
  expressions, we need to exend the Op type to include 
  an Exp constructor, and modify the valid function to
  check that both the base and the exponent are greater
  than 1. This is because we work with natural numbers
  [1..] and raising a number to the power of 1 does not
  change its value, just like raising 1 to any power
  does not change the value of the base. So we can
  exploit an algebraic property here, just like we did
  for addition, multiplication, and subtraction.
  Lastly, we also need to modify the apply function to
  include the exponentiation operation.

  Let us take some solutions again:

    ghci> :l ex9-6.hs
    ghci> ns = [1,3,7,10,25,50]
    ghci> xs = take 100 (exactSols ns 765)
    ghci> exp = [x | x <- xs, '^' `elem` show x]
    ghci> exp
    [(50/10)*(25+((3-1)^7)),(50*(25+((3-1)^7)))/10]

  b) In order to find the nearest solutions if no exact
  solution is possible, we define a function nearestSols
  that takes a list of numbers and a target number.

  This function generates all valid expressions using the
  available numbers and evaluates them. Then, using a 
  single pass (foldr) over the list of results, it finds
  the expressions whose result is closest to the target.

  We compute the absolute difference between each result 
  and the target, keeping track of the smallest difference  
  seen so far, and collecting all expressions that match 
  it.

  This approach naturally includes exact matches, and since  
  an exact match has zero difference, it will be preferred  
  over any approximation. 

  The result is a list of one or more expressions that 
  produce a value as close as possible to the given target,
  using only the available numbers and operators. Changing
  the target number to 831 for our example list of numbers
  gives us the following results:

  ghci> take 5 (nearestSols ns 831)
  [7+((1+10)*(25+50)),10*(1+(7+(25+50))),
   10*(1+((7+25)+50)),10*((1+7)+(25+50)),
   10*((1+(7+25))+50)]

  These expressions differ by at most 1 from the target
  value of 831, which is the best we can do in this case.
  
  c) To find the simplest expressions, we define a
  function simplicity that counts the number of operations
  in an expression. This function is used to sort the
  results by their simplicity, so that the simplest
  expressions are returned first.

  ghci> take 10 (simplestSols ns 765)
  [(25-10)*(1+50),3*((7*(50-10))-25),
   (25-(3+7))*(1+50),((25-3)-7)*(1+50),
   ((25-7)-3)*(1+50),((3-1)+7)*(10+(25+50)),
   ((3-1)+7)*((10+25)+50),(3+(7-1))*(10+(25+50)),
   (3+(7-1))*((10+25)+50),((3+7)-1)*(10+(25+50))]

-}