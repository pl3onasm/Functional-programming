-----------------------------------------------------------
-- Exercise 9.5

-- arithmetic operations used in expressions
data Op = Add | Sub | Mul | Div

-- show instance for pretty printing operations
instance Show Op where
  show Add = "+"
  show Sub = "-"
  show Mul = "*"
  show Div = "/"

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

-- validity check for operations:
-- subtraction must not go negative,
-- division must not divide by zero
valid :: Op -> Int -> Int -> Bool
valid Add _ _ = True
valid Sub _ _ = True  -- changed to allow negative results
valid Mul _ _ = True
valid Div x y = y /= 0 && x `mod` y == 0

-- performs the operation assuming it is valid
apply :: Op -> Int -> Int -> Int
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x `div` y

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
-- combinations of included elements)
subs :: [a] -> [[a]]
subs [] = [[]]
subs (x : xs) = yss ++ map (x :) yss
  where yss = subs xs

-- inserts an element into all possible positions in a list
interl :: a -> [a] -> [[a]]
interl x []       = [[x]]
interl x (y : ys) = (x : y : ys) : map (y :) (interl x ys)

-- returns all permutations of a list
-- (i.e. all possible arrangements of elements)
perms :: [a] -> [[a]]
perms []       = [[]]
perms (x : xs) = concat (map (interl x) (perms xs))

-- returns all permutations of all subsequences
-- of a list (i.e. all possible combinations
-- of elements in all orders)
choices :: [a] -> [[a]]
choices = concat . map perms . subs

-- splits a list into all non-empty pairs of left 
-- and right sublists
split :: [a] -> [([a],[a])]
split []  = []
split [_] = []
split (x : xs) = 
  ([x],xs) : [(x : ls,rs) | (ls,rs) <- split xs]

-- generates all valid expressions using a given list
-- of natural numbers
exprs :: [Int] -> [Expr]
exprs []  = []
exprs [n] = [Val n]
exprs ns  = [e | (ls,rs) <- split ns, l <- exprs ls,
                r <- exprs rs, e <- combine l r]

-- combines two expressions with each arithmetic 
-- operation to create new expressions
combine :: Expr -> Expr -> [Expr]
combine l r = [App o l r | o <- ops]

-- list of all arithmetic operations used in expressions
ops :: [Op]
ops = [Add,Sub,Mul,Div]

-- finds all expressions that:
-- 1. use numbers from the input list
-- 2. evaluate to the target number
solutions :: [Int] -> Int -> [Expr]
solutions ns n =
  [e | ns' <- choices ns, e <- exprs ns', eval e == [n]]

-- counts all possible expressions (regardless of success)
possibleExprs :: [Int] -> Int
possibleExprs ns = length
  [e | ns' <- choices ns, e <- exprs ns']

-- counts only the expressions that evaluate successfully
successfulExprs :: [Int] -> Int
successfulExprs ns = length
  [e | ns' <- choices ns, e <- exprs ns', eval e /= []]

-- calculates percentage given successful and total counts
percent :: Int -> Int -> Double
percent _ 0 = 0   -- avoid division by zero
percent x y = (100 * fromIntegral x) / fromIntegral y

-----------------------------------------------------------

{-
  If we generalize the numeric domain from natural numbers
  to integers, we can allow negative (intermediate) results
  in expressions. This means that we can use subtraction
  without restrictions, and we can also use division by
  negative numbers. To achieve this, we only need to
  change the valid function 'valid' to allow negative 
  results. This increases the number of successful
  expressions from 4672540 to 10839369.

  ghci> :l ex9-5.hs
  ghci> ns = [1,3,7,10,25,50]
  ghci> successfulExprs ns
  10839369

  It is a nice example of how a small semantic change in 
  constraints can massively broaden a solution space.

-}