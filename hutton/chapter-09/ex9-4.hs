-----------------------------------------------------------
-- Exercise 9.4

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
valid Sub x y = x > y
valid Mul _ _ = True
valid Div x y = x `mod` y == 0

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
subs (x:xs) = yss ++ map (x:) yss
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
-- of natural numbers. 
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

-- computes percentage
percent :: Int -> Int -> Double
percent x y = (100 * fromIntegral x) / fromIntegral y

-----------------------------------------------------------

{-
  Let us get some results first.

    ghci> :l ex9-4.hs
    ghci> ns = [1,3,7,10,25,50]
    ghci> take 5 (solutions ns 765)
    [3*((7*(50-10))-25),((7*(50-10))-25)*3,
     3*(((50-10)*7)-25),(((50-10)*7)-25)*3,(25-10)*(1+50)]

  To get the total number of possible expressions over a 
  list of natural numbers, use:

    ghci> possibleExprs ns
    33665406

  To count the number that actually evaluate:

    ghci> successfulExprs ns
    4672540

  To get the percentage of success:

    ghci> percent (successfulExprs ns) (possibleExprs ns)
    13.879351403039665

  Note: it is better to compile with GHC (using ghc -O2)
  for performance; GHCi will likely be too slow.

-}
