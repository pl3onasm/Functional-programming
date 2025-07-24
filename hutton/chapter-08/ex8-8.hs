-----------------------------------------------------------
-- Exercise 8.8

-- In this exercise, we extend the tautology checker to
-- support disjunction and equivalence.

data Prop = Const Bool
          | Var Char
          | Not Prop
          | And Prop Prop
          | Imply Prop Prop
          | Or Prop Prop
          | Equiv Prop Prop
  deriving (Show, Eq)

type Assoc k v = [(k,v)]

type Subst = Assoc Char Bool

-- finds the first value associated with a key
-- in an association list
find :: Eq k => k -> Assoc k v -> v
find k t = head [v | (k',v) <- t, k == k']

-- removes duplicates from a list
rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x : xs) = x : rmdups (filter (/= x) xs)

-- returns a list of all variables that occur
-- in a proposition
vars :: Prop -> [Char]
vars (Const _)    = []
vars (Var x)      = [x]
vars (Not p)      = vars p
vars (And p q)    = vars p ++ vars q
vars (Imply p q)  = vars p ++ vars q
vars (Or p q)     = vars p ++ vars q
vars (Equiv p q)  = vars p ++ vars q

-- evaluates a proposition under a substitution
-- where the variables are replaced by their values
-- and the constants are evaluated to their boolean values
eval :: Subst -> Prop -> Bool
eval _ (Const b)    = b
eval s (Var x)      = find x s
eval s (Not p)      = not (eval s p)
eval s (And p q)    = eval s p && eval s q
eval s (Imply p q)  = eval s p <= eval s q
eval s (Or p q)     = eval s p || eval s q
eval s (Equiv p q)  = eval s p == eval s q

-- generates all possible combinations of True and False
-- for a given number of boolean variables
-- where each combination is represented as a list of Bool
bools :: Int -> [[Bool]]
bools 0 = [[]]
bools n = map (False :) bss ++ map (True :) bss
  where bss = bools (n - 1)

-- generates all possible substitutions for a proposition
-- where each variable can be either True or False
substs :: Prop -> [Subst]
substs p = map (zip vs) (bools (length vs))
  where vs = rmdups (vars p)

-- checks if a proposition is a tautology
-- by evaluating it under all possible substitutions:
-- if it evaluates to True for all substitutions,
-- then it is a tautology
isTaut :: Prop -> Bool
isTaut p = and [eval s p | s <- substs p]

-----------------------------------------------------------

{-
  This program defines a tautology checker for  
  propositional logic formulas. The syntax is represented  
  using a data type Prop, which includes Boolean constants,  
  variables, negation, conjunction, implication, 
  disjunction, and equivalence.

  The key components are:

  - vars: extracts all variables from a formula  
  - bools: generates all combinations of Boolean values  
  - substs: builds all variable-to-value assignments  
  - eval: evaluates a formula under a given assignment  
  - isTaut: checks whether the formula is always true  
      under every possible assignment

  Examples:   

  ghci> :l ex8-8
  ghci> p1 = Imply (Var 'p') (Var 'q')
  ghci> p2 = Or (Not (Var 'p')) (Var 'q')
  ghci> isTaut (Equiv p1 p2)
  True
  ghci> p3 = And (Var 'p') (Not (Var 'p'))
  ghci> isTaut p3
  False
  ghci> p4 = Or (Var 'p') (Not (Var 'p'))
  ghci> isTaut p4
  True
  ghci> p5 = Not (And (Var 'p') (Var 'q'))
  ghci> p6 = Or (Not (Var 'p')) (Not (Var 'q'))
  ghci> isTaut (Equiv p5 p6)
  True

-}