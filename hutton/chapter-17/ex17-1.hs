-----------------------------------------------------------
-- Exercise 17.1

-- ┌──────────────────────────────────────────────────────┐
-- │             Language and code extensions             │
-- └──────────────────────────────────────────────────────┘

-- | A stack of evaluated values
type Stack = [Int]

-- | Expressions in the extended language
data Expr
  = Val Int         -- ^ A constant integer value
  | Add Expr Expr   -- ^ Addition of two expressions
  | Throw           -- ^ Throw an exception
  | Catch Expr Expr -- ^ Catch exceptions: try the first e, 
                    --   if it fails evaluate the second e
  deriving Show

-- | Code for the virtual machine
data Code
  = HALT            -- ^ Stop execution
  | PUSH Int Code   -- ^ Push a value and 
                    --   continue with the next code
  | ADD Code        -- ^ Add the top two integers on 
                    --   the stack and continue
  | POP Code        -- ^ Pop and discard the top value 
                    --   of the stack, then continue
  deriving Show

-- ┌──────────────────────────────────────────────────────┐
-- │                Compiling expressions                 │
-- └──────────────────────────────────────────────────────┘

-- | Compiles an expression into code with default 
-- success and failure continuations
comp :: Expr -> Code
comp x = comp' x HALT HALT

-- | Compiles an expression with custom continuations
-- sc: code to execute if the expression succeeds
-- fc: code to execute if the expression fails
comp' :: Expr -> Code -> Code -> Code
comp' (Val n)     sc _  = PUSH n sc
comp' (Add x y)   sc fc = comp' x 
                          (comp' y (ADD sc) (POP fc)) fc
comp' Throw       _  fc = fc
comp' (Catch x h) sc fc = comp' x sc (comp' h sc fc)

-- ┌──────────────────────────────────────────────────────┐
-- │               Virtual machine execution              │
-- └──────────────────────────────────────────────────────┘

-- | Executes a program on a stack of values
exec :: Code -> Stack -> Stack
exec HALT s = s
exec (PUSH n c)  s          = exec c (n : s)
exec (ADD c)    (m : n : s) = exec c ((n + m) : s)
exec (POP c)    (_ : s)     = exec c s
exec _ _ = error "Invalid program or stack state"

-----------------------------------------------------------

{-
  Example usage:

  ghci> let expr1 = Add (Val 1) (Add (Val 2) (Val 3))
  ghci> let code = comp expr1
  ghci> code
  PUSH 1 (PUSH 2 (PUSH 3 (ADD (ADD HALT))))
  ghci> head (exec code [])
  6

  ghci> let expr2 = Catch (Add (Val 1) Throw) (Val 42)
  ghci> let code = comp expr2
  ghci> code
  PUSH 1 (POP (PUSH 42 HALT))
  ghci> head (exec expr2 [])
  42
  
-}