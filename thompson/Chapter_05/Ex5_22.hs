-----------------------------------------------------------
-- Exercise 5.22

onSeparateLines :: [String] -> String
onSeparateLines ss = concat [s ++ "\n"  | s <- ss ]


-----------------------------------------------------------

{-

Testing in GHCi

ghci> :l Ex5_22
ghci> s = onSeparateLines ["hello", "world", "!"]
ghci> putStr s
hello
world
!


-}