-----------------------------------------------------------
-- Exercise 5.14

type Name = String
type Street = String

data House = HouseNumber Int
            | HouseName Name
            deriving (Show)

data Adress = Address Name Street House
              deriving (Show)

houseToString :: House -> String
houseToString (HouseNumber n) = show n
houseToString (HouseName s)   = s

adressToString :: Adress -> String
adressToString (Address name street house) =
  name ++ "\n" ++ street ++ "\n" ++ houseToString house


-----------------------------------------------------------

{-

Testing in GHCi:

ghci> :l Ex5_14
ghci> h1 = HouseNumber 42
ghci> h2 = HouseName "The White House"
ghci> a1 = Address "John Doe" "Main Street" h1
ghci> a2 = Address "Jane Smith" "Elm Street" h2
ghci> putStrLn (adressToString a1)
John Doe
Main Street
42
ghci> putStrLn (adressToString a2)
Jane Smith
Elm Street
The White House

-}