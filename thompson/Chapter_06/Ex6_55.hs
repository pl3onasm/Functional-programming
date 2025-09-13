-----------------------------------------------------------
-- Exercise 6.55

-- | Players in a trick-taking game
data Player = North | East | South | West
  deriving (Show, Eq, Enum, Ord)

-----------------------------------------------------------

{-

This is a simple enumeration type representing the four
players in a trick-taking game. Each constructor (North,
East, South, West) is a distinct value of type Player. 
This makes that any variable of type Player can only 
take on one of these four values, ensuring type safety.

Deriving Show allows us to easily print the Player values,
while deriving Eq allows for equality comparisons between
Player values. Deriving Enum allows us to enumerate the
players, and Bounded provides the minimum (North) and
maximum (West) values of the Player type.

Testing in GHCi
ghci> :l Ex6_55
ghci> [North .. West]
[North,East,South,West]
ghci> minBound :: Player
North
ghci> North == South
False

-}