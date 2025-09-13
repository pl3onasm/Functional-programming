module Chapter_06.Ex6_62 where

import Chapter_06.Ex6_53
import Chapter_06.Ex6_56
import Chapter_06.Ex6_57 (winNT)
import Chapter_06.Ex6_58 (winT)
import Test.HUnit

-----------------------------------------------------------
-- Exercise 6.62

-- | A team is a pair of players sitting 
-- opposite each other
data Team = NS | EW
  deriving (Eq, Show)

-- | Determines the team of a player
teamOf :: Player -> Team
teamOf North = NS
teamOf South = NS
teamOf East  = EW
teamOf West  = EW

-- | Determines the winning team of a game, given a 
-- function that decides the individual trick winner
gameWinner :: (Trick -> Player) -> [Trick] -> Team
gameWinner winnerFn tricks =
  if nsWins > ewWins then NS else EW
  where
    winners = [(teamOf . winnerFn) t | t <- tricks]
    nsWins  = length [t | t <- winners, t == NS]
    ewWins  = length [t | t <- winners, t == EW]

-- | Yields the winning team of a game with no trump suit
winnerNT :: [Trick] -> Team
winnerNT = gameWinner winNT

-- | Yields the winning team of a game with a trump suit
winnerT :: Suit -> [Trick] -> Team
winnerT trump = gameWinner (winT trump)

-- Example cards
c2H = (Two,   Hearts)
c5H = (Five,  Hearts)
c8H = (Eight, Hearts)
cKH = (King,  Hearts)
cAH = (Ace,   Hearts)
c2C = (Two,   Clubs)
cAC = (Ace,   Clubs)
cQC = (Queen, Clubs)
c7S = (Seven, Spades)
c9S = (Nine,  Spades)
cKS = (King,  Spades)
cAS = (Ace,   Spades)
c2D = (Two,   Diamonds)
c3D = (Three, Diamonds)
c4D = (Four,  Diamonds)
c9D = (Nine,  Diamonds)
cJD = (Jack,  Diamonds)
cQD = (Queen, Diamonds)
cKD = (King,  Diamonds)
cAD = (Ace,   Diamonds)

-- Example game
game :: [Trick]
game = [t1, t2, t3, t4, t5]
  where
  t1 = Trick East  [cAH, c2H, c5H, c3D]
  t2 = Trick East  [c9D, cKD, c2C, cJD]
  t3 = Trick South [c7S, c8H, cAD, c9S]
  t4 = Trick West  [c2D, cQD, c4D, cKH]
  t5 = Trick North [cAC, cAS, cKS, cQC]

-- | Tests for winnerNT and winnerT
testWinners :: Test
testWinners = TestList
  [TestCase (assertEqual "winnerNT game"
              NS (winnerNT game)),
   TestCase (assertEqual "winnerT Clubs game"
              NS (winnerT Clubs game)),
   TestCase (assertEqual "winnerT Spades game"
              EW (winnerT Spades game))
  ]


-----------------------------------------------------------

{-

Testing in GHCi

ghci> :set -i..
ghci> :set -package HUnit
ghci> :l Ex6_62
ghci> runTestTT testWinners
Cases: 3  Tried: 3  Errors: 0  Failures: 0
Counts {cases = 3, tried = 3, errors = 0, failures = 0}

-}