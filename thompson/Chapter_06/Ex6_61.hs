module Chapter_06.Ex6_61 where

import Chapter_06.Ex6_53
import Chapter_06.Ex6_56
import Test.HUnit

-----------------------------------------------------------
-- Exercise 6.61

-- | A hand is a colllection of cards held by a player
type Hand = [Card]

-- | Hands held by all four players in canonical order
type Hands = [Hand]

-- | Checks whether the play in a trick is both possible  
-- and legal.
-- Possible: each card played is in the player's hand
-- Legal: players follow suit if they can
checkPlay :: Hands -> Trick -> Bool
checkPlay hands (Trick lead cards) =  
  and [valid hand card | (hand,card) <- zip hands cards]
  where
    leadSuit = snd (cards !! fromEnum lead)
    valid hand card@(v, s) =
      card `elem` hand && (s == leadSuit ||                        
      all ((/= leadSuit) . snd) hand)

-- | Example cards
c2H, c5H, cKH, cAH, c2C, cQC, cAC, c9S, cKS :: Card
c2H = (Two, Hearts)
c5H = (Five, Hearts)
cKH = (King, Hearts)
cAH = (Ace, Hearts)
c2C = (Two, Clubs)
cQC = (Queen, Clubs)
cAC = (Ace, Clubs)
c9S = (Nine, Spades)
cKS = (King, Spades)

-- | Example hands 
h1, h2, h3, h4 :: Hands
h1 = [[c2H, c5H, cQC], [cAH, c2C], [cAC, cKH], [c9S, cKS]]  
h2 = [[c2H, cAC], [cAH, c9S], [cQC, c5H], [cKS, cKH]]
h3 = [[cAC, c5H], [cQC], [c9S], [cKS, c2C]]                  
h4 = [[c2C, cAC], [c2H, c5H], [c9S, cKH], [cKS]]             

-- | Example tricks
t1, t2, t3, t4, t5, t6 :: Trick
t1 = Trick South [c2H, cKH, c5H, c9S]   
t2 = Trick West  [c2H, cAH, c5H, cKH]  
t3 = Trick North [cQC, c2C, cAC, c9S]   
t4 = Trick East  [cAC, cQC, c9S, cKS]  
t5 = Trick North [c2C, c5H, c9S, cKS]   
t6 = Trick South [cAC, c9S, cQC, cKH]   

-- | Tests for checkPlay
testCheckPlay :: Test
testCheckPlay = TestList
  [TestCase (assertEqual "Invalid: not all cards in hand"
              False (checkPlay h1 t1)),
   TestCase (assertEqual "Valid: all players follow suit"
              True (checkPlay h2 t2)),
   TestCase (assertEqual "Valid: west cannot follow"
              True (checkPlay h1 t3)),
   TestCase (assertEqual "Invalid: west should follow"
              False (checkPlay h3 t4)),
   TestCase (assertEqual "Valid: no one can follow"
              True (checkPlay h4 t5)),
   TestCase (assertEqual "Valid: two players cannot follow"
              True (checkPlay h2 t6))
  ]


-----------------------------------------------------------

{-

The checkPlay function takes a list of hands (in canonical
order) and a Trick (which consists of a leading player and
a list of played cards in canonical order). It zips the
list of hands with the list of played cards, and checks
that each played card is in the corresponding player's 
hand. It also checks that each player follows suit if they 
can, by comparing the suit of the played card with the suit 
of the lead card, and ensuring that if they do not match, 
the player has no cards of the lead suit in their hand.


Testing in GHCi

ghci> :set -i..
ghci> :set -package HUnit
ghci> :l Ex6_61
ghci> runTestTT testCheckPlay
Cases: 6  Tried: 6  Errors: 0  Failures: 0
Counts {cases = 6, tried = 6, errors = 0, failures = 0}

-}