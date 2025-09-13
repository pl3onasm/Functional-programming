import Chapter_06.Ex6_53
import Chapter_06.Ex6_56
import Chapter_06.Ex6_61 (Hands, checkPlay)
import Data.List (sort)
import Test.HUnit

-----------------------------------------------------------
-- Exercise 6.63

-- | Checks whether the plays in a sequence of tricks are 
-- each both possible and legal by recursively checking
-- each trick, starting from the last trick played
checkPlays :: [Trick] -> Bool
checkPlays tricks = allLegal && allPossible
  where
  allPossible = sort (concat initHands) == sort fullDeck
  (allLegal, initHands) = check (reverse tricks) lastHands
  lastHands = [[], [], [], []] 

  -- recursively checks each trick in the list of tricks
  -- returning whether all plays were legal and the hands
  -- at the start of the game
  check :: [Trick] -> Hands -> (Bool, Hands)
  check [] hands = (True, hands)
  check (Trick lead cards : ts) hands =
    let prevHands = addCards hands cards
        currLegal = checkPlay prevHands (Trick lead cards)
        (remLegal, remHands) = check ts prevHands
    in (currLegal && remLegal, remHands)

  -- adds the played cards back to each player's hand 
  addCards :: Hands -> [Card] -> Hands
  addCards [] [] = []
  addCards (h : hs) (c : cs) = (c : h) : addCards hs cs

-- All playing cards for tests
c2H = (Two, Hearts); c3H = (Three, Hearts)
c4H = (Four, Hearts); c5H = (Five, Hearts)
c6H = (Six, Hearts); c7H = (Seven, Hearts)
c8H = (Eight, Hearts); c9H = (Nine, Hearts)
cTH = (Ten, Hearts); cJH = (Jack, Hearts)
cQH = (Queen, Hearts); cKH = (King, Hearts)
cAH = (Ace, Hearts)
c2C = (Two, Clubs); c3C = (Three, Clubs)
c4C = (Four, Clubs); c5C = (Five, Clubs)
c6C = (Six, Clubs); c7C = (Seven, Clubs)
c8C = (Eight, Clubs); c9C = (Nine, Clubs)
cTC = (Ten, Clubs); cJC = (Jack, Clubs)
cQC = (Queen, Clubs); cKC = (King, Clubs)
cAC = (Ace, Clubs)
c2S = (Two, Spades); c3S = (Three, Spades)
c4S = (Four, Spades); c5S = (Five, Spades)
c6S = (Six, Spades); c7S = (Seven, Spades)
c8S = (Eight, Spades); c9S = (Nine, Spades)
cTS = (Ten, Spades); cJS = (Jack, Spades)
cQS = (Queen, Spades); cKS = (King, Spades)
cAS = (Ace, Spades)
c2D = (Two, Diamonds); c3D = (Three, Diamonds)
c4D = (Four, Diamonds); c5D = (Five, Diamonds)
c6D = (Six, Diamonds); c7D = (Seven, Diamonds)
c8D = (Eight, Diamonds); c9D = (Nine, Diamonds)
cTD = (Ten, Diamonds); cJD = (Jack, Diamonds)
cQD = (Queen, Diamonds); cKD = (King, Diamonds)
cAD = (Ace, Diamonds)

-- Example tricks
t1  = Trick North [cAH, c4H, c9H, c6H]
t2  = Trick East  [c5D, cAD, c7D, c2D]
t3  = Trick South [c4C, c9C, cAC, c5C]
t4  = Trick West  [c6S, c2S, c4S, cAS]
t5  = Trick North [cKD, c3D, cJD, c4D]
t6  = Trick East  [c7C, cKC, c2C, cTC]
t7  = Trick South [c2H, c5H, cKH, c8H]
t8  = Trick West  [c9S, c8S, c5S, cKS]
t9  = Trick North [cQD, c6D, c6C, c9D]
t10 = Trick East  [cTS, cQS, cJS, c3S]
t11 = Trick South [c3H, c7H, cQH, cJH]
t12 = Trick West  [cJC, cTD, c3C, cQC]
t13 = Trick North [c8D, cTH, c8C, c7S]
t14 = Trick East  [c5D, cAD, c6D, c2D]
t15 = Trick South [c2H, c8S, cKH, c8H]
t16 = Trick West  [c9S, c5H, c5S, cKS]
t17 = Trick North [c4C, c6D, c6C, c9D]

-- | Tests for checkPlays
testCheckPlays :: Test
testCheckPlays = TestList
  [TestCase (assertEqual "Valid: all plays legal and\
      \ possible" True (checkPlays game1)),
   TestCase (assertEqual "Not possible: South does not\
      \ have 6D in trick 2" False (checkPlays game2)),
   TestCase (assertEqual "Not legal: East should follow\ 
      \suit in tricks 7 & 8" False (checkPlays game3)),
   TestCase (assertEqual "Not possible: North replays 4C\
      \in trick 9" False (checkPlays game4))
  ]
  where
    game1 = [t1, t2, t3, t4, t5, t6, t7, t8, 
             t9, t10, t11, t12, t13]
    game2 = [t1, t14, t3, t4, t5, t6, t7, t8, 
             t9, t10, t11, t12, t13]
    game3 = [t1, t2, t3, t4, t5, t6, t15, t16, 
             t9, t10, t11, t12, t13]
    game4 = [t1, t2, t3, t4, t5, t6, t7, t8,
             t17, t10, t11, t12, t13]
    

-----------------------------------------------------------

{-

The function checkPlays checks whether the plays in a 
sequence of tricks (a full game) are both possible and  
legal. 

We define:
- Possible: each card played could have come from a 
  valid hand (no duplicates, no missing cards).
- Legal: players follow suit when able.

The tricks are processed in reverse order, starting from
the last trick played, to reconstruct each player's hand 
at the start of the game. For each trick, the function: 
- adds the cards from the current trick back to 
  the hands reconstructed from later tricks
- checks (using checkPlay) whether the play in the 
  current trick is legal with respect to these hands. 
  
Once all tricks have been processed, the legality of the
game is the conjunction of the legality of each trick.
At this point the initial hands have also been fully
reconstructed, and can be used to check the possibility
condition: the game is possible if the sorted concatenation
of the initial hands equals the sorted full deck of cards.
This is a sound check, because if any player plays a card 
not in their hand at any point in the game, the 
reconstructed initial hands will be incomplete, and the
possibility check will fail.

Testing in GHCi

ghci> :set -i..
ghci> :set -package HUnit 
ghci> :l Ex6_63
ghci> runTestTT testCheckPlays
Cases: 4  Tried: 4  Errors: 0  Failures: 0
Counts {cases = 4, tried = 4, errors = 0, failures = 0}



-}