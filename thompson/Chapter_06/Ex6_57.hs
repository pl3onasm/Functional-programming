module Chapter_06.Ex6_57 where

import Chapter_06.Ex6_53
import Chapter_06.Ex6_56
import Test.HUnit

-----------------------------------------------------------
-- Exercise 6.57

-- | Decides the winner of a trick, assuming no trump suit
-- The winner is the player who played the highest card
-- of the suit led
winNT :: Trick -> Player
winNT (Trick lead cards) = snd (maximum ledCards)
  where
    leadSuit = snd (cards !! fromEnum lead)
    plrs  = [North .. West]
    ledCards = [(v, pl) | (pl, (v, s)) <- zip plrs cards, 
                          s == leadSuit]

-- | Example cards
c2H, c5H, cKH, cJH, cAC, cQC, c9C, cKC :: Card
c2H = (Two, Hearts)
c5H = (Five, Hearts)
cKH = (King, Hearts)
cJH = (Jack, Hearts)
cAC = (Ace, Clubs)
cQC = (Queen, Clubs)
c9C = (Nine, Clubs)
cKC = (King, Clubs)
cAH = (Ace, Hearts)
cAS = (Ace, Spades)


-----------------------------------------------------------

{-

This combines the previous exercises to determine the 
winner of a trick in a trick-taking card game, assuming 
there is no trump suit.

The winNT function takes a Trick (which consists of a 
leading player and a list of played cards in canonical 
order) and determines the winner. It does this by zipping 
the list of players with the list of played cards, and
filtering out only those cards that match the suit of the
lead card. It then finds the maximum card among those
cards and returns the corresponding player as the winner.

Note that the lead suit is determined by looking at the
card played by the leading player, which is found using
the fromEnum function to get the index of the leading
player in the list of players. 

Testing in GHCi

ghci> :set -i..
ghci> :l Ex6_57
ghci> winNT (Trick South [c2H, c5H, cKH, cJH])
South
ghci> winNT (Trick West [cKC, cAC, cQC, c9C])
East
ghci> winNT (Trick North [c5H, cQC, cAC, cKH])
West
ghci> winNT (Trick East [cKH, c5H, cAC, cQC])
North

-}