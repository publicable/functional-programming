{- 
3.2 - size hand2
  size hand2
= size (Add(Card(Numeric 2) Hearts)(Add(Card Jack Spades) Empty))
= 1 + size(Add(Card Jack Spades) Empty)
= 1 + 1 + size(Empty)
= 1 + 1 + 0
= 2
-}

module BlackJack where 
import Cards
import Wrapper

-- Gives and empty hand
empty :: Hand
empty = Empty

-- Calculates the value of the given hand
value :: Hand -> Integer
value Empty                      = 0

{- First calculates if hand is over 21, 
   if it is subtracts 10 for every ace
   in order to change ace value to 1 -}
value hand | handValue hand > 21 = handValue hand - noa * 10    
           | otherwise           = handValue hand
    where
    handValue (Add card hand)= valueCard card + handValue hand
    handValue Empty          = 0
    noa = numberOfAces hand

-- Calculates the value of a rank
valueRank :: Rank -> Integer
valueRank (Numeric n) = n
valueRank Ace         = 11
valueRank _           = 10

-- Calculates the value of a card
valueCard :: Card -> Integer
valueCard card = valueRank (rank card)

-- Calculates the amount of aces in a hand
numberOfAces :: Hand -> Integer
numberOfAces Empty                              = 0
numberOfAces (Add card hand) | rank card == Ace = 1 + numberOfAces hand
                             | otherwise        = 0 + numberOfAces hand

-- Checks if the hand is over the limit or not
gameOver :: Hand -> Bool
gameOver hand = value hand > 21

-- Checks which player is the winner,
-- Guest (first hand) or Bank (second hand)
winner :: Hand -> Hand -> Player
winner h1 _  | gameOver h1         = Bank       -- Bank always wins if Guest is bust
winner _  h2 | gameOver h2         = Guest      -- since we know Guest is not bust
winner h1 h2 | value h1 > value h2 = Guest      -- since we know neither player is bust
             | otherwise           = Bank

--- End of part A ---
