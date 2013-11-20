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
import System.Random



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

(<+)::Hand -> Hand ->Hand
hand <+ Empty = hand
Empty <+ hand = hand
(Add card hand1) <+ hand2 = hand1 <+ (Add card hand2)


fullDeck::Hand
fullDeck = fullSuit Hearts <+ fullSuit Diamonds <+ fullSuit Spades  <+ fullSuit Clubs
    where
    fullSuit suit = (Add (Card Ace suit)(Add (Card King suit)
                      (Add (Card Queen suit) (Add (Card Jack suit)
                      (Add (Card (Numeric 10) suit) (Add (Card (Numeric 9) suit) 
                      (Add (Card (Numeric 8) suit) (Add (Card (Numeric 7) suit) 
                      (Add (Card (Numeric 6) suit) (Add (Card (Numeric 5) suit) 
                      (Add (Card (Numeric 4) suit) (Add (Card (Numeric 3) suit) 
                      (Add (Card (Numeric 2) suit) Empty)))))))))))))
draw::Hand -> Hand -> (Hand,Hand)
draw Empty _ = error "draw: the deck is empty"
draw (Add card hand1) hand2 = (hand1, (Add card hand2))

playBank::Hand -> Hand
playBank deck = playBank' deck Empty

{- helperfunction with first argument deck and 
   second argument banks hand. 
   As long as bankHands value is under 16, it draws new cards from the deck
-}
playBank'::Hand -> Hand -> Hand
playBank' deck bankHand | value bankHand >= 16 = bankHand
playBank' deck bankHand | otherwise            = playBank' deck' bankHand'
    where 
    (deck', bankHand') = draw deck bankHand


shuffle::StdGen -> Hand -> Hand
shuffle _ Empty = Empty
shuffle g deck = shuffle' g deck Empty


shuffle'::StdGen -> Hand -> Hand ->Hand
shuffle' g Empty shuffled = shuffled

--removes the randomized card from the original deck and adds it to the shuffled deck.
shuffle' g deck shuffled = shuffle' g' deck' (Add card shuffled) 
    where 
    (card, deck') = removeCard n deck -- uses helperfunction to remove nth card from deck.
    (n, g') = randomR (1, (size deck)) g -- generates random values.

--gives the nth card and the deck without the nth card.
removeCard::Integer -> Hand -> (Card, Hand)
removeCard _ Empty = error "removeCard: deck is empty"
removeCard n hand = removeCard' n hand Empty

removeCard'::Integer -> Hand -> Hand -> (Card, Hand)
--when the card is found, give it as result and add upp the two parts of the deck to one deck
removeCard' 1 (Add card lower) upper = (card,lower<+upper)
--iterates over cards by moving them one by one from lower to upper part of deck.
removeCard' n (Add card lower) upper = removeCard' (n-1) lower (Add card upper)

removeCard' n Empty _ = error "removeCard': deck is empty"

belongsTo :: Card -> Hand -> Bool
c `belongsTo` Empty = False
c `belongsTo` (Add c' h) = c == c' || c `belongsTo` h

implementation = Interface
  { iEmpty = empty
  , iFullDeck = fullDeck
  , iValue = value
  , iGameOver = gameOver
  , iWinner = winner
  , iDraw = draw
  , iPlayBank = playBank
  , iShuffle = shuffle
  }

main::IO()
main = runGame implementation
