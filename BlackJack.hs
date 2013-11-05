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

empty :: Hand
empty = Empty

