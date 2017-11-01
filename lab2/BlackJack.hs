module BlackJack where
import Cards
import RunGame

empty :: Hand
empty = Empty

value :: Hand -> Integer
value Empty           = 0
value (Add card hand) | handValue <= 21 = handValue
                      | handValue > 21 = handValue - aceValue
    where
        handValue = valueRank (rank card) + value hand
        aceValue = numberOfAces (Add card hand) * 10

valueRank :: Rank -> Integer
valueRank Ace             = 11
valueRank King            = 10
valueRank Queen           = 10
valueRank Jack            = 10
valueRank (Numeric value) = value

numberOfAces :: Hand -> Integer
numberOfAces Empty                              = 0
numberOfAces (Add card hand) | rank card == Ace = 1 + numberOfAces hand
                             | otherwise        = numberOfAces hand


hand = Add (Card Ace Hearts) 
        (Add (Card Ace Hearts)
        (Add (Card Ace Spades) Empty))

hand2 = Add (Card Ace Hearts) 
        (Add (Card Jack Hearts)
        (Add (Card (Numeric 3) Spades) Empty))

--gameOver :: Hand -> Bool

--winner :: Hand -> Hand -> Player
