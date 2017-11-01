module BlackJack where
import Cards
import RunGame

-- | Hands for testing
hand1 = Add (Card (Numeric 2) Spades) (Add (Card Ace Clubs) empty)
hand2 = Add (Card Ace Hearts) hand1

-- | Returns an empty hand
empty :: Hand
empty = Empty

-- | Find the value of a given hand. 
-- Aces will count as 11 if possible otherwise 1
value :: Hand -> Integer
value Empty           = 0
value (Add card hand) | handValue <= 21 = handValue
                      | handValue > 21 = handValue - aceValue
    where
        handValue = valueRank (rank card) + value hand
        aceValue = numberOfAces (Add card hand) * 10

-- | Translates the rank to its value.
valueRank :: Rank -> Integer
valueRank Ace             = 11
valueRank King            = 10
valueRank Queen           = 10
valueRank Jack            = 10
valueRank (Numeric value) = value

-- | Recursivly counts all aces in the given hand.
numberOfAces :: Hand -> Integer
numberOfAces Empty                              = 0
numberOfAces (Add card hand) | rank card == Ace = 1 + numberOfAces hand
                             | otherwise        = numberOfAces hand
-- | Checks if the hands value is above 21
gameOver :: Hand -> Bool
gameOver hand = value hand > 21 

-- | Checks which hand is highest value and not over 21.
-- Take the hands in order Guest -> Bank.
winner :: Hand -> Hand -> Player
winner guest bank  | gameOver guest = Bank
                   | gameOver bank = Guest
                   | value guest > value bank = Guest
                   | otherwise = Bank
