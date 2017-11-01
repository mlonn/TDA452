module BlackJackSune where
    import Cards
    import RunGame
    
    hand1 = Add (Card (Numeric 2) Spades) (Add (Card Ace Clubs) empty)
    hand2 = Add (Card Ace Hearts) hand1
    
    empty :: Hand
    empty = Empty
    
    value :: Hand -> Integer
    value Empty = 0
    value (Add card hand) = valueRank (rank card) + value hand
    
    numberOfAces :: Hand -> Integer
    numberOfAces Empty = 0
    numberOfAces (Add card hand) | rank card == Ace = 1 + numberOfAces hand
                                 | otherwise        = numberOfAces hand
    
    valueRank :: Rank -> Integer
    valueRank Ace = 11
    valueRank King = 10
    valueRank Queen = 10
    valueRank Jack = 10
    valueRank (Numeric value) = value
    
    --gameOver :: Hand -> Bool
    
    --winner :: Hand -> Hand -> Player
    