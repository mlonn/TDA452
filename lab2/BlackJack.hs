module BlackJack where
import           Cards
import           RunGame
import           System.Random
import           Test.QuickCheck

-- | Hands for testing
hand1 = Add (Card (Numeric 2) Spades) (Add (Card (Numeric 3) Clubs) empty)
hand2 = Add (Card (Numeric 5) Hearts) empty
hand3 = Add (Card (Numeric 4) Hearts) hand5
hand4 = Add (Card (Numeric 6) Spades) (Add (Card (Numeric 7) Clubs) empty)
hand5 = Add (Card (Numeric 5) Hearts) hand4
-- | Returns an empty hand
empty :: Hand
empty = Empty

-- | Find the value of a given hand.
-- Aces will count as 11 if possible otherwise 1
value :: Hand -> Integer
value Empty                  = 0
value hand | handValue <= 21 = handValue
           | handValue > 21  = handValue - aceValue
    where
        handValue = valueHand hand
        aceValue  = numberOfAces hand * 10

-- | Calculates the entire hand recursivly
valueHand :: Hand -> Integer
valueHand (Add card hand) = valueRank (rank card) + valueHand hand
valueHand _               = 0

-- | Translates the rank to its value.
valueRank :: Rank -> Integer
valueRank Ace             = 11
valueRank (Numeric value) = value
valueRank _               = 10

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
winner guest bank  | gameOver guest           = Bank
                   | gameOver bank            = Guest
                   | value guest > value bank = Guest
                   | otherwise                = Bank

-- | Keeps the order and places the first hand on top of the second one.
-- (<+) :: Hand -> Hand -> Hand
-- Empty <+ bottom = bottom
-- top <+ bottom   = top' ~+ bottom
--      where top' = flipHand top

(<+) :: Hand -> Hand -> Hand
Empty <+ bottom = bottom
top <+ bottom   = Add card (hand <+ bottom)
   where (Add card hand) = top

-- | Adds one hand to another. (Reverses order of first hand)
-- Takes first card from first argument and puts it on top of the second
-- until all cards in the first argument is in the second.
(~+) :: Hand -> Hand -> Hand
(Add card hand) ~+ targetHand = hand ~+ Add card targetHand
_ ~+ targetHand               = targetHand

-- | Reverses the order of cards.
flipHand :: Hand -> Hand
flipHand hand = hand ~+ Empty

-- | quickCheck test method for <+.
prop_onTopOf_assoc :: Hand -> Hand -> Hand -> Bool
prop_onTopOf_assoc p1 p2 p3 =
    p1<+(p2<+p3) == (p1<+p2)<+p3


prop_size_onTopOf :: Hand -> Hand -> Bool
prop_size_onTopOf h1 h2 = size h1 + size h2 == size (h1 <+ h2)

fullDeck :: Hand
fullDeck = foldr ((<+) . fullSuit) Empty suits
    where suits = [Hearts, Spades, Diamonds, Clubs]

fullSuit :: Suit -> Hand
fullSuit suit = foldr Add Empty cards
    where 
        ranks = [Numeric val | val <- [2..10]] ++ [Jack, Queen, King, Ace]
        cards = [Card r suit | r <- ranks]


draw :: Hand -> Hand -> (Hand,Hand)
draw deck hand | deck == Empty = error "draw: The deck is empty."
               | otherwise = (deck', Add card hand)
    where 
        Add card deck' = deck

prop_draw :: Hand -> Hand -> Property
prop_draw deck hand = deck /= Empty ==> 
                        size deck - 1 == size deck' && 
                        size hand + 1 == size hand' && 
                        deckTopCard == handTopCard
        where
            (deck', hand')                = draw deck hand
            Add deckTopCard deckRemainder = deck
            Add handTopCard handRemainder = hand'
        

playBank :: Hand -> Hand
playBank deck | deck == Empty = error "draw: The deck is empty."
              | otherwise     = playBank' deck Empty

playBank' :: Hand -> Hand -> Hand
playBank' deck bankHand | valueHand bankHand' < 16 = playBank' deck' bankHand'
                        | otherwise = bankHand'
    where (deck',bankHand') = draw deck bankHand
    
pickCard :: Integer -> Hand -> Card 
pickCard n deck  | deck == Empty = error "draw: The deck is empty."
                 | n > size deck = error "deck has less cards than n"
                 | otherwise = pickCard' n Empty deck

pickCard' :: Integer -> Hand -> Hand -> Card
pickCard' n stack deck | n == size stack = stackCard
                       | otherwise = pickCard' n (Add deckCard stack) deck'
            where 
                Add deckCard deck' = deck
                Add stackCard stack' = stack
                
    
    
    
    
    
    
    
    
    {-

prop_shuffle_sameCards :: StdGen -> Card -> Hand -> Bool
prop_shuffle_sameCards g c h =
    c `belongsTo` h == c `belongsTo` shuffle g h
-}
