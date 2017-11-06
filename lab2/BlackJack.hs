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
winner guest bank  
    | gameOver guest           = Bank
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

-- | QuickCheck test method for <+.
prop_onTopOf_assoc :: Hand -> Hand -> Hand -> Bool
prop_onTopOf_assoc p1 p2 p3 =
    p1<+(p2<+p3) == (p1<+p2)<+p3

-- | tests the size property when using <+ operator 
prop_size_onTopOf :: Hand -> Hand -> Bool
prop_size_onTopOf h1 h2 = size h1 + size h2 == size (h1 <+ h2)

-- | Generates a full deck of 52 unique cards.
fullDeck :: Hand
fullDeck = foldr ((<+) . fullSuit) Empty suits
    where suits = [Hearts, Spades, Diamonds, Clubs]

-- | Given a suit it will generate a hand with all cards from that suit.
fullSuit :: Suit -> Hand
fullSuit suit = foldr Add Empty cards
    where 
        ranks = [Numeric val | val <- [2..10]] ++ [Jack, Queen, King, Ace]
        cards = [Card r suit | r <- ranks]

-- | Draws a single card from a deck and adds it to a hand.
-- Arguments are given as the deck then hand and 
-- returns them in the same order.
draw :: Hand -> Hand -> (Hand,Hand)
draw deck hand | deck == Empty = error "draw: The deck is empty."
               | otherwise = (deck', Add card hand)
    where 
        Add card deck' = deck

-- | Tests the size property of when drawing cards.
prop_draw :: Hand -> Hand -> Property
prop_draw deck hand = deck /= Empty ==> 
                        size deck - 1 == size deck' && 
                        size hand + 1 == size hand' && 
                        deckTopCard == handTopCard
        where
            (deck', hand')                = draw deck hand
            Add deckTopCard deckRemainder = deck
            Add handTopCard handRemainder = hand'
        
-- | Wrapper to let the bank draw cards.
playBank :: Hand -> Hand
playBank deck 
        | deck == Empty = error "draw: The deck is empty."
        | otherwise     = playBank' deck Empty

-- | Draws cards from a deck to a hand until that hand has a value of > 16
playBank' :: Hand -> Hand -> Hand
playBank' deck bankHand 
        | valueHand bankHand' < 16 = playBank' deck' bankHand'
        | otherwise = bankHand'
    where (deck',bankHand') = draw deck bankHand
    
-- | Removes card at position n from the deck.
removeCard :: Integer -> Hand -> (Card, Hand) 
removeCard n deck  
        | deck == Empty = error "draw: The deck is empty."
        | n > size deck = error "deck has less cards than n"
        | otherwise = removeCard' n Empty deck

-- | Moves card to from one hand to another until you find the card to be
-- removed, then adds the two hands together
-- keeps the order of the original deck.
removeCard' :: Integer -> Hand -> Hand -> (Card, Hand)
removeCard' n top bottom 
        | n == 1 = (card' , top ~+ bottom')
        | n < 1 = error "removeCard: Can't remove <1 card"
        | n > size bottom = error "removeCard: Not that many cards in deck"
        | otherwise = removeCard' (n-1) (Add card' top) bottom'
    where (Add card' bottom') = bottom

-- | Shuffles the cards in a given deck
shuffleCards :: StdGen -> Hand -> Hand
shuffleCards g deck = shuffle' g Empty deck

-- | Takes a random card from the source deck and adds it to a target deck
--   until all cards have been removed from the source deck
shuffle' :: StdGen -> Hand -> Hand -> Hand
shuffle' g target Empty = target
shuffle' g target source = shuffle' g' (Add card' target) source' 
    where 
        (nr, g') = randomR (1, size source) g
        (card', source') = removeCard nr source

-- | Checks that the cards are the same after shuffle.
prop_shuffle_sameCards :: StdGen -> Card -> Hand -> Bool
prop_shuffle_sameCards g c h =
    c `belongsTo` h == c `belongsTo` shuffleCards g h

-- | Checks for a card in a deck.
belongsTo :: Card -> Hand -> Bool
c `belongsTo` Empty = False
c `belongsTo` (Add c' h) = c == c' || c `belongsTo` h 

-- | Checks that the deck has the same size befor and after shuffle
prop_size_shuffle :: StdGen -> Hand -> Bool
prop_size_shuffle g deck = size deck == size (shuffleCards g deck)


-- | Defines what functions should be callable.
implementation = Interface
    { iEmpty    = empty
    , iFullDeck = fullDeck
    , iValue    = value
    , iGameOver = gameOver
    , iWinner   = winner 
    , iDraw     = draw
    , iPlayBank = playBank
    , iShuffle  = shuffleCards
    }

-- | Run a new game round.
main :: IO ()
main = runGame implementation