module Card (Card (..), Suit (..), Value (..), initialDeck, valueOf) where

data Suit = Heart | Club | Spade | Diamond deriving (Show, Eq)

data Value = Ace | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King deriving (Eq)

instance Show Value where
  show Ace = "Ace"
  show Two = "2"
  show Three = "3"
  show Four = "4"
  show Five = "5"
  show Six = "6"
  show Seven = "7"
  show Eight = "8"
  show Nine = "9"
  show Ten = "10"
  show Jack = "Jack"
  show Queen = "Queen"
  show King = "King"

newtype Card = Card (Value, Suit)

initialDeck :: [Card]
initialDeck =
  [ Card (value, suit)
    | value <- [Ace, Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen, King],
      suit <- [Heart, Diamond, Spade, Club]
  ]

valueOf :: Card -> (Integer, Maybe Integer)
valueOf (Card (value, _)) = case value of
  Ace -> (1, Just 11)
  Two -> (2, Nothing)
  Three -> (3, Nothing)
  Four -> (4, Nothing)
  Five -> (5, Nothing)
  Six -> (6, Nothing)
  Seven -> (7, Nothing)
  Eight -> (8, Nothing)
  Nine -> (9, Nothing)
  _ -> (10, Nothing)

instance Show Card where
  show (Card (value, suit)) =
    concat
      [ show value,
        " of ",
        show suit,
        "s"
      ]
