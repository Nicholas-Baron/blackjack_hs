module Main where

import Card
import Data.Char (toLower)
import Data.List (intercalate)
import Data.Maybe (maybeToList)
import System.Random.Shuffle (shuffleM)

shuffledDeck :: IO [Card]
shuffledDeck = shuffleM initialDeck

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

data GameState = GameState
  { playerHand :: [Card],
    dealerHand :: [Card],
    deck :: [Card]
  }

initialState :: GameState
initialState = fromStartingDeck initialDeck

fromStartingDeck :: [Card] -> GameState
fromStartingDeck deck =
  GameState
    { playerHand = [],
      dealerHand = [],
      deck = deck
    }

playerHit :: GameState -> GameState
playerHit (GameState {deck, playerHand, dealerHand}) =
  let (toPlayer : rest) = deck
   in GameState
        { deck = rest,
          dealerHand = dealerHand,
          playerHand = toPlayer : playerHand
        }

dealerPlay :: GameState -> GameState
dealerPlay (GameState {deck, playerHand, dealerHand}) =
  let (toDealer : rest) = deck in GameState {deck = rest, playerHand = playerHand, dealerHand = toDealer : dealerHand}

data PlayerAction
  = Hit
  | Stay
  | Quit
  deriving (Eq)

parseAction :: String -> Maybe PlayerAction
parseAction input = case map toLower input of
  "hit" -> Just Hit
  "stay" -> Just Stay
  "quit" -> Just Quit
  _ -> Nothing

printHandValue :: [Card] -> IO ()
printHandValue hand = do
  print $ "Hand: " ++ show hand
  print $ "Total Value: " ++ intercalate " or " (map show $ valueOfHand hand)

valueOfHand :: [Card] -> [Integer]
valueOfHand = foldl valueAdder []
  where
    valueAdder :: [Integer] -> Card -> [Integer]
    valueAdder [] c = let (val1, val2) = valueOf c in val1 : maybeToList val2
    valueAdder oldValues c =
      let (val1, val2) = valueOf c
       in case val2 of
            Nothing -> map (+ val1) oldValues
            Just otherVal -> map (+ val1) oldValues ++ map (+ otherVal) oldValues

handleInput :: GameState -> IO (Maybe GameState)
handleInput state = do
  print $ "Dealer has " ++ show (head $ dealerHand state)
  printHandValue $ playerHand state
  if minimum (valueOfHand $ playerHand state) > 21
    then do
      print "Bust"
      return (Just state)
    else do
      userInput <- getLine
      case parseAction userInput of
        Nothing -> do
          print "Enter one of the following (case insensitive): Hit, Stay, Quit"
          handleInput state
        Just Quit -> return Nothing
        Just Hit -> handleInput (playerHit state)
        Just Stay -> return (Just state)

endPlayerRound :: GameState -> GameState
endPlayerRound inputState@GameState {deck, playerHand, dealerHand} =
  let (nextCard : rest) = deck
      canTake = (< 18) . minimum . valueOfHand
   in if canTake dealerHand
        then endPlayerRound GameState {deck = rest, dealerHand = nextCard : dealerHand, playerHand = playerHand}
        else inputState

printWinner :: GameState -> IO ()
printWinner state = do
  let playerValues = filter (<= 21) $ valueOfHand $ playerHand state
      dealerValues = filter (<= 21) $ valueOfHand $ dealerHand state

  print $ case (playerValues, dealerValues) of
    ([], _) -> "Dealer won"
    (_, []) -> "You won!"
    _ -> case maximum playerValues `compare` maximum dealerValues of
      LT -> "Dealer won"
      EQ -> "Push"
      GT -> "You won!"

playRound :: GameState -> IO ()
playRound state = do
  let setupState = dealerPlay . playerHit . dealerPlay . playerHit
  afterInputs <- handleInput (setupState state)
  case afterInputs of
    Nothing -> print "Bye!"
    Just state -> do
      printWinner (endPlayerRound state)
      shuffledDeck >>= playRound . fromStartingDeck

main :: IO ()
main = shuffledDeck >>= playRound . fromStartingDeck
