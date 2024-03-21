module Main where

import Card
import Data.List (intercalate)
import GameState
import System.Random.Shuffle (shuffleM)

shuffledDeck :: IO [Card]
shuffledDeck = shuffleM initialDeck

printHandValue :: [Card] -> IO ()
printHandValue hand = do
  print $ "Hand: " ++ show hand
  print $ "Total Value: " ++ intercalate " or " (map show $ valueOfHand hand)

handleInput :: GameState -> IO (Maybe GameState)
handleInput state = do
  print $ "Dealer has " ++ show (dealerTopCard state)
  printHandValue $ playerHand state
  if minimum (valueOfPlayerHand state) > 21
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

printWinner :: GameState -> IO ()
printWinner state = do
  let playerValues = filter (<= 21) $ valueOfPlayerHand state
      dealerValues = filter (<= 21) $ valueOfDealerHand state

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
  maybe
    (putStrLn "Bye!")
    ( \playerDoneState ->
        printWinner (endPlayerRound playerDoneState) >> newRound
    )
    afterInputs

newRound :: IO ()
newRound = shuffledDeck >>= playRound . fromStartingDeck

main :: IO ()
main = newRound
