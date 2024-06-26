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

hasPlayerWonOrLost :: GameState -> Maybe Bool
hasPlayerWonOrLost state =
  let playerValues = valueOfPlayerHand state
   in if 21 `elem` playerValues then Just True else if not (any (< 21) playerValues) then Just False else Nothing

handleInput :: GameState -> IO (Maybe GameState)
handleInput state = do
  putStrLn $ "Dealer has " ++ show (dealerTopCard state)
  printHandValue $ playerHand state
  case hasPlayerWonOrLost state of
    Just True -> return (Just state)
    Just False -> putStrLn "Bust" >> return (Just state)
    Nothing -> do
      userInput <- getLine
      case parseAction userInput of
        Nothing -> do
          putStrLn "Enter one of the following (case insensitive): Hit, Stay, Quit"
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
playRound state =
  let setupState = dealerPlay . playerHit . dealerPlay . playerHit
   in handleInput (setupState state)
        >>= maybe
          (putStrLn "Bye!")
          ( \playerDoneState ->
              printWinner (endPlayerRound playerDoneState) >> newRound
          )

newRound :: IO ()
newRound = shuffledDeck >>= playRound . fromStartingDeck

main :: IO ()
main = newRound
