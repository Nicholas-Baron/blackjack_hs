{-# LANGUAGE LambdaCase #-}

module Main where

import Card
import Data.List (intercalate)
import Data.Maybe (fromJust)
import GameState
import System.Random.Shuffle (shuffleM)

shuffledDeck :: IO [Card]
shuffledDeck = shuffleM initialDeck

showHand :: [Card] -> String
showHand = intercalate ", " . map show

showValues :: [Card] -> String
showValues = intercalate " or " . map show . valueOfHand

printHandValue :: String -> [Card] -> IO ()
printHandValue prefix hand =
  putStrLn $
    concat
      [ prefix,
        ' ' : showHand hand,
        "\nValue: ",
        showValues hand
      ]

data EndOfPlayerTurn
  = EndGame
  | PlayerBust GameState
  | PlayerWait GameState
  | PlayerWin GameState

handleInput :: GameState -> IO EndOfPlayerTurn
handleInput state = do
  putStrLn $ "Dealer has " ++ show (dealerTopCard state)
  printHandValue "You have" $ playerHand state
  case hasPlayerWonOrLost state of
    Just True -> return (PlayerWin state)
    Just False -> putStrLn "Bust" >> return (PlayerBust state)
    Nothing -> do
      userInput <- getLine
      case parseAction userInput of
        Nothing -> do
          putStrLn "Enter one of the following (case insensitive): Hit, Stay, Quit"
          handleInput state
        Just Quit -> return EndGame
        Just Hit -> handleInput (playerHit state)
        Just Stay -> return (PlayerWait state)

printWinner :: GameState -> IO ()
printWinner state = do
  let playerValues = filter (<= 21) $ valueOfPlayerHand state
      dealerValues = filter (<= 21) $ valueOfDealerHand state

  printHandValue "Dealer's final hand" $ dealerHand state

  putStrLn $ case (playerValues, dealerValues) of
    ([], _) -> "Dealer won"
    (_, []) -> "You won!"
    _ -> case maximum playerValues `compare` maximum dealerValues of
      LT -> "Dealer won"
      EQ -> "Push"
      GT -> "You won!"

playRound :: GameState -> IO ()
playRound state =
  let setupState = dealerPlay . playerHit . dealerPlay . playerHit
   in handleInput (setupState state) >>= \case
        EndGame -> putStrLn "Bye!"
        PlayerWait state -> printWinner (endPlayerRound state) >> newRound
        PlayerBust state -> printWinner (endPlayerRound state) >> newRound
        PlayerWin state -> printWinner (endPlayerRound state) >> newRound

newRound :: IO ()
newRound = shuffledDeck >>= playRound . fromJust . fromStartingDeck

main :: IO ()
main = newRound
