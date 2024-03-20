module GameState
  ( GameState,
    fromStartingDeck,
    playerHit,
    dealerPlay,
    endPlayerRound,
    dealerTopCard,
    playerHand,
    valueOfHand,
    valueOfDealerHand,
    valueOfPlayerHand,
    PlayerAction (..),
    parseAction,
  )
where

import Card
import Data.Char (toLower)
import Data.Maybe (listToMaybe, maybeToList)

data GameState = GameState
  { playerHand :: [Card],
    dealerHand :: [Card],
    deck :: [Card]
  }

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

dealerTopCard :: GameState -> Maybe Card
dealerTopCard GameState {dealerHand} =
  listToMaybe dealerHand

dealerPlay :: GameState -> GameState
dealerPlay (GameState {deck, playerHand, dealerHand}) =
  let (toDealer : rest) = deck in GameState {deck = rest, playerHand = playerHand, dealerHand = toDealer : dealerHand}

endPlayerRound :: GameState -> GameState
endPlayerRound inputState@GameState {deck, playerHand, dealerHand} =
  let (nextCard : rest) = deck
      canTake = (< 18) . minimum . valueOfHand
   in if canTake dealerHand
        then endPlayerRound GameState {deck = rest, dealerHand = nextCard : dealerHand, playerHand = playerHand}
        else inputState

valueOfPlayerHand :: GameState -> [Integer]
valueOfPlayerHand = valueOfHand . dealerHand

valueOfDealerHand :: GameState -> [Integer]
valueOfDealerHand = valueOfHand . dealerHand

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
