-------------------------------------------------------------------------------
-- Perpetual Haskelling Initiative                                           --
-------------------------------------------------------------------------------
-- This source code is licensed under the MIT licence found in the           --
-- LICENSE file in the root directory of this source tree.                   --
-------------------------------------------------------------------------------
module Purestone.Game ( setupGame, processMove, draw, resetPoints,
        increasePoints, changeTurn ) where

import Purestone.Board
import Purestone.Card
import Purestone.Deck
import Purestone.Player
import Purestone.Random ( shuffleIO )

import System.Random ( randomRIO )
import Control.Monad.IO.Class (liftIO)
import Servant

-- | `setupGame` takes the 2 players decks (in a list) and creates the initial
--   board and determines the starting player
setupGame :: [Deck] -> Handler Board
setupGame [d1,d2] = do 
    start <- liftIO $ randomRIO (1,2)
    shuffled1 <- liftIO $ shuffleIO d1
    shuffled2 <- liftIO $ shuffleIO d2
    player1 <- initPlayer shuffled1
    player2 <- initPlayer shuffled2
    return $ Board player1 player2 start
setupGame _ = throwError err412

-- | `initPlayer` creates a new Player from a Deck following the specs in the `README`. 
-- The top 3 cards are removed from the deck and placed into the player's hand
initPlayer :: Deck -> Handler Player
initPlayer (c1:c2:c3:d) = return $ Player 30 [c1,c2,c3] d 0 10 [] [] 0
initPlayer _ = throwError err412

-- | `processMove` takes the current board, the cards to play and the player
--   number and attempts to perform the move. `Nothing` is returned if this
--   fails
processMove :: Board -> [Card] -> Int -> Maybe Board
processMove = undefined

-- | `draw` takes a player and draws the top card from their deck into their hand
draw :: Player -> Player
-- If no cards, the player takes damage equal to amount of overdraws
draw (Player hp h [] c m bs dp o) = Player (hp - (o+1)) h [] c m bs dp (o+1)
draw (Player hp h (x:xs) c m bs dp o) = case length h of
  10 -> Player hp h xs c m bs dp o -- If the player has 10 cards, drawn card is lost
  otherwise -> Player hp (x:h) xs c m bs dp o

-- | `resetPoints` sets the player's current points to their max points
resetPoints :: Player -> Player
resetPoints player@Player{..} =  player { playerCurrentPoints = playerMaximumPoints }

-- | `increasePoints` increases a player's maximum amount of points up to 10
increasePoints :: Player -> Player
increasePoints player@Player{playerMaximumPoints = 10} = player
increasePoints player@Player{..} = player { playerMaximumPoints = playerMaximumPoints + 1 }

-- | `startTurn` starts a player's turn
startTurn :: Player -> Player
startTurn player = resetPoints . increasePoints . draw $ player

-- | `changeTurn` switches the turn from player 1 to player 2 or player 2 to
--    player 1 and calls the start turn function for that player
changeTurn :: Board -> Board
changeTurn (Board p1 p2 1) = Board p1 (startTurn p2) 2
changeTurn (Board p1 p2 _) = Board (startTurn p1) p2 1
