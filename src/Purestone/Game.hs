-------------------------------------------------------------------------------
-- Perpetual Haskelling Initiative                                           --
-------------------------------------------------------------------------------
-- This source code is licensed under the MIT licence found in the           --
-- LICENSE file in the root directory of this source tree.                   --
-------------------------------------------------------------------------------
module Purestone.Game ( setupGame, processMove, draw, resetPoints,
        increasePoints, startTurn, changeTurn ) where

import Purestone.Board
import Purestone.Card
import Purestone.Deck
import Purestone.Player

-- | `setupGame` takes the 2 players decks (in a list) and creates the initial
--   board and determines the starting player
setupGame :: [Deck] -> (Board, Int)
setupGame = undefined

-- | `processMove` takes the current board, the cards to play and the player
--   number and attempts to perform the move. `Nothing` is returned if this
--   fails
processMove :: Board -> [Card] -> Int -> Maybe Board
processMove = undefined

-- | `draw` takes a player and draws the top card from their deck into their hand
draw :: Player -> Player
-- If no cards, the player takes damage equal to amount of overdraws
draw (Player hp h [] c m bs dp o) = (Player (hp - (o+1)) h [] c m bs dp (o+1))
draw (Player hp h (x:xs) c m bs dp o) = case length h of
  10 -> (Player hp h xs c m bs dp o) -- If the player has 10 cards, drawn card is lost
  otherwise -> (Player hp (x:h) xs c m bs dp o)

-- | `resetPoints` sets the player's current points to their max points
resetPoints :: Player -> Player
resetPoints (Player hp h d _ m bs dp o) =  (Player hp h d m m bs dp o)

-- | `increasePoints` increases a player's maximum amount of points up to 10
increasePoints :: Player -> Player
increasePoints player@(Player _ _ _ _ 10 _ _ _) = player
increasePoints (Player hp h d c m bs dp o) = (Player hp h d c (m+1) bs dp o)

-- | `startTurn` starts a player's turn
startTurn :: Player -> Player
startTurn player = resetPoints . increasePoints . draw $ player

-- | `changeTurn` switches the turn from player 1 to player 2 or player 2 to
--    player 1 and calls the start turn function for that player
changeTurn :: Board -> Board
changeTurn (Board p1 p2 0) = (Board p1 (startTurn p2) 1)
changeTurn (Board p1 p2 _) = (Board (startTurn p1) p2 0)
