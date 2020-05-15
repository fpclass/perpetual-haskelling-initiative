-------------------------------------------------------------------------------
-- Perpetual Haskelling Initiative                                           --
-------------------------------------------------------------------------------
-- This source code is licensed under the MIT licence found in the           --
-- LICENSE file in the root directory of this source tree.                   --
-------------------------------------------------------------------------------
module Purestone.Game ( setupGame, processMove ) where

import Purestone.Board
import Purestone.Card
import Purestone.Deck

-- | `setupGame` takes the 2 players decks (in a list) and creates the initial 
--   board and determines the starting player
setupGame :: [Deck] -> (Board, Int)
setupGame = undefined

-- | `processMove` takes the current board, the cards to play and the player 
--   number and attempts to perform the move. `Nothing` is returned if this 
--   fails
processMove :: Board -> [Card] -> Int -> Maybe Board
processMove = undefined
