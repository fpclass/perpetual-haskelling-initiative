module Purestone.Server.GameState where

import Purestone.Board

-- | Represents the current board, the number of players
--   and whether player 1/player 2 has yet to receive the 
--   most recent changes
type GameState = (Maybe Board, Int, Bool, Bool)
