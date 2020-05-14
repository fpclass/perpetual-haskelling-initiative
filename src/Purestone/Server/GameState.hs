module Purestone.Server.GameState where

import Purestone.Board

-- | Represents the current state, and whether player 1/player 2
--   has received the most recent changes respectfully
type GameState = (Maybe Board, Bool, Bool)
