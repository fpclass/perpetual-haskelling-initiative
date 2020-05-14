module Purestone.Server.GameState where

import Data.Time.Clock

import Purestone.Board

-- | Represents the current board, whether player 1/player 2 has yet to receive the 
--   most recent changes and which player's turn it is
type GameState = (Maybe Board, UTCTime, Int)
