module Purestone.Server.MakeMove (makeMove) where

import Servant
import Data.IORef

import Purestone.Board
import Purestone.Card
import Purestone.Server.GameState

makeMove :: IORef GameState -> Int -> Int -> [Card] -> Handler Board
makeMove = undefined