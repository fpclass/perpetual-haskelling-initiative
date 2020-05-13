module Purestone.Server.MakeMove (makeMove) where

import Servant

import Purestone.Board
import Purestone.Card

makeMove :: Int -> [Card] -> Handler Board
makeMove = undefined