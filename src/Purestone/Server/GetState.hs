module Purestone.Server.GetState (getState) where

import Servant

import Purestone.Board

getState :: Int -> Handler Board
getState = undefined
