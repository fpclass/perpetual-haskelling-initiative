module Purestone.Server.GetState (getState) where

import Servant
import Data.IORef

import Purestone.Board
import Purestone.Server.GameState

getState :: IORef GameState -> Int -> Int -> Handler Board
getState = undefined
