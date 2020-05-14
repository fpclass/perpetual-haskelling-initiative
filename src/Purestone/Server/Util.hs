module Purestone.Server.Util ( connect, gameReady ) where

import Servant
import Data.IORef

import Purestone.Server.ConnectResponse
import Purestone.Server.GameState

-- | `connect` will attempt to connect a new user to a game
connect :: IORef GameState -> Handler ConnectResponse
connect = undefined

-- | `gameReady` takes a game ID and will return whether the 
--   game is ready to play. This will be used by clients to 
--   tell when a 2nd player has joined
gameReady :: IORef GameState -> Int -> Handler Bool
gameReady = undefined
