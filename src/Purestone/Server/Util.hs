module Purestone.Server.Util ( connect, gameReady ) where

import Servant
import Data.IORef

import Purestone.Server.ConnectResponse
import Purestone.Server.GameState

connect :: IORef GameState -> Handler ConnectResponse
connect = undefined

gameReady :: IORef GameState -> Int -> Handler Bool
gameReady = undefined
