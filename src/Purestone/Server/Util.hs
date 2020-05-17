-------------------------------------------------------------------------------
-- Perpetual Haskelling Initiative                                           --
-------------------------------------------------------------------------------
-- This source code is licensed under the MIT licence found in the           --
-- LICENSE file in the root directory of this source tree.                   --
-------------------------------------------------------------------------------
module Purestone.Server.Util ( connect, gameReady ) where

import Servant
import Data.IORef
import Data.Time.Clock
import Control.Monad.IO.Class (liftIO)

import Purestone.Deck
import Purestone.Board
import Purestone.Game
import Purestone.Server.ConnectResponse
import Purestone.Server.GameState

-- | `connect` will attempt to connect a new user to a game. It takes the IORef
--   of the game state so it can start games, and the IORef of [Deck] which 
--   stores the Deck of players when they join (as the setupGame is only run
--   when the second player has joined, so the Deck of the first player needs
--   to be tracked)
connect :: IORef GameState -> IORef [Deck] -> Deck -> Handler ConnectResponse
connect s ds d = do
    decks <- liftIO $ readIORef ds
    case decks of
        [] -> do
            liftIO $ atomicWriteIORef ds [d]
            pure $ Connected 1 1

        [d1] -> do
            liftIO $ atomicWriteIORef ds [d1, d]
            time <- liftIO getCurrentTime

            board <- liftIO $ setupGame decks
            liftIO $ atomicWriteIORef s (Just board, time, boardTurn board)
            pure $ Connected 1 2

        _ -> throwError err409

-- | `gameReady` takes a game ID and will return whether the 
--   game is ready to play. This will be used by clients to 
--   tell when a 2nd player has joined. It does this by checking
--   if the number of Decks stored in the IORef is 2
gameReady :: IORef [Deck] -> Int -> Handler Bool
gameReady ds _ = (==2) . length <$> liftIO (readIORef ds)
