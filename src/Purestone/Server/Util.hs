-------------------------------------------------------------------------------
-- Perpetual Haskelling Initiative                                           --
-------------------------------------------------------------------------------
-- This source code is licensed under the MIT licence found in the           --
-- LICENSE file in the root directory of this source tree.                   --
-------------------------------------------------------------------------------
module Purestone.Server.Util ( connect, gameReady ) where

import Servant
import Data.Time.Clock
import qualified Data.IntMap as IM 
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar

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
connect :: TVar GameStates -> TVar (Int, Maybe Deck) -> Deck -> Handler ConnectResponse
connect s cd d = do
    (next, held) <- liftIO $ readTVarIO cd
    case held of
        Nothing -> do
            liftIO $ atomically $ writeTVar cd (next, Just d)
            pure $ Connected next 1

        Just d1 -> do
            liftIO $ atomically $ writeTVar cd (next + 1, Nothing)

            time <- liftIO getCurrentTime
            board <- setupGame (d1, d)
            liftIO $ atomically $ modifyTVar s $ IM.insert next (board, time, boardTurn board)
            pure $ Connected next 2

-- | `gameReady` takes a game ID and will return whether the 
--   game is ready to play. This will be used by clients to 
--   tell when a 2nd player has joined. It does this by checking
--   if the number of Decks stored in the IORef is 2
gameReady :: TVar GameStates -> Int -> Handler Bool
gameReady s g = IM.member g <$> liftIO (readTVarIO s)
