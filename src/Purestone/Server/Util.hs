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
import Purestone.Game
import Purestone.Server.ConnectResponse
import Purestone.Server.GameState

-- | `connect` will attempt to connect a new user to a game. It takes the TVar
--   of the game states so it can start games, and the TVar of current start
--   state which stores the ID for the next game and the deck of the waiting
--   player if there is one
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
            liftIO $ atomically $ modifyTVar s $ IM.insert next (board, time)
            pure $ Connected next 2

-- | `gameReady` takes a game ID and will return whether the game is ready to
--   play. This will be used by clients to tell when a 2nd player has joined.
--   It does this by checking if the game ID given exists in the IntMap of game
--   states.
gameReady :: TVar GameStates -> Int -> Handler Bool
gameReady s g = IM.member g <$> liftIO (readTVarIO s)
