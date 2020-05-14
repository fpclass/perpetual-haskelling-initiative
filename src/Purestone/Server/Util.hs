module Purestone.Server.Util ( connect, gameReady ) where

import Servant
import Data.IORef
import Control.Monad.IO.Class (liftIO)

import Purestone.Deck
import Purestone.Server.ConnectResponse
import Purestone.Server.GameState

-- | `connect` will attempt to connect a new user to a game
connect :: IORef GameState -> Deck -> Handler ConnectResponse
connect s d = do
    (_, n, _, _) <- liftIO $ readIORef s
    case n of
        0 -> do
            liftIO $ atomicWriteIORef s (Nothing, 1, False, False)
            pure $ Connected 1 1
        1 -> do
            liftIO $ atomicWriteIORef s (Nothing, 2, True, True)
            pure $ Connected 1 2
        _ -> throwError $ err500

-- | `gameReady` takes a game ID and will return whether the 
--   game is ready to play. This will be used by clients to 
--   tell when a 2nd player has joined
gameReady :: IORef GameState -> Int -> Handler Bool
gameReady s _ = do
    (_, n, _, _) <- liftIO $ readIORef s
    pure $ n == 2
