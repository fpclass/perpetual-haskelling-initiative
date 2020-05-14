module Purestone.Server.Util ( connect, gameReady ) where

import Servant
import Data.IORef
import Control.Monad.IO.Class (liftIO)

import Purestone.Deck
import Purestone.Server.ConnectResponse
import Purestone.Server.GameState

-- TEMPORARY DEFINITION: setupGame takes the decks and creates the starting board
setupGame :: [Deck] -> Board
setupGame = undefined

-- | `connect` will attempt to connect a new user to a game
connect :: IORef GameState -> IORef [Deck] -> Deck -> Handler ConnectResponse
connect s ds d = do
    decks <- liftIO $ readIORef ds
    case length decks of
        0 -> do
            liftIO $ atomicWriteIORef ds [d]
            pure $ Connected 1 1
        1 -> do
            liftIO $ atomicWriteIORef ds (decks++[d])
            liftIO $ atomicWriteIORef s (setupGame ds, True, True)
            pure $ Connected 1 2
        _ -> throwError $ err500

-- | `gameReady` takes a game ID and will return whether the 
--   game is ready to play. This will be used by clients to 
--   tell when a 2nd player has joined
gameReady :: IORef [Deck] -> Int -> Handler Bool
gameReady ds _ = (==2) <$> length <$> liftIO (readIORef ds)
