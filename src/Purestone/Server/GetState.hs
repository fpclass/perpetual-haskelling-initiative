module Purestone.Server.GetState (getState) where

import Servant
import Data.IORef
import Control.Monad.IO.Class (liftIO)

import Purestone.Board
import Purestone.Server.GameState

-- | `stateResponse` determines the response to send to the player
stateResponse :: IORef GameState -- ^ IORef to the GameState
              -> Board           -- ^ Board to return to the user if there is changes
              -> Bool            -- ^ Boolean of whether there is new changes for the player, if False HTTP304 is returned
              -> (GameState -> (GameState, ())) -- ^ Function used to edit IORef to show player now has latest changed
              -> Handler Board
stateResponse s b c f = 
    if c then do
        liftIO $ atomicModifyIORef s f
        pure b
    else
        throwError err304

-- | `getState` attempts to return the current board if there are changes. If the
--   player already has the latest changes then HTTP304 is returned 
getState :: IORef GameState -> Int -> Int -> Handler Board
getState s _ p = do
    (b, u1, u2) <- liftIO $ readIORef s

    -- If there is no board then return 404, otherwise determine response
    flip (maybe $ throwError err404) b $ \b' -> 
        case p of
            1 -> stateResponse s b' u1 $ \(b', _, u2) -> ((b', False, u2), ())
            2 -> stateResponse s b' u2 $ \(b', u1, _) -> ((b', u1, False), ())
            _ -> throwError err400

