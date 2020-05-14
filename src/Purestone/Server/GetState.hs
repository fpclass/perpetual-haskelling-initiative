module Purestone.Server.GetState (getState) where

import Servant
import Data.IORef
import Data.Time.Clock
import Control.Monad.IO.Class (liftIO)

import Purestone.Board
import Purestone.Server.GameState

-- | `getState` attempts to return the current board if there are changes. If the
--   player already has the latest changes then HTTP304 is returned 
getState :: IORef GameState -> Int -> Int -> Maybe UTCTime -> Handler Board
getState s _ p d = do
    (b, mod, _) <- liftIO $ readIORef s

    -- If there is no board then return 404, otherwise determine response
    flip (maybe $ throwError err404) b $ \b' -> 
        case d of 
            Nothing -> pure b'
            Just date
                | date < mod -> pure b'
                | otherwise  -> throwError err304
