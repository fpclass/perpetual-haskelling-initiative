-------------------------------------------------------------------------------
-- Perpetual Haskelling Initiative                                           --
-------------------------------------------------------------------------------
-- This source code is licensed under the MIT licence found in the           --
-- LICENSE file in the root directory of this source tree.                   --
-------------------------------------------------------------------------------
module Purestone.Server.GetState (getState) where

import Servant
import Data.Time.Clock
import qualified Data.IntMap as IM (lookup)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent.STM.TVar

import Purestone.Board
import Purestone.Server.Sanitise
import Purestone.Server.GameState

-- | `getState` attempts to return the current board if there are changes. It takes 
--   a QueryParam `update` and only returns the state if the latest change is after
--   the time sepecified in this query param, otherwise it returns HTTP304. If this 
--   query parameter is missing then the state is always returned
getState :: TVar GameStates -> Int -> Int -> Maybe UTCTime -> Handler Board
getState s g p d = do
    -- Searches for the GameState of the given game ID in the IntMap of game states
    -- If the game ID doesn't exist then Nothing is returned
    gs <- IM.lookup g <$> liftIO (readTVarIO s)

    -- If there is no board then return 404, otherwise determine response
    flip (maybe $ throwError err404) gs $ \(b, mod) -> 
        case d of
            Nothing -> pure $ sanitiseBoard b p
            Just date
                | date < mod -> pure $ sanitiseBoard b p
                | otherwise  -> throwError err304
