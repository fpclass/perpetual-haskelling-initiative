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

-- | `getState` attempts to return the current board if there are changes. If the
--   player already has the latest changes then HTTP304 is returned. It does this
--   by comparing the time from the `Last-Received-Update` QueryParam to the date
--   of the last change. If this query parameter is missing then the state is 
--   always returned
getState :: TVar GameStates -> Int -> Int -> Maybe UTCTime -> Handler Board
getState s g p d = do
    gs <- IM.lookup g <$> liftIO (readTVarIO s)

    -- If there is no board then return 404, otherwise determine response
    flip (maybe $ throwError err404) gs $ \(b, mod, _) -> 
        case d of 
            Nothing -> pure $ sanitiseBoard b p
            Just date
                | date < mod -> pure $ sanitiseBoard b p
                | otherwise  -> throwError err304
