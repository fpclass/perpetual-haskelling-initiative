-------------------------------------------------------------------------------
-- Perpetual Haskelling Initiative                                           --
-------------------------------------------------------------------------------
-- This source code is licensed under the MIT licence found in the           --
-- LICENSE file in the root directory of this source tree.                   --
-------------------------------------------------------------------------------
module Main (main) where

import Network.Wai.Handler.Warp
import Data.IntMap.Strict (empty)
import Control.Concurrent.STM.TVar (newTVarIO)

import Purestone.Server.App

-- | Entry point for the server executable. Creates the 2 TVars used by the 
--   program and uses `warp` to deploy the API
main :: IO ()
main = app <$> newTVarIO empty <*> newTVarIO (1, Nothing) >>= run 3000
