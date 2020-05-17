-------------------------------------------------------------------------------
-- Perpetual Haskelling Initiative                                           --
-------------------------------------------------------------------------------
-- This source code is licensed under the MIT licence found in the           --
-- LICENSE file in the root directory of this source tree.                   --
-------------------------------------------------------------------------------
module Main (main) where

import Network.Wai.Handler.Warp
import Data.IORef
import Data.IntMap.Strict (empty)

import Purestone.Server.App

main :: IO ()
main = app <$> newTVarIO empty <*> newIORef [] >>= run 3000