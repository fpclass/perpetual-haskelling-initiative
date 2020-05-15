-------------------------------------------------------------------------------
-- Perpetual Haskelling Initiative                                           --
-------------------------------------------------------------------------------
-- This source code is licensed under the MIT licence found in the           --
-- LICENSE file in the root directory of this source tree.                   --
-------------------------------------------------------------------------------
module Purestone.Server.ConnectResponse ( ConnectResponse(..) ) where

import Data.Aeson
import GHC.Generics

import Purestone.JSON

-- | `ConnectResponse` is what is sent by the server upon successful joining of a game
data ConnectResponse = 
    Connected {
        -- | `connectedGameId` is the game ID of the game joined
        connectedGameId :: Int,
        -- | `connectedPlayer` is the player number (1 or 2)
        connectedPlayer :: Int
    } deriving (Generic)

instance FromJSON ConnectResponse where
    parseJSON = genericParseJSON jsonOpts
instance ToJSON ConnectResponse where
    toJSON = genericToJSON jsonOpts
