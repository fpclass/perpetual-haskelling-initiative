-------------------------------------------------------------------------------
-- Perpetual Haskelling Initiative                                           --
-------------------------------------------------------------------------------
-- This source code is licensed under the MIT licence found in the           --
-- LICENSE file in the root directory of this source tree.                   --
-------------------------------------------------------------------------------

module Purestone.Board ( Board(..) ) where

-------------------------------------------------------------------------------

import Purestone.Player
import Purestone.JSON
import Data.Aeson
import GHC.Generics

-------------------------------------------------------------------------------

-- | Represents the board during a match
data Board
    = Board {
        -- | The first player in a match
        boardPlayer1 :: Player,
        -- | The second player in a match
        boardPlayer2 :: Player,
        -- | The player whose turn it currently is
        boardTurn :: Int
    } deriving (Eq, Show, Generic)

instance FromJSON Board where
    parseJSON = genericParseJSON jsonOpts
instance ToJSON Board where
    toJSON = genericToJSON jsonOpts
-------------------------------------------------------------------------------
