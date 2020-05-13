-------------------------------------------------------------------------------
-- Perpetual Haskelling Initiative                                           --
-------------------------------------------------------------------------------
-- This source code is licensed under the MIT licence found in the           --
-- LICENSE file in the root directory of this source tree.                   --
-------------------------------------------------------------------------------

module Purestone.Board ( Board ) where

-------------------------------------------------------------------------------

import Purestone.Player
import GHC.Generics

-------------------------------------------------------------------------------

-- | Represents the board during a match
data Board
  = Board {
    -- | The first player in a match
    boardPlayer1 :: Player,
    -- | The second player in a match
    boardPlayer2 :: Player
  } deriving (Eq, Show, Generic)


-------------------------------------------------------------------------------
