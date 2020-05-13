-------------------------------------------------------------------------------
-- Perpetual Haskelling Initiative                                           --
-------------------------------------------------------------------------------
-- This source code is licensed under the MIT licence found in the           --
-- LICENSE file in the root directory of this source tree.                   --
-------------------------------------------------------------------------------

module Purestone.Player ( Player ) where

-------------------------------------------------------------------------------

import Purestone.Hand
import Purestone.Card
import GHC.Generics

-------------------------------------------------------------------------------

-- | Type to represent the deck of a player
type Deck = [Card]

-- | Reoresents the player during a match
data Player
  = Player {
    -- | The hand of the player
    hand :: Hand,
    -- | The deck of the player
    deck :: Deck,
    -- | The current points the player has for their turn
    currentPoints :: Int,
    -- | The maximum number of points the player has
    maximumPoints :: Int,
    -- | The Programs the player has played onto the board
    boardSpace :: [Card],
    -- | The player's programs that have been killed
    deadPrograms :: [Card]
  } deriving (Eq, Show, Generic)


-------------------------------------------------------------------------------
