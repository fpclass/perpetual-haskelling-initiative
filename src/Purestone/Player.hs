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
import Purestone.Deck
import GHC.Generics

-------------------------------------------------------------------------------

-- | Reoresents the player during a match
data Player
  = Player {
    -- | The hand of the player
    playerHand :: Hand,
    -- | The deck of the player
    playerDeck :: Deck,
    -- | The current points the player has for their turn
    playerCurrentPoints :: Int,
    -- | The maximum number of points the player has
    playerMaximumPoints :: Int,
    -- | The Programs the player has played onto the board
    playerBoardSpace :: [Card],
    -- | The player's programs that have been killed
    playerDeadPrograms :: [Card]
  } deriving (Eq, Show, Generic)


-------------------------------------------------------------------------------
