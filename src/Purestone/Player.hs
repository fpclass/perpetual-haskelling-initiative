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

type Deck = [Card]

data Player
  = Player {
    hand :: Hand,
    deck :: Deck,
    currentPoints :: Int,
    maximumPoints :: Int,
    boardSpace :: [Card],
    deadPrograms :: [Card]
  } deriving (Eq, Show, Generic)


-------------------------------------------------------------------------------
