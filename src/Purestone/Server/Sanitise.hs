-------------------------------------------------------------------------------
-- Perpetual Haskelling Initiative                                           --
-------------------------------------------------------------------------------
-- This source code is licensed under the MIT licence found in the           --
-- LICENSE file in the root directory of this source tree.                   --
-------------------------------------------------------------------------------
module Purestone.Server.Sanitise ( sanitiseBoard ) where

import Purestone.Player
import Purestone.Board

-- | `sanitisePlayer` takes a player and turns the player's
--   Hand and Deck into empty lists
sanitisePlayer :: Player -> Player
sanitisePlayer p = p { playerHand = [], playerDeck = [] }

-- | `sanitiseBoard` takes a board and the player number, and sanitises
--   the board so they cannot see the other players Hand/Deck
sanitiseBoard :: Board -> Int -> Board
sanitiseBoard b p = case p of
    1 -> b { boardPlayer2 = sanitisePlayer $ boardPlayer2 b}
    2 -> b { boardPlayer1 = sanitisePlayer $ boardPlayer1 b}
