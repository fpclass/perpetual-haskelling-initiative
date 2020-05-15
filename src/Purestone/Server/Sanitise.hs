module Purestone.Server.Sanitise ( sanitiseBoard ) where

import Purestone.Player
import Purestone.Board

sanitisePlayer :: Player -> Player
sanitisePlayer p = p { playerHand = [], playerDeck = [] }

sanitiseBoard :: Board -> Int -> Board
sanitiseBoard b p = case p of
    1 -> b { boardPlayer2 = sanitisePlayer $ boardPlayer2 b}
    2 -> b { boardPlayer1 = sanitisePlayer $ boardPlayer1 b}
