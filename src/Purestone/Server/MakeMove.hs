-------------------------------------------------------------------------------
-- Perpetual Haskelling Initiative                                           --
-------------------------------------------------------------------------------
-- This source code is licensed under the MIT licence found in the           --
-- LICENSE file in the root directory of this source tree.                   --
-------------------------------------------------------------------------------
module Purestone.Server.MakeMove (makeMove) where

import Servant
import Data.IORef
import Data.Time.Clock
import Control.Monad.IO.Class (liftIO)

import Purestone.Board
import Purestone.Player
import Purestone.Card
import Purestone.Hand
import Purestone.Server.Sanitise
import Purestone.Server.GameState

-- Temporary Definition: Takes the board, the move and the player number and 
-- attempts to do that move. `Nothing` is returned if it fails
processMove :: Board -> [Card] -> Int -> Maybe Board
processMove = undefined

-- | `subsetOf` determines if 1 set is a subset of another (taken from
--   https://mail.haskell.org/pipermail/beginners/2010-January/003350.html)
subsetOf :: (Eq a) => [a] -> [a] -> Bool
subsetOf xs ys = null $ filter (not . (`elem` ys)) xs

-- | `getMoveResponse` checks the user has the cards they want to play
--   and attempts to play those cards. HTTP400 is returned if the cards
--   were not valid
getMoveResponse :: IORef GameState -> Board -> [Card] -> Hand -> Int -> Handler Board
getMoveResponse s b move hand p = 
    -- Check move is not empty and the user has the cards they want to play
    if not (null move) && move `subsetOf` hand then
        -- Use `prcessMove` to attempt move. If it fails then return HTTP400
        flip (maybe $ throwError err400) (processMove b move p) $ \b' -> do
            -- Update time modified to now, then return sanitised board to user
            time <- liftIO getCurrentTime
            liftIO $ atomicWriteIORef s (Just b', time, 3-p)
            pure $ sanitiseBoard b' p
    else
        throwError err400

-- | `makeMove` takes the GameState, game ID, player and a list of cards
--   the player want to play and attempts to play those cards
makeMove :: IORef GameState -> Int -> Int -> [Card] -> Handler Board
makeMove s _ p cs = do
    (b, _, t) <- liftIO $ readIORef s

    -- Check the right player is attempting to move
    if t /= p then
        throwError err400
    else
        -- If there is no board then return 404 as the game hasn't started, otherwise
        -- determine response using `getMoveResponse`
        flip (maybe $ throwError err404) b $ \b'@Board{..} -> 
            case p of
                1 -> getMoveResponse s b' cs (playerHand boardPlayer1) 1
                2 -> getMoveResponse s b' cs (playerHand boardPlayer2) 2
                _ -> throwError err400
