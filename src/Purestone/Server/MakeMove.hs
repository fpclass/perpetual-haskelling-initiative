-------------------------------------------------------------------------------
-- Perpetual Haskelling Initiative                                           --
-------------------------------------------------------------------------------
-- This source code is licensed under the MIT licence found in the           --
-- LICENSE file in the root directory of this source tree.                   --
-------------------------------------------------------------------------------
module Purestone.Server.MakeMove (makeMove) where

import Servant
import Data.Time.Clock
import qualified Data.IntMap as IM (lookup, insert)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar

import Purestone.Board
import Purestone.Player
import Purestone.Card
import Purestone.Hand
import Purestone.Game
import Purestone.Server.Sanitise
import Purestone.Server.GameState

-- | `subsetOf` determines if 1 set is a subset of another (taken from
--   https://mail.haskell.org/pipermail/beginners/2010-January/003350.html)
subsetOf :: (Eq a) => [a] -> [a] -> Bool
subsetOf xs ys = null $ filter (not . (`elem` ys)) xs

-- | `getMoveResponse` checks the user has the cards they want to play
--   and attempts to play those cards. HTTP400 is returned if the cards
--   were not valid
getMoveResponse :: TVar GameStates -> Board -> [Card] -> Hand -> Int -> Int -> Handler Board
getMoveResponse s b move hand g p = 
    -- Check move is not empty and the user has the cards they want to play
    if not (null move) && move `subsetOf` hand then
        -- Use `prcessMove` to attempt move. If it fails then return HTTP400
        flip (maybe $ throwError err400) (processMove b move p) $ \b' -> do
            -- Update time modified to now, then return sanitised board to user
            time <- liftIO getCurrentTime
            liftIO $ atomically $ modifyTVar s $ IM.insert g (b', time, 3-p)
            pure $ sanitiseBoard b' p
    else
        throwError err400

-- | `makeMove` takes the GameState, game ID, player and a list of cards
--   the player want to play and attempts to play those cards
makeMove :: TVar GameStates -> Int -> Int -> [Card] -> Handler Board
makeMove s g p cs = do
    -- Get GameState of game with given ID (Maybe value)
    gs <- IM.lookup g <$> liftIO (readTVarIO s)

    -- If there is no board then return 404 as the game hasn't started, otherwise
    -- determine response using `getMoveResponse`. If the wrong player is trying
    -- to play then return 403
    flip (maybe $ throwError err404) gs $ \(b@Board{..}, _, t) ->
        if t /= p then
            throwError err403
        else case p of
            1 -> getMoveResponse s b cs (playerHand boardPlayer1) g 1
            2 -> getMoveResponse s b cs (playerHand boardPlayer2) g 2
            _ -> throwError err400
