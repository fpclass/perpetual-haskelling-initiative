module Purestone.Server.MakeMove (makeMove) where

import Servant
import Data.IORef
import Data.Time.Clock
import Control.Monad.IO.Class (liftIO)

import Purestone.Board
import Purestone.Player
import Purestone.Card
import Purestone.Hand
import Purestone.Server.GameState

-- Temporary Definition
processMove :: Board -> [Card] -> Int -> Maybe Board
processMove = undefined

-- | `subsetOf` determines if 1 set is a subset of another (taken from
--   https://mail.haskell.org/pipermail/beginners/2010-January/003350.html)
subsetOf :: (Eq a) => [a] -> [a] -> Bool
subsetOf xs ys = null $ filter (not . (`elem` ys)) xs

-- | `getMoveResponse` checks the user has the cards they want to play
--   and attempts to play those cards. HTTP400 is returned if the cards
--   were not valid
getMoveResponse :: Board -> [Card] -> Hand -> Int -> Handler Board
getMoveResponse b move hand p = 
    if not (null move) && move `subsetOf` hand then
        maybe (throwError err400) pure $ processMove b move p
    else
        throwError err400

-- | `makeMove` takes the GameState, game ID, player and a list of cards
--   the player want to play and attempts to play those cards
makeMove :: IORef GameState -> Int -> Int -> [Card] -> Handler Board
makeMove s _ p cs = do
    (b, _, t) <- liftIO $ readIORef s
    time <- liftIO getCurrentTime

    if t /= p then
        throwError err400
    else
        -- If there is no board then return 404, otherwise determine response
        flip (maybe $ throwError err404) b $ \b'@Board{..} -> 
            case p of
                1 -> do
                    response <- getMoveResponse b' cs (playerHand boardPlayer1) 1
                    liftIO $ atomicWriteIORef s (Just response, time, 2)
                    pure response
                2 -> do
                    response <- getMoveResponse b' cs (playerHand boardPlayer2) 2
                    liftIO $ atomicWriteIORef s (Just response, time, 1)
                    pure response
                _ -> throwError err400
