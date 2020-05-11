-------------------------------------------------------------------------------
-- Perpetual Haskelling Initiative                                           --
-------------------------------------------------------------------------------
-- This source code is licensed under the MIT licence found in the           --
-- LICENCE file in the root directory of this source tree.                   --
-------------------------------------------------------------------------------

-- | This module contains the main entry point for the Perpetual Haskelling
-- Initiative executable.
module Main ( main ) where 

import System.IO
import Data.Char (toUpper)

-- Temporary Definitions - When Card/Deck are defined in the library this will be
-- removed
type Card = String
type Deck = [Card]

loadDeck :: IO (Maybe Deck)
loadDeck = undefined

saveDeck :: Deck -> IO ()
saveDeck = undefined

createNewDeck :: IO Deck
createNewDeck = undefined

editCurrentDeck :: Deck -> IO Deck
editCurrentDeck = undefined

play :: Deck -> IO ()
play = undefined

-- | This function is used to ensure options that require a deck are not called
--   without a deck. This is better than checking a Maybe Deck is not Nothing at 
--   the start of all of these options.
requireDeck :: Maybe Deck -> (Deck -> IO ()) -> IO ()
requireDeck deck f =
    case deck of 
        Nothing -> do
            putStrLn "This option requires a deck to be selected"
            menu Nothing
        Just d -> f d

-- | This optionally takes a Deck and shows the user a menu of available options
--   then runs the relavant function. Keeps running until the user uses the quit 
--   option
menu :: Maybe Deck -> IO ()
menu deck = do
    -- Determines whether to show options that require a deck to be present
    let deckOptions Nothing = [] 
        deckOptions _       = ["\tS) Save Deck", "\tE) Edit Current Deck", "\tP) Play"]

    putStrLn "Welcome to the Perpetual Haskelling Initiative!\n"
    putStrLn "Menu:"
    putStrLn "\tL) Load Deck"
    putStrLn "\tC) Create New Deck"
    mapM_ putStrLn $ deckOptions deck
    putStrLn "\tQ) Quit"
    putStr ">>> "

    choice <- getLine

    case map toUpper choice of
        "L" -> loadDeck >>= menu
        "S" -> requireDeck deck $ \d -> do
            saveDeck d
            menu $ Just d
        "C" -> createNewDeck >>= menu . Just
        "E" -> requireDeck deck $ \d -> editCurrentDeck d >>= menu . Just
        "P" -> requireDeck deck $ \d -> do
            play d
            menu $ Just d
        "Q" -> pure ()
        _ -> do
            putStrLn "Invalid Choice"
            menu deck

-------------------------------------------------------------------------------
-- | 'main' is the main entry point for this application.
main :: IO ()
main = do
    -- This prevents delays in outputting prompts and other strange issues
    hSetBuffering stdout NoBuffering
    hSetBuffering stdin  NoBuffering

    menu Nothing
-------------------------------------------------------------------------------
