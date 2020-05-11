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

saveDeck :: Maybe Deck -> IO ()
saveDeck = undefined

createNewDeck :: IO Deck
createNewDeck = undefined

editCurrentDeck :: Maybe Deck -> IO Deck
editCurrentDeck = undefined

play :: Maybe Deck -> IO ()
play = undefined

menu :: Maybe Deck -> IO ()
menu deck = do
    -- Determines whether to show options that require a deck to be present
    let deckOptions = if deck == Nothing then 
                          ""
                      else
                          "\tS) Save Deck\n\tE) Edit Current Deck\n\tP) Play"
    putStrLn "Welcome to the Perpetual Haskelling Initiative!\n"
    putStrLn "Menu:"
    putStrLn "\tL) Load Deck"
    putStrLn "\tC) Create New Deck"
    putStrLn $ deckOptions ++ "\tQ) Quit"
    putStr ">>> "

    choice <- getLine

    case map toUpper choice of
        "L" -> loadDeck >>= menu
        "S" -> do
            saveDeck deck
            menu deck
        "C" -> createNewDeck >>= menu . Just
        "E" -> editCurrentDeck deck >>= menu . Just
        "P" -> do
            play deck
            menu deck
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
