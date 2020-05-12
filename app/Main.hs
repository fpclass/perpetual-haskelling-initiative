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
import Data.Aeson
import qualified Data.Text as T
import qualified Data.Text.IO as T (putStrLn, getLine)
import qualified Data.List.NonEmpty as NE
import Data.List (nub)
import Text.Read (readMaybe)
import System.FilePath
import Control.Monad (forM, (>=>))

import Purestone.Card
import Purestone.Language.Program

-- Temporary Definitions - When relavant definitions are defined in library this
-- will be removed
type Deck = [Card]
parseProg :: [String] -> Maybe Program
parseProg = undefined

cardsInDeck :: Int
cardsInDeck = 10

-- | `loadDeck` is a computation which tries to load a deck from disk.
loadDeck :: IO (Maybe Deck)
loadDeck = do
    putStr "Enter Filename (without extension): "
    fn <- getLine
    deck <- decodeFileStrict $ fn <.> "json"

    case deck of
        Nothing -> putStrLn $ "Error Loading Deck: " ++ (fn <.> ".json")
        _       -> putStrLn "Deck loaded successfully"

    return deck  

-- | `saveDeck`is a function which tries to save a deck to disk
saveDeck :: Deck -> IO ()
saveDeck d = do
    putStr "Enter Filename (without extension): "
    fn <- getLine
    encodeFile (fn <.> "json") d

-- | `makeCard` prompts the user through creating cards. It optionally takes a
--   card which is will use as default values
makeCard :: Maybe Card -> IO Card
makeCard c = do
    name <- prompt "\nEnter Card Name" $ cardName <$> c
    desc <- prompt "\nEnter Card Description" $ cardDescription <$> c
    -- (mapM readMaybe) passes a list of strings to a list of Paradigms if possible. Since paradigms is NonEmpty not [] it must
    -- be converted to a list and then from a list again after
    paras <- fmap NE.fromList $ promptMult "\nEnter Card Paradigms:" 1 (mapM readMaybe) $ NE.toList . cardParadigms <$> c
    prog <- promptMult "\nEnter Card Program:" 0 parseProg $ cardProgram <$> c

    -- Will be replaced with instance of card when possible
    pure $ Card name desc paras prog

    where
        -- | `prompt` takes a prompt and optionally a default value and gets valid (non-empty) text
        --   input
        prompt :: String -> Maybe T.Text -> IO T.Text
        prompt s d = do
            putStr s 
            -- If default value is given then show it in brackets
            T.putStrLn $ maybe "" (\x -> " (" <> x <> "):") d
            putStr "> "
            input <- T.getLine

            if input == "" then
                -- If blank and default exists use default, otherwise run again
                maybe (putStrLn "Input Cannot Be Blank" >> prompt s d) pure d
            else
                pure input

        -- | `promptMult` takes a prompt, a minimum length, a parsing function and optionally a default
        --   value and gets a valid list of the given type
        promptMult :: (Show a) => String -> Int -> ([String] -> Maybe [a]) -> Maybe [a] -> IO [a]
        promptMult s minLines parse d = do
            putStr s
            putStrLn $ maybe "" (\x -> " (" <> x <> "):") $ show <$> d
            putStrLn "Press enter on a blank line to finish (press enter on the first line to use the default value)"
            -- Get multiple lines and remove duplicates
            input <- nub <$> getLines
            let parsedInput = parse input

            case parsedInput of
                Nothing -> putStrLn "Invalid Input" >> promptMult s minLines parse d
                Just i
                    | null i -> maybe (putStrLn "Input Cannot Be Blank" >> promptMult s minLines parse d) pure d
                    | length i < minLines -> do
                            putStrLn $ "You must enter at least " ++ show minLines ++ " lines." 
                            promptMult s minLines parse d
                    | otherwise -> pure i 
        
        -- | `getLines` gets repeated user input until they enter a blank line
        getLines :: IO [String]
        getLines = do
            putStr "> "
            x <- getLine
            if x == "" then 
                return []
            else do
                xs <- getLines
                return (x:xs)

-- | `createNewDeck` is a computation which tries to create a new deck
createNewDeck :: IO Deck
createNewDeck = forM [1..cardsInDeck] $ \i -> do
    putStrLn $ "\nCard " ++ show i ++ ":"
    makeCard Nothing

-- | `editCurrentDeck` is a function which tries to edit the given deck
editCurrentDeck :: Deck -> IO Deck
editCurrentDeck d = forM (zip [1..] d) $ \(i, c) -> do
    putStrLn $ "\nCard " ++ show i ++ ":"
    makeCard $ Just c

-- | `play` is a function which tries to play the game with the given deck
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

    putStrLn "\nMenu:"
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
        "E" -> requireDeck deck $ editCurrentDeck >=> (menu . Just)
        "P" -> requireDeck deck $ \d -> do
            play d
            menu $ Just d
        "Q" -> pure ()
        _ -> do
            putStrLn "Invalid Choice"
            menu deck

-------------------------------------------------------------------------------
-- | `main` is the main entry point for this application.
main :: IO ()
main = do
    -- This prevents delays in outputting prompts and other strange issues
    hSetBuffering stdout NoBuffering
    hSetBuffering stdin  NoBuffering

    putStrLn "Welcome to the Perpetual Haskelling Initiative!"
    menu Nothing
-------------------------------------------------------------------------------
