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

-------------------------------------------------------------------------------

-- | 'main' is the main entry point for this application.
main :: IO ()
main = do
    -- This prevents delays in outputting prompts and other strange issues
    hSetBuffering stdout NoBuffering
    hSetBuffering stdin  NoBuffering

    putStrLn "Hello World!"

-------------------------------------------------------------------------------
