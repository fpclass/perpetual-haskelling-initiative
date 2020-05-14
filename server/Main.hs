module Main (main) where

import Network.Wai.Handler.Warp
import Data.IORef

import Purestone.Server.App

main :: IO ()
main = run 3000 <$> app =<< newIORef (Nothing, False, False)