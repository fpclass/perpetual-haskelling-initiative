module Main (main) where

import Network.Wai.Handler.Warp
import Data.IORef

import Purestone.Server.App

main :: IO ()
main = app <$> newIORef (Nothing, False, False) <*> newIORef [] >>= run 3000