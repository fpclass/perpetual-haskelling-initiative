module Main (main) where

import Network.Wai.Handler.Warp
import Data.IORef
import Data.Time.Clock

import Purestone.Server.App

main :: IO ()
main = do
    time <- getCurrentTime
    app <$> newIORef (Nothing, time, -1) <*> newIORef [] >>= run 3000