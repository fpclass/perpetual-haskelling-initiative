module Main (main) where

import Purestone.Server.App
import Network.Wai.Handler.Warp

main :: IO ()
main = run 3000 app