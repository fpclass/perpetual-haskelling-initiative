{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Purestone.Server.App (app) where

import Servant.Server
import Servant
import Data.IORef

import Purestone.Board
import Purestone.Card
import Purestone.Server.GetState
import Purestone.Server.MakeMove
import Purestone.Server.GameState

type PurestoneAPI =  "getState" :> Capture "gameID" Int :> Capture "player" Int :> Get '[JSON] Board
                :<|> "makeMove" :> Capture "gameID" Int :> Capture "player" Int :> ReqBody '[JSON] [Card] :> Post '[JSON] Board

server :: IORef GameState -> Server PurestoneAPI
server s = getState s :<|> makeMove s

app :: IORef GameState -> Application
app s = serve (Proxy :: Proxy PurestoneAPI) $ server s
