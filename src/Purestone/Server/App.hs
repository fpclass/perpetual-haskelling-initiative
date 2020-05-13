{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Purestone.Server.App (app) where

import Servant.Server
import Servant
-- import Data.Aeson

import Purestone.Board
import Purestone.Card
import Purestone.Server.GetState
import Purestone.Server.MakeMove

type PurestoneAPI =  "getState" :> Capture "gameID" Int :> Get '[JSON] Board
                :<|> "makeMove" :> Capture "gameID" Int :> ReqBody '[JSON] [Card] :> Post '[JSON] Board

server :: Server PurestoneAPI
server = getState :<|> makeMove

app :: Application
app = serve (Proxy :: Proxy PurestoneAPI) server
