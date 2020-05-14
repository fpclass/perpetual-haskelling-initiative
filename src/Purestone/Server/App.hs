{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Purestone.Server.App (app) where

import Servant.Server
import Servant
import Data.IORef
import Data.Time.Clock

import Purestone.Board
import Purestone.Card
import Purestone.Deck
import Purestone.Server.GetState
import Purestone.Server.MakeMove
import Purestone.Server.GameState
import Purestone.Server.ConnectResponse
import Purestone.Server.Util

type PurestoneAPI =  "getState"  :> Capture "gameID" Int :> Capture "player" Int :> QueryParam "Time-Of-Last-Request" UTCTime :> Get '[JSON] Board
                :<|> "makeMove"  :> Capture "gameID" Int :> Capture "player" Int :> ReqBody '[JSON] [Card] :> Post '[JSON] Board
                :<|> "connect"   :> ReqBody '[JSON] Deck :> Post '[JSON] ConnectResponse
                :<|> "gameReady" :> Capture "gameID" Int :> Get '[JSON] Bool

server :: IORef GameState -> IORef [Deck] -> Server PurestoneAPI
server s ds = getState s :<|> makeMove s :<|> connect s ds :<|> gameReady ds

app :: IORef GameState -> IORef [Deck] -> Application
app s ds = serve (Proxy :: Proxy PurestoneAPI) $ server s ds
