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

type PurestoneAPI =  "state"     :> Capture "gameID" Int :> Capture "player" Int :> QueryParam "update" UTCTime :> Get '[JSON] Board
                :<|> "state"     :> Capture "gameID" Int :> Capture "player" Int :> ReqBody '[JSON] [Card] :> Patch '[JSON] Board
                :<|> "connect"   :> ReqBody '[JSON] Deck :> Post '[JSON] ConnectResponse
                :<|> "gameReady" :> Capture "gameID" Int :> Get '[JSON] Bool

server :: IORef GameState -> IORef [Deck] -> Server PurestoneAPI
server s ds = getState s :<|> makeMove s :<|> connect s ds :<|> gameReady ds

app :: IORef GameState -> IORef [Deck] -> Application
app s ds = serve (Proxy :: Proxy PurestoneAPI) $ server s ds
