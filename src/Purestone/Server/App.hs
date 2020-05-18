-------------------------------------------------------------------------------
-- Perpetual Haskelling Initiative                                           --
-------------------------------------------------------------------------------
-- This source code is licensed under the MIT licence found in the           --
-- LICENSE file in the root directory of this source tree.                   --
-------------------------------------------------------------------------------
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Purestone.Server.App (app) where

import Servant.Server
import Servant
import Data.Time.Clock
import Control.Concurrent.STM.TVar

import Purestone.Board
import Purestone.Card
import Purestone.Deck
import Purestone.Server.GetState
import Purestone.Server.MakeMove
import Purestone.Server.GameState
import Purestone.Server.ConnectResponse
import Purestone.Server.Util

-- | `PurestoneAPI` defines the API for use by servant
type PurestoneAPI =  "state"     :> Capture "gameID" Int :> Capture "player" Int :> QueryParam "update" UTCTime :> Get '[JSON] Board
                :<|> "state"     :> Capture "gameID" Int :> Capture "player" Int :> ReqBody '[JSON] [Card] :> Patch '[JSON] Board
                :<|> "connect"   :> ReqBody '[JSON] Deck :> Post '[JSON] ConnectResponse
                :<|> "gameReady" :> Capture "gameID" Int :> Get '[JSON] Bool

-- | `server` defines which functions to use to serve the endpoints
server :: TVar GameStates -> TVar (Int, Maybe Deck) -> Server PurestoneAPI
server s ds = getState s :<|> makeMove s :<|> connect s ds :<|> gameReady s

-- | `app` takes the 2 TVars and creates an `Application` that warp can serve from 
--   using the `server` function
app :: TVar GameStates -> TVar (Int, Maybe Deck) -> Application
app s ds = serve (Proxy :: Proxy PurestoneAPI) $ server s ds
