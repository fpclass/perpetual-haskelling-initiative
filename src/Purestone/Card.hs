-------------------------------------------------------------------------------
-- Perpetual Haskelling Initiative                                           --
-------------------------------------------------------------------------------
-- This source code is licensed under the MIT licence found in the           --
-- LICENSE file in the root directory of this source tree.                   --
-------------------------------------------------------------------------------
module Purestone.Card ( Card(..) ) where 

-------------------------------------------------------------------------------

import Data.List.NonEmpty
import Data.Text
import Data.Aeson
import GHC.Generics

import Purestone.Paradigm
import Purestone.Language.Program
import Purestone.JSON

-------------------------------------------------------------------------------

-- | Represents cards in the game.
data Card = Card {
    -- | The name of the card.
    cardName :: Text,
    -- | The card's flavour text.
    cardDescription :: Text,
    -- | The list of paradigms for this card.
    cardParadigms :: NonEmpty Paradigm,
    -- | The card's program.
    cardAction :: Action,
    -- | The card's cost
    cardCost :: Int
} deriving (Eq, Show, Generic)


-- Define FromJSON and ToJSON instances for Card. genericParseJSON jsonOpts automatically 
-- derives instance using the field name with the card prefix removed as the key
instance FromJSON Card where
    parseJSON = genericParseJSON jsonOpts
instance ToJSON Card where
    toJSON = genericToJSON jsonOpts

-------------------------------------------------------------------------------
