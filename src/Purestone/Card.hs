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
    cardProgram :: Program
} deriving (Eq, Show, Generic)

instance FromJSON Card
instance ToJSON Card

-------------------------------------------------------------------------------
