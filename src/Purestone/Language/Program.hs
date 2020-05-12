-------------------------------------------------------------------------------
-- Perpetual Haskelling Initiative                                           --
-------------------------------------------------------------------------------
-- This source code is licensed under the MIT licence found in the           --
-- LICENSE file in the root directory of this source tree.                   --
-------------------------------------------------------------------------------
-- | This module contains representations of 
module Purestone.Language.Program ( Action, Instr(..) ) where 

-------------------------------------------------------------------------------
import GHC.Generics
import Data.Aeson
import Purestone.JSON
-------------------------------------------------------------------------------

-- | A program is a list of instructions.
type Action = [Instr]

-- | Represents instructions that can be used to program cards with.
data Instr 
    = Attack Int 
    | Heal Int
    deriving (Eq, Show, Generic)

instance FromJSON Instr where
    parseJSON = genericParseJSON jsonOpts
instance ToJSON Instr where
    toJSON = genericToJSON jsonOpts
-------------------------------------------------------------------------------
