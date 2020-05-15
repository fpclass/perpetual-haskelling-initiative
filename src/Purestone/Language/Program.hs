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
    = Attack Int    -- Does (Int) damage to target
    | Heal Int      -- Heals (Int) damage to target
    | Draw Int      -- Draw (Int) number of cards
    | Delete        -- Removes target program from play
    | Mute          -- Remove all text from a target program
    | Reset         -- Return target program to owners hand
    | Chown         -- Gain control of a target program 
    deriving (Eq, Show, Generic)

instance FromJSON Instr where
    parseJSON = genericParseJSON jsonOpts
instance ToJSON Instr where
    toJSON = genericToJSON jsonOpts
-------------------------------------------------------------------------------
