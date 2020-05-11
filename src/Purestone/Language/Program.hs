-------------------------------------------------------------------------------
-- Perpetual Haskelling Initiative                                           --
-------------------------------------------------------------------------------
-- This source code is licensed under the MIT licence found in the           --
-- LICENSE file in the root directory of this source tree.                   --
-------------------------------------------------------------------------------

-- | This module contains representations of 
module Purestone.Language.Program ( Program, Instr(..) ) where 

-------------------------------------------------------------------------------

-------------------------------------------------------------------------------

-- | A program is a list of instructions.
type Program = [Instr]

-- | Represents instructions that can be used to program cards with.
data Instr 
    = NoOp 
    deriving (Eq, Show)

-------------------------------------------------------------------------------
