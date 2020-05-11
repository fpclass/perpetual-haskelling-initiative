-------------------------------------------------------------------------------
-- Perpetual Haskelling Initiative                                           --
-------------------------------------------------------------------------------
-- This source code is licensed under the MIT licence found in the           --
-- LICENSE file in the root directory of this source tree.                   --
-------------------------------------------------------------------------------

module Purestone.Paradigm ( Paradigm(..) ) where 

-------------------------------------------------------------------------------

-- | Enumerates card paradigms.
data Paradigm 
    = Functional 
    | Imperative 
    | Logic 
    | ObjectOriented
    | Declarative
    | WeaklyTyped 
    | StronglyTyped 
    | DependentlyTyped
    | Interpreted
    | Compiled
    | Pure 
    | Impure
    deriving (Eq, Show)

-------------------------------------------------------------------------------
