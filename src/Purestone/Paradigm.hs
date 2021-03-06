-------------------------------------------------------------------------------
-- Perpetual Haskelling Initiative                                           --
-------------------------------------------------------------------------------
-- This source code is licensed under the MIT licence found in the           --
-- LICENSE file in the root directory of this source tree.                   --
-------------------------------------------------------------------------------
module Purestone.Paradigm ( Paradigm(..) ) where 

import GHC.Generics
import Data.Aeson
-------------------------------------------------------------------------------

-- | Enumerates card paradigms.
data Paradigm 
    = Functional 
    | Imperative 
    | ObjectOriented
    deriving (Eq, Show, Read, Generic)

instance FromJSON Paradigm
instance ToJSON Paradigm
-------------------------------------------------------------------------------
