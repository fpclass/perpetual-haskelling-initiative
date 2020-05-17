-------------------------------------------------------------------------------
-- Perpetual Haskelling Initiative                                           --
-------------------------------------------------------------------------------
-- This source code is licensed under the MIT licence found in the           --
-- LICENSE file in the root directory of this source tree.                   --
-------------------------------------------------------------------------------

module Purestone.Random ( shuffleIO ) where 

-------------------------------------------------------------------------------

import System.Random.Shuffle (shuffle')
import System.Random (newStdGen, getStdGen)

-------------------------------------------------------------------------------

-- | `shuffleIO` takes a list of values and shuffles them using `shuffle'`
-- and the global random number generator
shuffleIO :: [a] -> IO [a]
shuffleIO xs = do
    g <- getStdGen
    -- `shuffle'` doesn't update the generator so we update it with `newStdGen`
    newStdGen
    return $ shuffle' xs (length xs) g

-------------------------------------------------------------------------------
