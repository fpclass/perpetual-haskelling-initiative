-------------------------------------------------------------------------------
-- Perpetual Haskelling Initiative                                           --
-------------------------------------------------------------------------------
-- This source code is licensed under the MIT licence found in the           --
-- LICENSE file in the root directory of this source tree.                   --
-------------------------------------------------------------------------------
module Purestone.JSON (jsonOpts) where

import Data.Aeson
import Data.Char (isLower, isUpper, toLower)

-- These are currently only used in Purestone.Card however they could be useful for future modules 
-- and including them in Purestone.Card causes a lot of conflicts with imports (for example dropWhile
-- could be from Prelude, Data.Text or Data.List.NonEmpty)

-- | `jsonOpts` defines an option for use with genericFromJSON/genericToJSON which configures
--   the formatting of the field name
jsonOpts :: Options
jsonOpts = defaultOptions {
    fieldLabelModifier = toFieldName,
    tagSingleConstructors = True
}

-- | `toFieldName` takes a string name and strips the first word of the camel case. It is used 
--   to remove the prefix on record field names for JSON instances
toFieldName :: String -> String
toFieldName = while isUpper toLower . dropWhile isLower
    where while _ _ [] = []
          while p f (x:xs)
            | p x       = f x : while p f xs
            | otherwise = x : xs