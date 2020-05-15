-------------------------------------------------------------------------------
-- Perpetual Haskelling Initiative                                           --
-------------------------------------------------------------------------------
-- This source code is licensed under the MIT licence found in the           --
-- LICENSE file in the root directory of this source tree.                   --
-------------------------------------------------------------------------------

module Purestone.Interpreter where

-------------------------------------------------------------------------------

import Purestone.Language.Program

import Data.Text (Text)
import Data.Void
import Data.Either

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L


-------------------------------------------------------------------------------
type Error = String

type Parser = Parsec Void String


interpreter :: [String] -> Maybe Action
interpreter [] = Just []
interpreter (instr : instrs)= do 
                    x  <- translate instr
                    xs <- interpreter instrs
                    Just (x:xs)



-- Translates a line of code into an instruction
translate :: String -> Maybe Instr
translate line  | (isRight p) && (w == "ATTACK")        = getInteger Attack (length w) line
                | (isRight p) && (w == "HEAL")          = getInteger Heal (length w) line
                | (isRight p) && (w == "DRAW")          = getInteger Draw (length w) line
                | (isRight p) && (w == "DELETE")        = Just Delete
                | (isRight p) && (w == "MUTE")          = Just Mute
                | (isRight p) && (w == "RESET")         = Just Reset     
                | (isRight p) && (w == "CHOWN")         = Just Chown
                | otherwise = Nothing 
                where   p = parse getWord "" line
                        Right w = p 

-- returns an instruction with form (Instr Int)
getInteger :: (Int -> Instr) -> Int -> String -> Maybe Instr
getInteger instr n line =       if isRight s then 
                                        Just (instr (fromInteger num)) 
                                else 
                                        Nothing
                                where   s = parse (integer <* eof) "" (drop (n+1) line)
                                        Right num = s

sc :: Parser ()
sc = L.space space1 (L.skipLineComment "//") (L.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

integer :: Parser Integer
integer = lexeme L.decimal

getWord :: Parser [Char]
getWord = (many (satisfy ( /= ' ' ) :: Parser Char ) :: Parser [Char] )
              
