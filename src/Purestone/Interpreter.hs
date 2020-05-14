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
-- drop 7 w
translate line  | (isRight p) && (isRight s) && (w == "ATTACK")    = Just (Attack (fromInteger n) )
                | (isRight p) && (isRight s) && (w == "HEAL")      = Just (Heal (fromInteger n) )
                | otherwise = Nothing 
                where   p = parse getWord "" line
                        Right w = p 
                        s = parse (integer <* eof) "" (drop ((length w)+1) line)
                        Right n = s

sc :: Parser ()
sc = L.space space1 (L.skipLineComment "//") (L.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

integer :: Parser Integer
integer = lexeme L.decimal

getWord :: Parser [Char]
getWord = (many (satisfy ( /= ' ' ) :: Parser Char ) :: Parser [Char] )
              
