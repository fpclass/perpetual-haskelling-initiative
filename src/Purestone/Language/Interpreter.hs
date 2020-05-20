-------------------------------------------------------------------------------
-- Perpetual Haskelling Initiative                                           --
-------------------------------------------------------------------------------
-- This source code is licensed under the MIT licence found in the           --
-- LICENSE file in the root directory of this source tree.                   --
-------------------------------------------------------------------------------

module Purestone.Language.Interpreter where

-------------------------------------------------------------------------------

import Purestone.Language.Program

import Data.Void
import Data.Either

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L


-------------------------------------------------------------------------------
type Err = String

type Parser = Parsec Void String

-- takes a list of strings where each string is a line, and returns a list of instructions or an error
interpreter :: [String] -> Either Err Action
interpreter [] = Right []
interpreter (instr : instrs)= do 
                    x  <- translate instr
                    xs <- interpreter instrs
                    Right (x:xs)



-- Translates a line of code into an instruction or an error
translate :: String -> Either Err Instr
translate line  | (isRight p) && (w == "ATTACK")        = getInteger Attack (length w) line
                | (isRight p) && (w == "HEAL")          = getInteger Heal (length w) line
                | (isRight p) && (w == "DRAW")          = getInteger Draw (length w) line
                | (isRight p) && (w == "DELETE")        = Right Delete
                | (isRight p) && (w == "MUTE")          = Right Mute
                | (isRight p) && (w == "RESET")         = Right Reset     
                | (isRight p) && (w == "CHOWN")         = Right Chown
                | otherwise = Left ("invalid input: " ++ w ++ " in " ++ line) 
                where   p = parse getWord "" line
                        Right w = p 


-- extracts the first word from a string
getWord :: Parser [Char]
getWord = (many (satisfy ( /= ' ' ) :: Parser Char ) :: Parser [Char] )

-- returns an instruction with form (Instr Int) or an error
getInteger :: (Int -> Instr) -> Int -> String -> Either Err Instr
getInteger instr n line =       if isRight s then 
                                        Right (instr (fromInteger num)) 
                                else 
                                        Left ("invalid input in line: " ++ line ++ " was expecting integer" )
                                where   s = parse (integer <* eof) "" (drop (n+1) line)
                                        Right num = s

sc :: Parser ()
sc = L.space space1 (L.skipLineComment "//") (L.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

integer :: Parser Integer
integer = lexeme L.decimal