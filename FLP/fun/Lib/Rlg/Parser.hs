module Lib.Rlg.Parser where

import Lib.Rlg.Rlg
import Lib.Utils.Utils
import Text.ParserCombinators.ReadP
import Data.Char

newLine = char '\n'
comma   = char ','
arrow   = string "->"

parseRlg :: String -> Either String Rlg
parseRlg s = case readP_to_S rlgParser s of
   [(rlg, _)] -> Right rlg
   _ -> Left "Failed to parse RLG"

rlgParser :: ReadP Rlg
rlgParser = do
    nonterminals <- parseNonterminals
    newLine
    terminals <- parseTerminals
    newLine
    start <- parseNonterminal
    newLine
    rules <- parseRules
    eof
    return $ Rlg nonterminals terminals rules start

parseNonterminal  = many1 $ satisfy (isUpper)
parseNonterminals = sepBy1 parseNonterminal comma
parseTerminal     = many1 $ satisfy (isLower)
parseTerminals    = sepBy parseTerminal comma
parseSymbols      = many1 $ satisfy (isValid)

parseRules = many1 $ do
    t <- parseRule
    newLine
    return t
  where
    parseRule = do
        left <- parseNonterminal
        arrow
        right <- parseSymbols
        return $ Rule left (splitStr right)
