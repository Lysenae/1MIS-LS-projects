module Lib.Rlg.Parser where

import Lib.Rlg.Rlg
import Text.ParserCombinators.ReadP

newLine = char '\n'
comma   = char ','

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
    rules <- parseRules
    newLine
    start <- parseStart
    eof
    return $ Rlg nonterminals terminals rules start


parseNonterminal = many1 $ satisfy (/= ',') $ satisfy (/= )

parseNonterminals = sepBy1 parseState comma
