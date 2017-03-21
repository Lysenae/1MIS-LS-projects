-- Project: FLP #1 - plg-2-nka
-- Author:  Daniel Klimaj; xklima22@stud.fit.vutbr.cz

module Lib.RLG.ToString where

import Lib.RLG.RLG

-- Gets readable content of Either String RLG
getStr :: Either String RLG -> String
getStr (Left msg)  = msg
getStr (Right rlg) = show rlg

-- Converts RLG to String
rlg2str :: Either String RLG -> String
rlg2str (Left msg)  = msg
rlg2str (Right rlg) =
  cvtSymbols (nonterminals rlg) "," ++ "\n" ++
  cvtSymbols (terminals rlg)  "," ++ "\n" ++
  start rlg ++ "\n" ++
  cvtRules (rules rlg)

-- cvtSymbols symbolList divider
cvtSymbols :: [Symbol] -> String -> String
cvtSymbols [] _ = ""
cvtSymbols (s:ss) d
  | ss == []  = s
  | otherwise = s ++ d ++ (cvtSymbols ss d)

cvtRules :: [Rule] -> String
cvtRules []     = ""
cvtRules (r:rs) = (rule2str r) ++ (cvtRestRules rs)

cvtRestRules :: [Rule] -> String
cvtRestRules []     = ""
cvtRestRules (r:rs) = "\n" ++ (rule2str r) ++ (cvtRestRules rs)

rule2str :: Rule -> String
rule2str r = (left r) ++ "->" ++ cvtSymbols (right r) ""
