-- Author: Daniel Klimaj; xklima22@stud.fit.vutbr.cz

module Lib.Rlg.ToString where

import Lib.Rlg.Rlg

-- Gets readable content of Either String Rlg
getStr :: Either String Rlg -> String
getStr (Left msg)  = msg
getStr (Right rlg) = show rlg

-- Converts RLG to String
rlg2str :: Either String Rlg -> String
rlg2str (Left msg)  = msg
rlg2str (Right rlg) =
  cvtSymbols (nonterminals rlg) ++ "\n" ++
  cvtSymbols (terminals rlg)  ++ "\n" ++
  start rlg ++ "\n" ++
  cvtRules (rules rlg)

-- Rlg {
--  nonterminals = ["A","B"],
--  terminals = ["a","b","c"],
--  rules = [
--   Rule {left = "A", right = ["a","a","B"]},
--   Rule {left = "A", right = ["c","c","B"]},
--   Rule {left = "B", right = ["b","B"]},
--   Rule {left = "B", right = ["#"]}],
--  start = "A"
-- }

cvtSymbols :: [Symbol] -> String
cvtSymbols s = "sym"

cvtRules :: [Rule] -> String
cvtRules r = "rules"
