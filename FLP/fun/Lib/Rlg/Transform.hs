-- Author: Daniel Klimaj; xklima22@stud.fit.vutbr.cz

module Lib.Rlg.Transform where

import Lib.Rlg.Rlg
import Lib.Misc.Misc

transformRlg :: Rlg -> Either String Rlg
transformRlg rlg =
  Right $ (trStart . trNterm . trRules) $ rlg

trNterm :: Rlg -> Rlg
trNterm rlg = rlg { nonterminals = ("666"):(nonterminals rlg) }

trStart :: Rlg -> Rlg
trStart rlg = rlg { start = "SS" }

trRules :: Rlg -> Rlg
trRules rlg = rlg { rules = preservedRules (rules rlg) }

preservedRules :: [Rule] -> [Rule]
preservedRules [] = []
preservedRules (r:rs)
  | checkRuleP r = [r] ++ (preservedRules rs)
  | otherwise    = [] ++ (preservedRules rs)

-- Preserve rules of form A->xB and A->#
checkRuleP :: Rule -> Bool
checkRuleP r
  | (length (right r) == 2) && (isTermS ((right r)!!0)) &&
    (isNtermS ((right r)!!1)) = True
  | (length (right r) == 1) && ((right r)!!0 == "#") = True
  | otherwise = False

