-- Author: Daniel Klimaj; xklima22@stud.fit.vutbr.cz

module Lib.RLG.Transform.SimpleRules where

import Lib.Misc.Misc

import Lib.RLG.RLG
import Lib.RLG.Transform.Helpers

simpleRules' :: RLG -> RLG -> RLG
simpleRules' orlg trlg =
  trlg { rules = uniqR (simpleRules (copySimpleRules orlg trlg)) }

simpleRules ::[Rule] -> [Rule]
simpleRules r = trSimpleRules r r

trSimpleRules :: [Rule] -> [Rule] -> [Rule]
trSimpleRules [] _ = []
trSimpleRules (r:rs) all
  | chkRuleS r = trSimpleRule r all ++ trSimpleRules rs all
  | otherwise  = [r] ++ trSimpleRules rs all

trSimpleRule :: Rule -> [Rule] -> [Rule]
trSimpleRule r all = findDerivations (left r) (getSetNX (firstRSym r) all) all

-- find non-simple rules with specified symbol on the left side
findXRules :: Symbol -> Symbol -> [Rule] -> [Rule]
findXRules _ _ [] = []
findXRules x s (r:rs)
  | (left r) == s && chkRuleS r == False = [r {left = x}] ++ findXRules x s rs
  | otherwise = findXRules x s rs

findDerivations :: Symbol -> [Symbol] -> [Rule] -> [Rule]
findDerivations _ [] _         = []
findDerivations x (s:ss) rules =
  findXRules x s rules ++ findDerivations x ss rules

getSetNX :: Symbol -> [Rule] -> [Symbol]
getSetNX s r = getSetNX' s r r

getSetNX' :: Symbol -> [Rule] -> [Rule] -> [Symbol]
getSetNX' s [] _ = [s]
getSetNX' s (r:rs) all
  | chkRuleS r == True && (left r) == s && isNtermS (firstRSym r) =
    getSetNX' (firstRSym r) all all `lm` getSetNX' s rs all
  | otherwise = [] `au` getSetNX' s rs all
