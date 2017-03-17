-- Author: Daniel Klimaj; xklima22@stud.fit.vutbr.cz

module Lib.Rlg.Transform.SimpleRules where

import Lib.Misc.Misc

import Lib.Rlg.Rlg
import Lib.Rlg.Transform.Helpers

simpleRules' :: Rlg -> Rlg -> Rlg
simpleRules' orlg trlg = trlg { rules = uniqR (simpleRules orlg trlg) }

simpleRules :: Rlg -> Rlg -> [Rule]
simpleRules orlg trlg = trSimpleRules (rules orlg) (rules trlg)

trSimpleRules :: [Rule] -> [Rule] -> [Rule]
trSimpleRules [] all = all
trSimpleRules (r:rs) all
  | chkRuleS r  = trSimpleRule r all ++ trSimpleRules rs all
  | otherwise   = [] ++ trSimpleRules rs all

trSimpleRule :: Rule -> [Rule] -> [Rule]
trSimpleRule r all = findXRules (left r) (firstRSym r) all

-- find non-simple rules with specified symbol on the left side
findXRules :: Symbol -> Symbol -> [Rule] -> [Rule]
findXRules _ _ [] = []
findXRules x s (r:rs)
  | (left r) == s && chkRuleS r == False = [r {left = x}] ++ findXRules x s rs
  | otherwise = [] ++ findXRules x s rs
