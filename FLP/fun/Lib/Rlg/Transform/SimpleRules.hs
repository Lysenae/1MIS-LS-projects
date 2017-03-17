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
trSimpleRule r all = findDerivations (left r) (getSetNX (firstRSym r) all) all

-- findDerivations symX setN[SymX] allRules
findDerivations :: Symbol -> [Symbol] -> [Rule] -> [Rule]
findDerivations _ [] _         = []
findDerivations x (s:ss) rules = findXRules x s rules ++ findDerivations x ss rules

-- find non-simple rules with specified symbol on the left side
findXRules :: Symbol -> Symbol -> [Rule] -> [Rule]
findXRules _ _ [] = []
findXRules x s (r:rs)
  | (left r) == s && chkRuleS r == False = [r {left = x}] ++ findXRules x s rs
  | otherwise = [] ++ findXRules x s rs

getSetNX :: Symbol -> [Rule] -> [Symbol]
getSetNX s [] = [s]
getSetNX s (r:rs)
  | chkRuleS r == False && (left r) == s && isNtermS (lastRSym r) =
    lastRSym r `au` getSetNX s rs
  | otherwise = [] `au` getSetNX s rs
