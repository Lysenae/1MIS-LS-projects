-- Author: Daniel Klimaj; xklima22@stud.fit.vutbr.cz

module Lib.Rlg.Transform.SimpleRules where

import Lib.Misc.Misc

import Lib.Rlg.Rlg
import Lib.Rlg.Transform.Helpers

simpleRules' :: Rlg -> Rlg
simpleRules' rlg = rlg { rules = simpleRules rlg }

simpleRules :: Rlg -> [Rule]
simpleRules rlg = trSimpleRules (rules rlg) (rules rlg)

trSimpleRules :: [Rule] -> [Rule] -> [Rule]
trSimpleRules [] _ = []
trSimpleRules (r:rs) all
  | chkRuleS r  = trSimpleRule r all ++ trSimpleRules rs all
  | otherwise   = [r] ++ trSimpleRules rs all

trSimpleRule :: Rule -> [Rule] -> [Rule]
trSimpleRule r all = [] --getSetNX (left r) all

getSetNX :: Symbol -> [Rule] -> [Symbol]
getSetNX _ [] = []
getSetNX s (r:rs)
  | chkRuleS r == False && (left r) == s && isNtermS (lastRSym r) = [lastRSym r] ++ getSetNX s rs
  | otherwise = [] ++ getSetNX s rs
