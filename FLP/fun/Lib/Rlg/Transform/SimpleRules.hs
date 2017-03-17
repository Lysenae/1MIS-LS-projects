-- Author: Daniel Klimaj; xklima22@stud.fit.vutbr.cz

module Lib.Rlg.Transform.SimpleRules where

import Lib.Rlg.Rlg
import Lib.Rlg.Transform.Helpers

-- 1. Preserve rules of form A->xB and A->#
preservedRules' :: Rlg -> Rlg
preservedRules' rlg = rlg { rules = preservedRules (rules rlg) }

preservedRules :: [Rule] -> [Rule]
preservedRules [] = []
preservedRules (r:rs)
  | chkRuleP r = [r] ++ preservedRules rs
  | otherwise  = [] ++ preservedRules rs
