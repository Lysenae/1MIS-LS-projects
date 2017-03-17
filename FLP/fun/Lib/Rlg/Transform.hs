-- Author: Daniel Klimaj; xklima22@stud.fit.vutbr.cz

module Lib.Rlg.Transform where

import Lib.Rlg.Rlg
import Lib.Misc.Misc

import Lib.Rlg.Merge
import Lib.Rlg.Transform.PreservedRules
import Lib.Rlg.Transform.AlphaNRules
import Lib.Rlg.Transform.AlphaRules
import Lib.Rlg.Transform.SimpleRules

transformRlg :: Rlg -> Either String Rlg
transformRlg rlg = Right $ simpleRules' rlg ((preservedRules' rlg) `mg`
  (alphaNRules' rlg) `mg` (alphaRules' rlg))
