-- Author: Daniel Klimaj; xklima22@stud.fit.vutbr.cz

module Lib.Rlg.Transform where

import Lib.Rlg.Rlg
import Lib.Misc.Misc

import Lib.Rlg.Transform.Rules

transformRlg :: Rlg -> Either String Rlg
transformRlg rlg = Right $ trRules rlg
