-- Author: Daniel Klimaj; xklima22@stud.fit.vutbr.cz

module Lib.Rlg.Transform where

import Lib.Rlg.Rlg
import Lib.Misc.Misc

import Lib.Rlg.Transform.Rules

transformRlg :: Rlg -> Either String Rlg
transformRlg rlg =
  Right $ (trStart . trNterm . trRules) $ rlg

trNterm :: Rlg -> Rlg
trNterm rlg = rlg { nonterminals = ("666"):(nonterminals rlg) }

trStart :: Rlg -> Rlg
trStart rlg = rlg { start = "SS" }

trRules :: Rlg -> Rlg
trRules rlg = rlg { rules = preservedRules (rules rlg) }

