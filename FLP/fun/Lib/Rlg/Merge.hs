-- Author: Daniel Klimaj; xklima22@stud.fit.vutbr.cz

module Lib.Rlg.Merge where

import Lib.Rlg.Rlg
import Lib.Misc.Misc

mergeNterm :: Rlg -> Rlg -> [Symbol]
mergeNterm r1 r2 = listMerge (nonterminals r1) (nonterminals r2)

mergeTerm :: Rlg -> Rlg -> [Symbol]
mergeTerm r1 r2 = listMerge (terminals r1) (terminals r2)

-- Latter overwrites start symbol
mergeStart :: Rlg -> Rlg -> Symbol
mergeStart r1 r2 = start r2

mergeRules :: Rlg -> Rlg -> [Rule]
mergeRules r1 r2 = listMerge (rules r1) (rules r2)

mergeRlg :: Rlg -> Rlg -> Rlg
mergeRlg r1 r2 = Rlg (mergeNterm r1 r2) (mergeTerm r1 r2) (mergeRules r1 r2)
  (mergeStart r1 r2)

mg :: Rlg -> Rlg -> Rlg
mg r1 r2 = mergeRlg r1 r2
