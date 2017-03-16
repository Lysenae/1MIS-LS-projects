-- Author: Daniel Klimaj; xklima22@stud.fit.vutbr.cz

{-# LANGUAGE RecordWildCards #-}

module Lib.Rlg.Transform where

import Lib.Rlg.Rlg

transformRlg :: Rlg -> Either String Rlg
transformRlg rlg =
  Right $ (trStart . trNterm . trRules) $ rlg

trNterm :: Rlg -> Rlg
trNterm rlg = rlg { nonterminals = ("666"):(nonterminals rlg) }

trStart :: Rlg -> Rlg
trStart rlg = rlg { start = "SS" }

trRules :: Rlg -> Rlg
trRules rlg = rlg { rules = [Rule "X" ["Y","Z","W"]] }
