-- Author: Daniel Klimaj; xklima22@stud.fit.vutbr.cz

{-# LANGUAGE RecordWildCards #-}

module Lib.Rlg.Transform where

import Lib.Rlg.Rlg

transformRlg :: Rlg -> Either String Rlg
transformRlg rlg =
  Right $ Rlg (trNterm rlg) (terminals rlg) (trRules rlg) (trStart rlg)

trNterm :: Rlg -> [Symbol]
trNterm rlg = ("666"):(nonterminals rlg)

trStart :: Rlg -> Symbol
trStart rlg = "SS"

trRules :: Rlg -> [Rule]
trRules rlg = [Rule "X" ["Y","Z","W"]]
