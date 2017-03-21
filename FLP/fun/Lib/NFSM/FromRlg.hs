-- Project: FLP #1 - plg-2-nka
-- Author:  Daniel Klimaj; xklima22@stud.fit.vutbr.cz

module Lib.NFSM.FromRlg where

import Data.Array

import Lib.NFSM.NFSM
import Lib.RLG.RLG
import Lib.Type.Symbol
import Lib.Type.State

fromRlg :: RLG -> NFSM
fromRlg rlg = NFSM (getStates rlg) (terminals rlg) (createTransitions rlg)
  (getStart rlg) (getEnds rlg)

getStates :: RLG -> [State]
getStates rlg = range (1, length (nonterminals rlg))
