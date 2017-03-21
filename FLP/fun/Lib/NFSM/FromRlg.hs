-- Project: FLP #1 - plg-2-nka
-- Author:  Daniel Klimaj; xklima22@stud.fit.vutbr.cz

module Lib.NFSM.FromRlg where

import Data.Array
import Data.List

import Lib.RLG.RLG
import Lib.NFSM.NFSM
import Lib.NFSM.Helpers
import Lib.Type.Symbol
import Lib.Type.State

fromRlg :: RLG -> Either String NFSM
fromRlg rlg = Right $ NFSM (getStates rlg) (terminals rlg) (createTransitions rlg)
  (getStart rlg) (getEnds rlg)

getStates :: RLG -> [State]
getStates rlg = range (1, length (nonterminals rlg))

createTransitions :: RLG -> [Transition]
createTransitions rlg = createTransitions' (rules rlg) (nonterminals rlg)

getStart :: RLG -> State
getStart rlg = nt2state (start rlg) (nonterminals rlg)

getEnds :: RLG -> [State]
getEnds rlg = sort $ getEnds' (rules rlg) (nonterminals rlg)

createTransitions' :: [Rule] -> [Symbol] -> [Transition]
createTransitions' [] _ = []
createTransitions' (r:rs) s
  | epsRule r = createTransitions' rs s
  | otherwise = (Transition (nt2state (left r) s) (firstRSym r)
    (nt2state (secondRSym r) s)) : createTransitions' rs s

getEnds' :: [Rule] -> [Symbol] -> [State]
getEnds' [] _ = []
getEnds' (r:rs) s
  | epsRule r = (nt2state (left r) s) : getEnds' rs s
  | otherwise = getEnds' rs s
