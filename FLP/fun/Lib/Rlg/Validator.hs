-- Author: Daniel Klimaj; xklima22@stud.fit.vutbr.cz

module Lib.Rlg.Validator where

import Lib.Rlg.Rlg
import Lib.Utils.Utils

chkLRuleSides :: [Symbol] -> [Rule] -> Bool
chkLRuleSides _ []        = True -- Everything else checked
chkLRuleSides ntms (r:rs) = (chkLRuleSide ntms r) && (chkLRuleSides ntms rs)

chkLRuleSide :: [Symbol] -> Rule -> Bool
chkLRuleSide ntms rule = elem (left rule) ntms

-- chkRRuleSides nonterminals terminals rules
chkRRuleSides :: [Symbol] -> [Symbol] -> [Rule] -> Bool
chkRRuleSides _ _ []     = True
chkRRuleSides n t (r:rs) = (chkRRuleSide (right r) n t) && (chkRRuleSides n t rs)

-- chkRRuleSide rule nonterminals terminals
chkRRuleSide :: [Symbol] -> [Symbol] -> [Symbol] -> Bool
chkRRuleSide [] _ _     = True
chkRRuleSide (r:rs) n t = (chkSymRRule r n t) && (chkRRuleSide rs n t)

chkSymRRule :: Symbol -> [Symbol] -> [Symbol] -> Bool
chkSymRRule s n t
  | (isTerm (s2ch s)) && (elem s t)  = True
  | (isNterm (s2ch s)) && (elem s n) = True
  | s == "#"                         = True
  | otherwise                        = False

chkRRuleSidesFormat :: [Rule] -> Bool
chkRRuleSidesFormat []     = True
chkRRuleSidesFormat (r:rs) =
  (chkRRuleSideFormat (right r) False) && (chkRRuleSidesFormat rs)

-- chkRRuleSideFormat ruleRightSide nonterminalFound
chkRRuleSideFormat :: [Symbol] -> Bool -> Bool
chkRRuleSideFormat [] _            = True
chkRRuleSideFormat (r:rs) ntmFound
  | ntmFound  = False -- Nonterminal must be the last symbol
  | otherwise = chkRRuleSideFormat rs (isNterm (s2ch r))

validateRlg :: Rlg -> Either String Rlg
validateRlg (Rlg nterm term rules start)
  | elem start nterm == False =
    Left "Start symbol is not in the nonterminals"
  | chkLRuleSides nterm rules == False =
    Left "Invalid nonterminal on the left side of the rule found"
  | chkRRuleSidesFormat rules == False =
    Left "Invalid format of the left side of the rule found"
  | chkRRuleSides nterm term rules == False =
    Left "Invalid symbol in the the right side of the rule found"
  | otherwise = Right $ Rlg nterm term rules start
