module Lib.Rlg.Validator where

import Data.Char

import Lib.Rlg.Rlg
import Lib.Utils.Utils

chkLRuleSides :: [Symbol] -> [Rule] -> Bool
chkLRuleSides _ []        = True -- Everything else checked
chkLRuleSides ntms (r:rs) = (chkLRuleSide ntms r) && (chkLRuleSides ntms rs)

chkLRuleSide :: [Symbol] -> Rule -> Bool
chkLRuleSide ntms rule = elem (left rule) ntms

-- chkRRuleSides nonterminals terminals rules
chkRRuleSides :: [Symbol] -> [Symbol] -> [Rule] -> Bool
chkRRuleSides _ _ []          = True
chkRRuleSides ntms tms (r:rs) =
  (chkRRuleSide (right r) ntms tms) && (chkRRuleSides ntms tms rs)

-- chkRRuleSide rule nonterminals terminals
chkRRuleSide :: [Symbol] -> [Symbol] -> [Symbol] -> Bool
chkRRuleSide [] _ _          = True
chkRRuleSide (r:rs) ntms tms =
  (chkSymRRule r ntms tms) && (chkRRuleSide rs ntms tms)

chkSymRRule :: Symbol -> [Symbol] -> [Symbol] -> Bool
chkSymRRule s ntms tms
  | (isLower (str2Char s)) && (elem s tms)  = True
  | (isUpper (str2Char s)) && (elem s ntms) = True
  | s == "#"                                = True
  | otherwise                               = False

validateRlg :: Either String Rlg -> Either String Rlg
validateRlg (Left msg) = Left msg
validateRlg (Right (Rlg nterm term rules start))
  | elem start nterm == False =
    Left "Start symbol is not in nonterminals"
  | chkLRuleSides nterm rules == False =
    Left "Invalid nonterminal on the left side of the rule"
  | chkRRuleSides nterm term rules == False =
    Left "Invalid symbol in the the right side of the rule"
  | otherwise = Right $ Rlg nterm term rules start
