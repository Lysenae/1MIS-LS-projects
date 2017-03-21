-- Author: Daniel Klimaj; xklima22@stud.fit.vutbr.cz

module Lib.RLG.Transform.Helpers where

import Lib.RLG.RLG
import Lib.Misc.Misc

chkRuleP :: Rule -> Bool
chkRuleP r
  | lengthR r == 2 && isTermS (firstRSym r) && isNtermS (lastRSym r) = True
  | lengthR r == 1 && firstRSym r == "#" = True
  | otherwise = False

chkRuleAN :: Rule -> Bool
chkRuleAN r
  | lengthR r > 1 && isNtermS (lastRSym r) = True
  | otherwise = False

chkRuleA :: Rule -> Bool
chkRuleA r
  | lengthR r >= 1 && isTermS (lastRSym r) = True
  | otherwise = False

chkRuleS :: Rule -> Bool
chkRuleS r
  | length (right r) == 1 && isNtermS (firstRSym r) = True
  | otherwise = False

isIndexed :: Symbol -> Bool
isIndexed [x]    = False
isIndexed (x:xs) = allNum xs

createANterm :: Int -> Symbol
createANterm i = "A" ++ show (i)

createBNterm :: Int -> Symbol
createBNterm i = "B" ++ show (i)

getRules :: (Int, [Rule]) -> [Rule]
getRules (_, r) = r

getIndex :: (Int, [Rule]) -> Int
getIndex (i, _) = i

addT :: (Int, [Rule]) -> (Int, [Rule]) -> (Int, [Rule])
addT (i1, r1) (i2, r2)
  | i1 > i2   = (i1, r1 ++ r2)
  | otherwise = (i2, r1 ++ r2)

getIdxRuleTuple :: (Symbol, [Symbol], Int, Int, [Rule]) -> (Int, [Rule])
getIdxRuleTuple (_, _, _, i, r) = (i, r)

-- Add newly created nonterminals
addNterms :: RLG -> RLG
addNterms rlg = rlg { nonterminals =
  listMerge (nonterminals rlg) (getIndexedNterms (rules rlg)) }

getIndexedNterms :: [Rule] -> [Symbol]
getIndexedNterms [] = []
getIndexedNterms (r:rs)
  | isIndexed (left r) = [left r] ++ getIndexedNterms rs
  | otherwise          = [] ++ getIndexedNterms rs

lengthR :: Rule -> Int
lengthR r = length (right r)

lastRSym :: Rule -> Symbol
lastRSym r = ((right r)!!(length (right r)-1))

firstRSym :: Rule -> Symbol
firstRSym r = (right r)!!0

uniqR :: [Rule] -> [Rule]
uniqR r
  | r == []   = []
  | otherwise = uniqR' r []

uniqR' :: [Rule] -> [Rule] -> [Rule]
uniqR' [] r = r
uniqR' (x:xs) r
  | elem x r  = uniqR' xs r
  | otherwise = uniqR' xs (r++[x])

-- Copies all simple non-recursive rules (A->A)
copySimpleRules :: RLG -> RLG -> [Rule]
copySimpleRules sr dr = copySimpleRules' (rules sr) (rules dr)

copySimpleRules' :: [Rule] -> [Rule] -> [Rule]
copySimpleRules' [] d = d
copySimpleRules' (s:ss) d
  | chkRuleS s && (left s) /= (firstRSym s) = [s] ++ copySimpleRules' ss d
  | otherwise                               = copySimpleRules' ss d
