-- Author: Daniel Klimaj; xklima22@stud.fit.vutbr.cz

module Lib.Rlg.Transform.Helpers where

import Lib.Rlg.Rlg
import Lib.Misc.Misc

chkRuleP :: Rule -> Bool
chkRuleP r
  | (length (right r) == 2) && (isTermS ((right r)!!0)) &&
    (isNtermS ((right r)!!1)) = True
  | (length (right r) == 1) && ((right r)!!0 == "#") = True
  | otherwise = False

chkRuleAN :: Rule -> Bool
chkRuleAN r
  | length (right r) > 1 && isNtermS ((right r)!!(length (right r)-1)) = True
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
addNterms :: Rlg -> Rlg
addNterms rlg = rlg { nonterminals =
  listMerge (nonterminals rlg) (getIndexedNterms (rules rlg)) }

getIndexedNterms :: [Rule] -> [Symbol]
getIndexedNterms [] = []
getIndexedNterms (r:rs)
  | isIndexed (left r) = [left r] ++ getIndexedNterms rs
  | otherwise          = [] ++ getIndexedNterms rs
