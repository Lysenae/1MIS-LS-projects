-- Author: Daniel Klimaj; xklima22@stud.fit.vutbr.cz

module Lib.Rlg.Transform.Rules where

import Lib.Rlg.Rlg
import Lib.Misc.Misc

-- 1. Preserve rules of form A->xB and A->#
preservedRules :: [Rule] -> [Rule]
preservedRules [] = []
preservedRules (r:rs)
  | checkRuleP r = [r] ++ (preservedRules rs)
  | otherwise    = [] ++ (preservedRules rs)

checkRuleP :: Rule -> Bool
checkRuleP r
  | (length (right r) == 2) && (isTermS ((right r)!!0)) &&
    (isNtermS ((right r)!!1)) = True
  | (length (right r) == 1) && ((right r)!!0 == "#") = True
  | otherwise = False

-- 2. Replace rules of form A->alphaB into form A->aA[n]
getNextIndex :: [String] -> Int
getNextIndex l
  | l == []   = 1
  | otherwise = nextIndex 0 l

nextIndex :: Int -> [String] -> Int
nextIndex i [] = i + 1
nextIndex i (l:ls)
  | idx l > i = nextIndex (idx l) ls
  | otherwise = nextIndex i ls

idx :: String -> Int
idx []     = 0
idx [s]    = 0
idx (s:ss)
  | allNum ss = read ss :: Int
  | otherwise = 0
