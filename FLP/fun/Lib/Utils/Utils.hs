module Lib.Utils.Utils where

import Data.Char

emptyStr :: String -> Bool
emptyStr [] = True
emptyStr _  = False

splitStr :: String -> [String]
splitStr []    = []
splitStr (h:r) = [h]:(splitStr r)

isValid :: Char -> Bool
isValid c
  | (isAlpha c) == True || c == '#' = True
  | otherwise                       = False

str2Char :: String -> Char
str2Char []     = ' '
str2Char (s:rs) = s
