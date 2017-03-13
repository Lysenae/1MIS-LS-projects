module Lib.CmdArgs.Parser where

import Lib.CmdArgs.Config

parseArg :: [String] -> Either String Config -> Either String Config
parseArg _ (Left msg) = (Left msg)
parseArg [] (Right (Config pi p1 p2 inf)) = Right $ Config pi p1 p2 inf
parseArg (a:as) (Right (Config pi p1 p2 inf))
  | a == "-i"     = parseArg as (Right $ Config True p1 p2 inf)
  | a == "-1"     = parseArg as (Right $ Config pi True p2 inf)
  | a == "-2"     = parseArg as (Right $ Config pi p1 True inf)
  | head a /= '-' && as == [] = Right $ Config pi p1 p2 a
  | otherwise     = Left $ "Invalid option " ++ a

parseArguments :: [String] -> Either String Config
parseArguments []   = Right $ Config False False False ""
parseArguments args = parseArg args (Right $ Config False False False "")
