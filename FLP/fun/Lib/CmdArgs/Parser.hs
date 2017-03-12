module Lib.CmdArgs.Parser where

import Lib.CmdArgs.Data

parseArg :: [String] -> Either String CmdArgs -> Either String CmdArgs
parseArg _ (Left msg) = (Left msg)
parseArg [] (Right (CmdArgs pi p1 p2 inf)) = Right $ CmdArgs pi p1 p2 inf
parseArg (a:as) (Right (CmdArgs pi p1 p2 inf))
  | a == "-i"     = parseArg as (Right $ CmdArgs True p1 p2 inf)
  | a == "-1"     = parseArg as (Right $ CmdArgs pi True p2 inf)
  | a == "-2"     = parseArg as (Right $ CmdArgs pi p1 True inf)
  | head a /= '-' && as == [] = Right $ CmdArgs pi p1 p2 a
  | otherwise     = Left $ "Invalid option " ++ a

parseArguments :: [String] -> Either String CmdArgs
parseArguments []   = Right $ CmdArgs False False False ""
parseArguments args = parseArg args (Right $ CmdArgs False False False "")
