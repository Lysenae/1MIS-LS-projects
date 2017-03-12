module Main
    (main)
  where

import System.Environment
import System.IO

import Lib.CmdArgs.Parser
import Lib.CmdArgs.Data

main :: IO ()
main = do
    c <- parseArguments <$> getArgs
    input <- hGetContents stdin
    print c

