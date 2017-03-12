module Main
    (main)
  where

import Control.Applicative
import System.Environment
import System.IO

import Lib.CmdArgs.Parser
import Lib.CmdArgs.Data
import Lib.CmdArgs.Functions

main :: IO ()
main = do
  c <- parseArguments <$> getArgs
  either print handleCmdArgs c

handleCmdArgs (CmdArgs pi p1 p2 inrlg) = print (CmdArgs pi p1 p2 inrlg)
