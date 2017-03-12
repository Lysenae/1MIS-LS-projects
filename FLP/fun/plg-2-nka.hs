module Main
    (main)
  where

import Control.Applicative
import System.Environment
import System.IO

import Lib.Utils.Utils

import Lib.CmdArgs.Parser
import Lib.CmdArgs.Data
import Lib.CmdArgs.Functions

main :: IO ()
main = do
  c <- parseArguments <$> getArgs
  either print handleCmdArgs c

handleCmdArgs :: CmdArgs -> IO ()
handleCmdArgs (CmdArgs pi p1 p2 inrlg) = do
  input <- getInput inrlg
  print input

getInput :: FilePath -> IO String
getInput inpath
  | emptyStr inpath = getContents
  | otherwise       = readFile inpath
