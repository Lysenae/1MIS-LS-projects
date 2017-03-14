-- Author: Daniel Klimaj; xklima22@stud.fit.vutbr.cz

{-# LANGUAGE RecordWildCards #-}

module Main
    (main)
  where

import Control.Applicative
import System.Environment
import System.IO

import Lib.Utils.Utils

import Lib.CmdArgs.Parser
import Lib.CmdArgs.Config

import Lib.Rlg.Rlg
import Lib.Rlg.Handler

main :: IO ()
main = do
  c <- parseArguments <$> getArgs
  either print handleCmdArgs c

handleCmdArgs :: Config -> IO ()
handleCmdArgs conf = do
  input <- getInput (inrlg conf)
  print (handleRlg conf input)

getInput :: FilePath -> IO String
getInput inpath
  | emptyStr inpath = getContents
  | otherwise       = readFile inpath
