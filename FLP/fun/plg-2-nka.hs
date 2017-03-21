-- Project: FLP #1 - plg-2-nka
-- Author:  Daniel Klimaj; xklima22@stud.fit.vutbr.cz

module Main
    (main)
  where

import Control.Applicative
import Control.Exception
import System.Environment
import System.IO
import System.IO.Error

import Lib.Misc.Misc
import Lib.CmdArgs.Parser
import Lib.CmdArgs.Config
import Lib.RLG.RLG
import Lib.RLG.Handler

main :: IO ()
main = do
  c <- parseArguments <$> getArgs
  either putStrLn handleCmdArgs c `catch` readExHandler

handleCmdArgs :: Config -> IO ()
handleCmdArgs conf = do
  input <- getInput (infile conf)
  putStrLn (handleRlg conf input)

getInput :: FilePath -> IO String
getInput inpath
  | emptyStr inpath = getContents
  | otherwise       = readFile inpath

readExHandler :: IOError -> IO ()
readExHandler e = putStrLn ("ERROR: " ++ show e)
