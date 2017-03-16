-- Author: Daniel Klimaj; xklima22@stud.fit.vutbr.cz

module Lib.Rlg.Handler where

import Lib.CmdArgs.Config

import Lib.Rlg.Rlg
import Lib.Rlg.Parser
import Lib.Rlg.Validator
import Lib.Rlg.Transform
import Lib.Rlg.ToString

handleRlg :: Config -> String -> String
handleRlg (Config inner transform nfsm _) input
  | inner     = getStr (handleInnerRlg input)
  | transform = rlg2str (handleTransformRlg input)
  | nfsm      = "NFSM not implemented yet"
  | otherwise = "Invalid config" -- Should not happen

handleInnerRlg :: String -> Either String Rlg
handleInnerRlg input = do
  inRlg <- parseRlg input
  validateRlg inRlg

handleTransformRlg :: String -> Either String Rlg
handleTransformRlg input = do
  inRlg <- parseRlg input
  validRlg <- validateRlg inRlg
  transformRlg validRlg
