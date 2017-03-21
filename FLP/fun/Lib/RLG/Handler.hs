-- Project: FLP #1 - plg-2-nka
-- Author:  Daniel Klimaj; xklima22@stud.fit.vutbr.cz

module Lib.RLG.Handler where

import Lib.CmdArgs.Config

import Lib.RLG.RLG
import Lib.RLG.Parser
import Lib.RLG.Validator
import Lib.RLG.Transform
import Lib.RLG.ToString

handleRlg :: Config -> String -> String
handleRlg (Config inner transform nfsm _) input
  | inner     = getStr (handleInnerRlg input)
  | transform = rlg2str (handleTransformRlg input)
  | nfsm      = "NFSM not implemented yet"
  | otherwise = "Invalid config" -- Should not happen

handleInnerRlg :: String -> Either String RLG
handleInnerRlg input = do
  inRlg <- parseRlg input
  validateRlg inRlg

handleTransformRlg :: String -> Either String RLG
handleTransformRlg input = do
  inRlg <- parseRlg input
  validRlg <- validateRlg inRlg
  transformRlg validRlg
