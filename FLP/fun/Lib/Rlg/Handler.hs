-- Author: Daniel Klimaj; xklima22@stud.fit.vutbr.cz

module Lib.Rlg.Handler where

import Lib.CmdArgs.Config

import Lib.Rlg.Rlg
import Lib.Rlg.Parser
import Lib.Rlg.Validator
import Lib.Rlg.Transform

-- Gets readable content of Either String Rlg
getStr :: Either String Rlg -> String
getStr (Left msg)  = msg
getStr (Right rlg) = show rlg

handleRlg :: Config -> String -> Either String Rlg
handleRlg (Config inner transform nfsm _) input
  | inner     = handleInnerRlg input
  | transform = handleTransformRlg input
  | nfsm      = Left "NFSM not implemented yet"
  | otherwise = Left "Invalid config" -- Should not happen

handleInnerRlg :: String -> Either String Rlg
handleInnerRlg input = do
  inRlg <- parseRlg input
  validateRlg inRlg

handleTransformRlg :: String -> Either String Rlg
handleTransformRlg input = do
  inRlg <- parseRlg input
  validRlg <- validateRlg inRlg
  transformRlg validRlg
