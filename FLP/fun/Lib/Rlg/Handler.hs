-- Author: Daniel Klimaj; xklima22@stud.fit.vutbr.cz

module Lib.Rlg.Handler where

import Lib.CmdArgs.Config

import Lib.Rlg.Rlg
import Lib.Rlg.Parser
import Lib.Rlg.Validator
import Lib.Rlg.Transform

handleRlg :: Config -> String -> Either String Rlg
handleRlg conf input = do
  inRlg <- parseRlg input
  validRlg <- validateRlg inRlg
  transformRlg (trlg conf) validRlg
