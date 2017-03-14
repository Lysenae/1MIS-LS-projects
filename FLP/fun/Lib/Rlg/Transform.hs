-- Author: Daniel Klimaj; xklima22@stud.fit.vutbr.cz

{-# LANGUAGE RecordWildCards #-}

module Lib.Rlg.Transform where

import Lib.Rlg.Rlg

transformRlg :: Bool -> Rlg -> Either String Rlg
transformRlg enabled rlg
  | enabled == False  = Left "Disabled"
  | otherwise         = performTransformation rlg

performTransformation :: Rlg -> Either String Rlg
performTransformation rlg = Right rlg
