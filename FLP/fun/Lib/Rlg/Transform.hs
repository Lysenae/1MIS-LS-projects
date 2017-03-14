-- Author: Daniel Klimaj; xklima22@stud.fit.vutbr.cz

{-# LANGUAGE RecordWildCards #-}

module Lib.Rlg.Transform where

import Lib.Rlg.Rlg

transformRlg :: Rlg -> Either String Rlg
transformRlg rlg = performTransformation rlg

performTransformation :: Rlg -> Either String Rlg
performTransformation rlg = Right rlg
