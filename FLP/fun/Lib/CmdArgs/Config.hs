-- Author: Daniel Klimaj; xklima22@stud.fit.vutbr.cz

module Lib.CmdArgs.Config where

data Config = Config
    { srlg  :: Bool      -- Show inner represenration of RLG
    , prlg  :: Bool      -- Show transformed RLG
    , pnka  :: Bool      -- Show equivalent NFSM
    , inrlg :: FilePath  -- File containing input RLG
    }
  deriving (Show)
