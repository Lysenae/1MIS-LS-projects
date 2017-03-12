module Lib.CmdArgs.Data where

data CmdArgs = CmdArgs
    { srlg  :: Bool      -- Show inner represenration of RLG
    , prlg  :: Bool      -- Show transformed RLG
    , pnka  :: Bool      -- Show equivalent NFSM
    , inrlg :: FilePath  -- File containing input RLG
    }
  deriving (Show)
